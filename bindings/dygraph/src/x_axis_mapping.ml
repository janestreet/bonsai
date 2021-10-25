open Core
open Import

(** How this works:

    The general idea here is that dygraphs does not have native support for putting
    "breaks" or "gaps" in the x-axis.  It assumes the x-axis will be continuous in time
    (for time series).

    We don't want that.  We want to hide overnights and weekends.

    In order to achieve that, we generate a (piecewise-linear) mapping between "real time"
    and "graph time" where we squash the overnight and weekend portions of "real time"
    into a tiny section of "graph time".

    This mapping is invertible, meaning that from any "real time" we can produce the
    corresponding "graph time" and vice versa. For a more precise description of how we
    produce this mapping, look below in [Time_mapping].

    So, given this mapping, we hook into the dygraphs library at the following points:

    - Before handing over our time series data, we map "real time" to "graph time"

    - Before dygraphs displays a time value in the legend (via [valueFormatter]), we map
      "graph time" back to "real time"

    - Before dygraphs display x-axis tick labels (via axisLabelFormatter), we map "graph
      time" back to "real time".

    This is based on this stack overflow question
    (https://stackoverflow.com/questions/17888989/how-to-skip-weekends-on-dygraps-x-axis)
    in which the author of dygraphs (danvk) agrees this is how you need to do it. *)

let dygraphs_date_axis_label_formatter
  : unit -> Js.date Js.t -> Granularity.t -> Options.Opts.t -> Js.js_string Js.t
  =
  fun () -> Js.Unsafe.pure_js_expr {| Dygraph.dateAxisLabelFormatter |}
;;

let dygraphs_number_axis_label_formatter
  : unit -> Js.number Js.t -> Granularity.t -> Options.Opts.t -> Js.js_string Js.t
  =
  fun () -> Js.Unsafe.pure_js_expr {| Dygraph.numberAxisLabelFormatter |}
;;

let default_axis_label_formatter x gran opts =
  match x with
  | `number x ->
    let number = Js.number_of_float x in
    dygraphs_number_axis_label_formatter () number gran opts |> Js.to_string
  | `date d   -> dygraphs_date_axis_label_formatter () d gran opts |> Js.to_string
;;

(* due to the floatness of the piecewise_linear math, timestamps come can out weird.  I
   can't imagine anyone needs more precision than ms (and if they do, they can't get it
   anyways b/c of dates in javascript), so this rounding feels relatively
   uncontroversial and makes the output look a lot better. *)
let round_time_nearest_ms time ~zone =
  let date, ofday = Time_ns.to_date_ofday time ~zone                    in
  let span        = Time_ns.Ofday.to_span_since_start_of_day ofday      in
  let ms          = Time_ns.Span.to_ms span |> Float.iround_nearest_exn in
  let ofday       = Time_ns.Span.of_int_ms ms |> Time_ns.Ofday.of_span_since_start_of_day_exn in
  Time_ns.of_date_ofday ~zone date ofday
;;

let default_value_formatter ~zone ms_since_epoch =
  let time = Time_ns.of_span_since_epoch (Time_ns.Span.of_ms ms_since_epoch) in
  Time_ns.to_string_trimmed (round_time_nearest_ms time ~zone) ~zone
;;

type t =
  { time_to_x_value      : Time_ns.t -> Time_ns.t
  ; x_value_to_time      : Time_ns.t -> Time_ns.t
  ; value_formatter      : float     -> Options.Opts.t -> string
  ; axis_label_formatter :
      Number_or_js_date.t -> Granularity.t -> Options.Opts.t -> string
  }

module Time_mapping = struct
  module Time_ns = struct
    (* Make Time_ns satisfy Floatable.S and Sexpable.S, which are required in order to use
       it with Piecewise_linear. *)
    include Time_ns

    let to_float t = Span.to_ns (to_span_since_epoch t)
    let of_float f = of_span_since_epoch (Span.of_ns f)
    let t_of_sexp  = Alternate_sexp.t_of_sexp
    let sexp_of_t  = Alternate_sexp.sexp_of_t
  end

  (* Our mapping between real time and graph time will be a piecewise_linear invertible
     function *)
  include Piecewise_linear_kernel.Make_invertible (Time_ns) (Time_ns)

  let create ~start_time ~end_time ~zone ~ofday_knots ~date_to_weight : t =
    let start_date = Time_ns.to_date start_time ~zone                 in
    let end_date   = Time_ns.to_date end_time ~zone                   in
    let dates      = Date.dates_between ~min:start_date ~max:end_date in
    let time date hr min =
      Time_ns.of_date_ofday ~zone date (Time_ns.Ofday.create ~hr ~min ())
    in
    let start_time = time start_date 0 0 in
    (* These knots represent a mapping from "real time" to "x position", where we are
       representing "x positions" as a float starting at 0. *)
    let knots =
      List.fold_map ~init:0. dates ~f:(fun acc date ->
        let weight = date_to_weight date in
        let knots =
          List.map ofday_knots ~f:(fun (ofday, pct) ->
            Time_ns.of_date_ofday ~zone date ofday, acc +. (weight *. pct))
        in
        let acc = acc +. weight in
        acc, knots)
      |> snd
      |> List.concat
    in
    (* Why do we map the float knots back to time knots?  We want to give dygraphs a
       dataset that has times as the x-values.  Dygraphs has internal logic which does
       different things if your data is a time series vs. if it's a float series.  We
       want the logic to know that it's a time series, even though the particular times
       we're giving it aren't our "real" times. *)
    let knots_in_time =
      List.map knots ~f:(fun (time, x_value) ->
        let x_value_time = Time_ns.add start_time (Time_ns.Span.of_day x_value) in
        time, x_value_time)
    in
    match create knots_in_time with
    | Ok t        -> t
    | Error error ->
      Error.raise_s
        [%message
          "Failed to create a piecewise linear mapping between x axis time and real time"
            (error : Error.t)]
  ;;
end

let only_display_market_hours
      ?(mkt_start_ofday = Time_ns.Ofday.create ~hr:9 ~min:30 ())
      ?(mkt_end_ofday   = Time_ns.Ofday.create ~hr:16        ())
      ~start_time
      ~end_time
      ~zone
      ()
  =
  let ofday_knots =
    (* These knots represent how the time within a day should be spaced out on the x-axis.
       We give the time between 00:00 and 09:30 only 0.01 of the x-axis space, the time
       between 09:30 and 16:00 0.98 of the space, and the time between 16:00 and 24:00
       another 0.01. *)
    [ Time_ns.Ofday.start_of_day, 0.; mkt_start_ofday, 0.01; mkt_end_ofday, 0.99 ]
  in
  let date_to_weight date =
    (* We only give 1/100th the x-axis space to weekends as we do to weekdays *)
    if Date.is_weekend date then 0.01 else 1.
  in
  let x_axis_mapping =
    Time_mapping.create ~start_time ~end_time ~zone ~ofday_knots ~date_to_weight
  in
  let time_to_x_value time    = Time_mapping.get         x_axis_mapping time    in
  let x_value_to_time x_value = Time_mapping.get_inverse x_axis_mapping x_value in
  let value_formatter ms_since_epoch _opts =
    let x_value = Time_ns.of_span_since_epoch (Time_ns.Span.of_ms ms_since_epoch) in
    let time    = x_value_to_time x_value                                         in
    Time_ns.to_string_trimmed (round_time_nearest_ms time ~zone) ~zone
  in
  let date_axis_label_formatter x_value granularity opts =
    let time           = x_value_to_time x_value                               in
    let ms_since_epoch = Time_ns.Span.to_ms (Time_ns.to_span_since_epoch time) in
    let js_date        = new%js Js.date_fromTimeValue ms_since_epoch           in
    dygraphs_date_axis_label_formatter () js_date granularity opts |> Js.to_string
  in
  let axis_label_formatter x gran opts =
    match x with
    | `number x ->
      (* This would be surprising, given we're dealing with time series here, but there
         is a "right" thing to do, so just do it. *)
      dygraphs_number_axis_label_formatter () (Js.number_of_float x) gran opts
      |> Js.to_string
    | `date d ->
      let ms_since_epoch : float = d##getTime                        in
      let span                   = Time_ns.Span.of_ms ms_since_epoch in
      let x_value                = Time_ns.of_span_since_epoch span  in
      date_axis_label_formatter x_value gran opts
  in
  { time_to_x_value; x_value_to_time; value_formatter; axis_label_formatter }
;;

let default ~zone =
  { time_to_x_value      = Fn.id
  ; x_value_to_time      = Fn.id
  ; value_formatter      = (fun x _opts -> default_value_formatter x ~zone)
  ; axis_label_formatter = default_axis_label_formatter
  }
;;
