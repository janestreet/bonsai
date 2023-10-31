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
  | `date d -> dygraphs_date_axis_label_formatter () d gran opts |> Js.to_string
;;

(* due to the floatness of the piecewise_linear math, timestamps come can out weird.  I
   can't imagine anyone needs more precision than ms (and if they do, they can't get it
   anyways b/c of dates in javascript), so this rounding feels relatively
   uncontroversial and makes the output look a lot better. *)
let round_time_nearest_ms time ~zone =
  let date, ofday = Time_ns.to_date_ofday time ~zone in
  let span = Time_ns.Ofday.to_span_since_start_of_day ofday in
  let ms = Time_ns.Span.to_ms span |> Float.iround_nearest_exn in
  let ofday = Time_ns.Span.of_int_ms ms |> Time_ns.Ofday.of_span_since_start_of_day_exn in
  Time_ns.of_date_ofday ~zone date ofday
;;

let default_value_formatter ~zone ms_since_epoch =
  let time = Time_ns.of_span_since_epoch (Time_ns.Span.of_ms ms_since_epoch) in
  Time_ns.to_string_trimmed (round_time_nearest_ms time ~zone) ~zone
;;

type t =
  { time_to_x_value : Time_ns.t -> Time_ns.t
  ; x_value_to_time : Time_ns.t -> Time_ns.t
  ; value_formatter : float -> Options.Opts.t -> string
  ; axis_label_formatter :
      Number_or_js_date.t -> Granularity.t -> Options.Opts.t -> string
  }

let default ~zone =
  { time_to_x_value = Fn.id
  ; x_value_to_time = Fn.id
  ; value_formatter = (fun x _opts -> default_value_formatter x ~zone)
  ; axis_label_formatter = default_axis_label_formatter
  }
;;

module For_dygraph_libraries = struct
  let round_time_nearest_ms = round_time_nearest_ms
  let dygraphs_date_axis_label_formatter = dygraphs_date_axis_label_formatter
  let dygraphs_number_axis_label_formatter = dygraphs_number_axis_label_formatter
end
