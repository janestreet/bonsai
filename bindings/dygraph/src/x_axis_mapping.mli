open  Core
open! Import

(** This module is a helper function designed to make it easy (easier) to make the spacing
    of the x-axis of a dygraphs graph differ from the x-values themselves.  Currently,
    this module deals only with time x-values, but may be extended to deal with numeric
    x-values in the future.

    Dygraphs does not have native support for putting "breaks" or "gaps" in the x-axis.
    It assumes the x-axis will be continuous in time (for time series) and that the space
    on the x-axis between any two points in time should be proportional to the difference
    of those times.

    Although that's a very reasonable default, we don't always want that - for example, we
    may want to effectively hide overnights and weekends (i.e. give those very little
    space on the x-axis).

    In order to do this, you can generate a [t] which provides a mapping between "real
    time" and "x-axis time".  To use a [t] correctly, you need to hook into dygraphs in
    the following three places:

    - Before handing over our time series data, use [time_to_x_value] to map your data's
      "real time" to the times we want to use for x-values.

    - Pass [valueFormatter] to [Dygraph.Options.Axis_options.create] to map x-values
      back to "real time" before displaying values in the legend.

    - Pass [axisLabelFormatter] to [Dygraph.Options.Axis_options.create] to map x-values
      back to "real time" before displaying x-axis tick labels.

    For an example usage, see [../examples/ocaml/hide_overnights.ml}
*)

type t =
  { time_to_x_value      : Time_ns.t -> Time_ns.t
  ; x_value_to_time      : Time_ns.t -> Time_ns.t
  ; value_formatter      : float     -> Options.Opts.t -> string
  ; axis_label_formatter :
      Number_or_js_date.t -> Granularity.t -> Options.Opts.t -> string
  }

(** [only_market_hours] will squash the overnight and weekend time periods into very small
    values on the graph's x-axis. *)
val only_display_market_hours
  :  ?mkt_start_ofday:Time_ns.Ofday.t
  -> ?mkt_end_ofday:Time_ns.Ofday.t
  -> start_time:Time_ns.t
  -> end_time:Time_ns.t
  -> zone:Time_float.Zone.t
  -> unit
  -> t

val default : zone:Time_float.Zone.t -> t
