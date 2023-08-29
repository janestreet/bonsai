open! Core
open! Gen_js_api

(** http://dygraphs.com/data.html

    This API does not yet support errorBars, customBars, or fractions.  Feel free to
    add.  *)
type t

val t_to_js : t -> Ojs.t

(** [create] is the right constructor when your x-values are floats. *)
val create : float array array -> t

(** [create_option] is for when your x-values are floats, and your y-values are optional. *)
val create_option : (float * float option array) array -> t

(** [create_date] is for when your x-values are dates.

    Remember to include the following script when using the Timezone libaray:
    <script type="text/javascript" src="https://timezone-web/timezone-web/all-tz-v1.js"></script>
*)
val create_date    : (Date.t    * float array) array -> zone:Timezone.t -> t

(** [create_time_ns] is for when your x-values are times. *)
val create_time_ns : (Time_ns.t * float array) array                    -> t

(** [create_time_ns_option] is for when your x-values are times and your y-values are options. *)
val create_time_ns_option : (Time_ns.t * float option array) array -> t

(** [create_from_independent_series] and [create_from_independent_time_series] are helper
    functions for when you are creating a single graph from multiple series which do not
    necessarily have points at the same x-values. This function will produce points at the
    union of all input series' x-values.

    Note: This function will *not* interpolate or fill-forward, but rather fill in missing
    values with None (null).  See http://dygraphs.com/tests/independent-series.html

    For a similar function that does fill forward, see
    [Ts_server_protocol_kernel.Time_series_data.transpose].
*)

val create_from_independent_series      : (float     * float) array array -> t
val create_from_independent_time_series : (Time_ns.t * float) array array -> t
