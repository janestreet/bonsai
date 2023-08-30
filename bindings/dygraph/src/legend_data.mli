open! Core
open! Import
open Gen_js_api

(** Legend_data is what you receive on the legendFormatter callback, as described here:
    https://github.com/danvk/dygraphs/pull/683 *)

module Series : sig
  type t =
    { dashHTML : Raw_html.t
    ; label : string
    ; labelHTML : Raw_html.t
    ; isVisible : bool
    ; isHighlighted : bool option
    ; color : string option
    ; y : float option
    ; yHTML : Raw_html.t option
    }
  [@@deriving equal, sexp]

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
end

type t =
  { x : float option
  ; xHTML : Html_or_number.t option
      (** Empirically, it seems that xHTML comes back as html (a string) for date/time-based
      x-axes and a number for number-based x-axes. *)
  ; series : Series.t list
  }
[@@deriving equal, sexp]

val t_of_js : Ojs.t -> t
val t_to_js : t -> Ojs.t
