open! Core
open! Import
open Gen_js_api

(** Ranges in dygraphs are represented as a number array with two elements.  This makes
    them a bit easier to work with. *)
type t =
  { low : float
  ; high : float
  }
[@@deriving sexp, equal]

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

module Spec : sig
  (** This is used when specifying a range as an input. *)
  type nonrec t =
    | Infer
    | Specified of t
  [@@deriving sexp, equal]

  val t_to_js : t -> Ojs.t
end
