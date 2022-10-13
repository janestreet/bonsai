open! Core
open! Bonsai_web

type t = float Int.Map.t [@@deriving sexp, equal]

type result =
  { view : Vdom.Node.t
  ; out : t
  ; reset : unit Effect.t
  }

(** The color-list component is used for both the "before"
    and "after" columns in the UI. It generates random int => float maps, and exposes a UI
    for manipulating them, as well as returning the currently-selected map. *)
val component : string -> result Computation.t
