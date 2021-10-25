open! Core
open! Bonsai_web

(** These controls come unstyled by default. jane-web-style provides css that will make
    the control and option pills pretty. *)

val create
  :  ?extra_attrs:Vdom.Attr.t list Value.t
  -> ?placeholder:string
  -> ?on_set_change:(String.Set.t -> unit Ui_effect.t) Value.t
  -> ?split:(string -> string list)
  -> unit
  -> (String.Set.t * Vdom.Node.t * (String.Set.t -> unit Ui_effect.t)) Computation.t
