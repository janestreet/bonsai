open! Core
open! Bonsai_web

(** These controls come unstyled by default. jane-web-style provides css that will make
    the control and option pills pretty. *)

(** [create] returns a typeahead using native browser controls. *)
val create
  :  ?extra_attrs:Vdom.Attr.t list Value.t
  -> ?placeholder:string
  -> ?on_select_change:('a option -> unit Ui_effect.t) Value.t
  -> ?to_string:('a -> string) Value.t
  -> (module Bonsai.Model with type t = 'a)
  -> all_options:'a list Value.t
  -> ('a option * Vdom.Node.t * ('a option -> unit Ui_effect.t)) Computation.t

val create_multi
  :  ?extra_attrs:Vdom.Attr.t list Value.t
  -> ?placeholder:string
  -> ?on_set_change:(('a, 'cmp) Set.t -> unit Ui_effect.t) Value.t
  -> ?to_string:('a -> string)
  -> ?split:(string -> string list)
  -> ('a, 'cmp) Bonsai.comparator
  -> all_options:'a list Value.t
  -> (('a, 'cmp) Set.t * Vdom.Node.t * (('a, 'cmp) Set.t -> unit Ui_effect.t))
       Computation.t
