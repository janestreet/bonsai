open! Core
open! Bonsai_web

(** These controls come unstyled by default. jane-web-style provides css that will make
    the control and option pills pretty. *)

(** A ['a t] represents a typeahead whose value has type ['a].

    [current_input] gives access to the current contents of the form's [<input>] element
*)
type 'a t =
  { selected : 'a
  ; set_selected : 'a -> unit Ui_effect.t
  ; current_input : string
  ; view : Vdom.Node.t
  }

(** [create] returns a typeahead using native browser controls.

    [to_option_description] if provided will render the description provided below the
    option. *)
val create
  :  ?extra_attrs:Vdom.Attr.t list Value.t
  -> ?placeholder:string
  -> ?on_select_change:('a option -> unit Ui_effect.t) Value.t
  -> ?to_string:('a -> string) Value.t
  -> ?to_option_description:('a -> string) Value.t
  -> ?handle_unknown_option:(string -> 'a option) Value.t
  -> (module Bonsai.Model with type t = 'a)
  -> equal:('a -> 'a -> bool)
  -> all_options:'a list Value.t
  -> 'a option t Computation.t

val create_multi
  :  ?extra_attrs:Vdom.Attr.t list Value.t
  -> ?placeholder:string
  -> ?on_set_change:(('a, 'cmp) Set.t -> unit Ui_effect.t) Value.t
  -> ?to_string:('a -> string) Value.t
  -> ?to_option_description:('a -> string) Value.t
  -> ?handle_unknown_option:(string -> 'a option) Value.t
  -> ?split:(string -> string list)
  -> ('a, 'cmp) Bonsai.comparator
  -> all_options:'a list Value.t
  -> ('a, 'cmp) Set.t t Computation.t

module Private : sig
  module For_testing : sig
    val create_with_browser_behavior_in_test
      :  ?extra_attrs:Vdom.Attr.t list Value.t
      -> ?placeholder:string
      -> ?on_select_change:('a option -> unit Ui_effect.t) Value.t
      -> ?to_string:('a -> string) Value.t
      -> ?to_option_description:('a -> string) Value.t
      -> ?handle_unknown_option:(string -> 'a option) Value.t
      -> (module Bonsai.Model with type t = 'a)
      -> equal:('a -> 'a -> bool)
      -> all_options:'a list Value.t
      -> 'a option t Computation.t

    val create_multi_with_browser_behavior_in_test
      :  ?extra_attrs:Vdom.Attr.t list Value.t
      -> ?placeholder:string
      -> ?on_set_change:(('a, 'cmp) Set.t -> unit Ui_effect.t) Value.t
      -> ?to_string:('a -> string) Value.t
      -> ?to_option_description:('a -> string) Value.t
      -> ?handle_unknown_option:(string -> 'a option) Value.t
      -> ?split:(string -> string list)
      -> ('a, 'cmp) Bonsai.comparator
      -> all_options:'a list Value.t
      -> ('a, 'cmp) Set.t t Computation.t
  end
end
