open! Core
open! Bonsai_web

module State : sig
  type 'a t

  val current : 'a t -> 'a
  val set : 'a t -> 'a -> unit Ui_effect.t

  (** The primary way to create a [State.t] is to use [tab_state] below. [create] should
      only be used if there is an external source that drives the tab select, such as a
      URL. *)
  val create : current:'a -> set:('a -> unit Ui_effect.t) -> 'a t
end

module Result : sig
  (** The result of a tabs UI component is the list of tabs, and the result
      from evaluating the current tab. *)
  type 'a t =
    { tabs : Vdom.Node.t list
    ; current : 'a
    }

  (** When the result from the tabs component is just a [Vdom.Node.t],
      there's an easy way to combine them.  This will make a Vdom node that
      looks like this:

      {v
        <div class="bonsai_ui_tab_container">
            <div class="bonsai_ui_tab_tabs"> ... tabs here ... </div>
            <div class="bonsai_ui_tab_body"> ... current tab here ... </div>
        </div>
      v}
  *)
  val combine_trivially : Vdom.Node.t t -> Vdom.Node.t
end

(** [tab_state] is used to build a basic state that can be used to store the
    state for a current tab. *)
val tab_state
  :  ?equal:('a -> 'a -> bool)
  -> (module Bonsai.Model with type t = 'a)
  -> initial:'a
  -> 'a State.t Computation.t

(** [tab_ui] takes a state and evaluates a computation-returning
    function that can match on the tab and produce a computation for that tab.

    [decorate] can be used to produce the contents for the button for each tab.

    [additional_button_attributes] can be used to add more [Vdom.Attr.t]s to the
    button nodes for each tab. *)
val tab_ui
  :  ?decorate:('a -> Vdom.Node.t) Value.t
  -> ?additional_button_attributes:(is_selected:bool -> 'a -> Vdom.Attr.t) Value.t
  -> (module Bonsai.Model with type t = 'a)
  -> all_tabs:'a list Value.t
  -> equal:('a -> 'a -> bool)
  -> 'a State.t Value.t
  -> f:
       (change_tab:('a -> unit Vdom.Effect.t) Value.t
        -> 'a Value.t
        -> 'result Computation.t)
  -> 'result Result.t Computation.t
