open! Core
open! Bonsai_web
open! Js_of_ocaml

(** When attached to a Vdom node, this attribute will monitor the size of this
    node, and report any changes to the size through the provided callback.

    Note: in almost all cases, you'll want to have box-sizing: border-box set
    on your node in order for it to measure the size of the element by looking
    at the border-size. *)
val on_change : (width:float -> height:float -> unit Ui_effect.t) -> Vdom.Attr.t

module For_testing : sig
  module Dimensions : sig
    type t =
      { width : float
      ; height : float
      }
  end

  val type_id : (Dimensions.t -> unit Vdom.Effect.t) Type_equal.Id.t
end
