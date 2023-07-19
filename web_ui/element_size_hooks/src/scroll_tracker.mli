open! Core
open! Bonsai_web
open! Js_of_ocaml

module Scrollable : sig
  type t =
    | Neither
    | Vertical
    | Horizontal
    | Both
  [@@deriving sexp, equal]
end

(** When attached to a Vdom node, this attribute will monitor and compare the scroll
    height/width and the client height/width and will make a determination about which
    direction(s) the node is scrollable in. 

    This is intended to be used on a parent/child pair by attaching to the child. If there
    is more than one child then there are a few edge cases where this won't work - notably
    on OSX with "when scrolling" scollbar setting enabled or if the parent has css property
    [scrollbar-gutter: stable]. *)
val on_change : (Scrollable.t -> unit Ui_effect.t) -> Vdom.Attr.t

module For_testing : sig
  val type_id : (Scrollable.t -> unit Vdom.Effect.t) Type_equal.Id.t
end
