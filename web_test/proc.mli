open! Core
open! Import

module Result_spec : sig
  include module type of struct
    include Bonsai_test.Result_spec
  end

  (** [filter_printed_attributes] controls which attributes on a Node will get
      printed analyzing the string name of the attribute. Styles correspond to
      the string "style" and a Node's key corresponds to the string "@key" *)
  val vdom
    :  ?filter_printed_attributes:(string -> bool)
    -> ('a -> Vdom.Node.t)
    -> ('a, Nothing.t) t
end

module Handle : sig
  include module type of struct
    include Bonsai_test.Handle
  end

  val click_on : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit
  val submit_form : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit

  val input_text
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> text:string
    -> unit

  val change
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> value:string
    -> unit

  val focus : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit
  val blur : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit

  val trigger_hook
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> ('c -> Vdom.Event.t) Type_equal.Id.t
    -> 'c
    -> unit

  module Drag_and_drop : sig
    val run
      :  ('a, 'b) t
      -> get_vdom:('a -> Vdom.Node.t)
      -> name:string
      -> Bonsai_web_ui_drag_and_drop.For_testing.Action.t
      -> unit
  end


end
