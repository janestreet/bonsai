open! Core
open! Import

module Result_spec : sig
  include module type of struct
    include Bonsai_test.Result_spec
  end

  (** [filter_printed_attributes] controls which attributes on a Node will get
      printed analyzing the string name of the attribute. Style properties
      correspond to their property name prefixed with "style.". For example, to
      filter out the "display" CSS property, you should return false for
      "style.display"; to filter out all CSS styles, return false when the
      string begins with "style.". A Node's key corresponds to the string
      "@key" *)
  val vdom
    :  ?filter_printed_attributes:(string -> bool)
    -> ('a -> Vdom.Node.t)
    -> ('a, Nothing.t) t
end

module Handle : sig
  include module type of struct
    include Bonsai_test.Handle
  end

  val click_on
    :  ?shift_key_down:bool
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val submit_form : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit

  val set_checkbox
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> checked:bool
    -> unit

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
  val mousemove : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit

  val trigger_hook
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> ('c -> unit Vdom.Effect.t) Type_equal.Id.t
    -> 'c
    -> unit

  val get_hook_value
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> 'c Type_equal.Id.t
    -> 'c

  module Bulk_size_tracker : sig
    type change =
      { selector : string
      ; width : float
      ; height : float
      }

    val change_sizes : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> change list -> unit
  end

  module Drag_and_drop : sig
    val run
      :  ('a, 'b) t
      -> get_vdom:('a -> Vdom.Node.t)
      -> name:string
      -> Bonsai_web_ui_drag_and_drop.For_testing.Action.t
      -> unit
  end


end
