open! Core
open! Import

val to_vdom
  :  Underlying_intf.C.t
  -> ?on_submit:Form_view.submission_options
  -> eval_context:Form_context.t
  -> Form_view.t
  -> Vdom.Node.t

val to_vdom_plain
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> Form_view.t
  -> Vdom.Node.t list

val view_error : Error.t -> Vdom.Node.t list
val view_error_details : Underlying_intf.C.t -> Error.t -> Vdom.Node.t
val view_tooltip : Underlying_intf.C.t -> Vdom.Node.t -> Vdom.Node.t

val render_remove_item
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> Form_view.remove_item
  -> index:int
  -> Vdom.Node.t

val render_append_item
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> Form_view.append_item
  -> Vdom.Node.t

val empty
  :  eval_context:Form_context.t
  -> view_context:Form_view.context
  -> unit
  -> Vdom.Node.t list

val collapsible
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> view_context:Form_view.context
  -> Form_view.collapsible
  -> Vdom.Node.t list

val raw
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> view_context:Form_view.context
  -> Form_view.raw
  -> Vdom.Node.t list

val record
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> view_context:Form_view.context
  -> Form_view.field list
  -> Vdom.Node.t list

val variant
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> view_context:Form_view.context
  -> Form_view.variant
  -> Vdom.Node.t list

val tuple
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> view_context:Form_view.context
  -> Form_view.t list
  -> Vdom.Node.t list

val option
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> view_context:Form_view.context
  -> Form_view.option_view
  -> Vdom.Node.t list

val list
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> view_context:Form_view.context
  -> Form_view.list_view
  -> Vdom.Node.t list

val view
  :  Underlying_intf.C.t
  -> eval_context:Form_context.t
  -> Form_view.t
  -> Vdom.Node.t list

val toplevel_combine : Vdom.Node.t list -> Vdom.Node.t
