open! Core
open Bonsai_web

include module type of struct
  include Bonsai_web_ui_form_view
end

val empty : t
val suggest_error : Error.t -> t -> t
val suggest_label' : Vdom.Node.t -> t -> t
val suggest_label : string -> t -> t
val set_label : Vdom.Node.t -> t -> t
val set_tooltip : Vdom.Node.t -> t -> t
val of_vdom : unique_key:string -> Vdom.Node.t -> t

val to_vdom
  :  ?theme:View.Theme.t
  -> ?on_submit:submission_options
  -> ?editable:editable
  -> t
  -> Vdom.Node.t

val to_vdom_plain : ?theme:View.Theme.t -> ?editable:editable -> t -> Vdom.Node.t list
val sexp_to_pretty_string : ('a -> Sexp.t) -> 'a -> string

module Expert : sig
  val view_error : theme:View.Theme.t -> Error.t -> Vdom.Node.t list
  val view_append_item : theme:View.Theme.t -> append_item -> Vdom.Node.t
  val view_remove_item : theme:View.Theme.t -> remove_item -> index:int -> Vdom.Node.t
end
