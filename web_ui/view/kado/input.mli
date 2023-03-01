open! Core
open! Import

val textbox
  :  constants:View.Constants.t
  -> input_attr:Vdom.Attr.t
  -> container_attr:Vdom.Attr.t
  -> title:string option
  -> on_change:(string -> unit Effect.t)
  -> value:string
  -> Vdom.Node.t

val dropdown
  :  constants:View.Constants.t
  -> input_attr:Vdom.Attr.t
  -> container_attr:Vdom.Attr.t
  -> title:string option
  -> on_change:(string -> unit Effect.t)
  -> options:(string * bool * Vdom.Node.t) list
  -> Vdom.Node.t

val date
  :  constants:View.Constants.t
  -> input_attr:Vdom.Attr.t
  -> container_attr:Vdom.Attr.t
  -> title:string option
  -> on_change:(string -> unit Effect.t)
  -> value:string
  -> Vdom.Node.t

val datetime
  :  constants:View.Constants.t
  -> input_attr:Vdom.Attr.t
  -> container_attr:Vdom.Attr.t
  -> title:string option
  -> on_change:(string -> unit Effect.t)
  -> value:string
  -> Vdom.Node.t

val checkbox
  :  constants:t
  -> input_attr:Vdom.Attr.t
  -> container_attr:Vdom.Attr.t
  -> label:Vdom.Node.t
  -> on_change:(bool -> unit Effect.t)
  -> checked:bool
  -> Vdom.Node.t

(* Radiobuttons look bad; disable for now
   {[
     val radiobutton
       :  constants:t
       -> input_attr:Vdom.Attr.t
       -> container_attr:Vdom.Attr.t
       -> label:Vdom.Node.t
       -> Vdom.Node.t
   ]} *)

val button_vbox : Vdom.Node.t list -> Vdom.Node.t
