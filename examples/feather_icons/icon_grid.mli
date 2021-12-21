open! Core
open! Import

(** The main grid of all the icons *)
val component : icons:Feather_icon.t list -> controls:Controls.t -> Vdom.Node.t
