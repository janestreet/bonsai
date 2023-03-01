open! Core
open Bonsai_web_ui_form_view

type t

val default : editable:editable -> t
val incr_depth : t -> t
val depth : t -> int
val editable : t -> editable
