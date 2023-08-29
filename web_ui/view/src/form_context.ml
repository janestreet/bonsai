open! Core
open Bonsai_web_ui_form_view

type t =
  { depth : int
  ; editable : editable
  }
[@@deriving fields ~getters]

let default ~editable = { depth = 0; editable }
let incr_depth t = { t with depth = t.depth + 1 }
