open Core
open! Import

type t =
  { label : string
  ; visible_by_default : bool
  }
[@@deriving fields ~getters]

let create_all_visible labels =
  List.map labels ~f:(fun label -> { label; visible_by_default = true })
;;
