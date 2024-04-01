open Core
open! Import

type t =
  { label : string
  ; override_label_for_visibility : string option
  ; visible_by_default : bool
  }
[@@deriving fields ~getters]

let create ?override_label_for_visibility label ~visible_by_default =
  { label; override_label_for_visibility; visible_by_default }
;;

let create_all_visible labels =
  List.map labels ~f:(fun label ->
    { label; override_label_for_visibility = None; visible_by_default = true })
;;
