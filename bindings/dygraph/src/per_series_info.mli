open! Core
open! Import

type t =
  { label              : string
  ; visible_by_default : bool
  }
[@@deriving fields]

val create_all_visible : string list -> t list
