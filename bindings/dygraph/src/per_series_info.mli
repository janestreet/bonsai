open! Core
open! Import

type t =
  { label : string
  ; override_label_for_visibility : string option
      (** It may be helpful to distinguish the series label from the "label for visibility".

      For example, in some graphs we encode information about the symbol we are looking at
      in the series label.  That information changes as you look at different symbols.
      However, the second series always semantically means the same thing as you move from
      symbol to symbol.  If you uncheck the second series to disable visibility, you may
      want to remember that change even if the series label changes.

      If you want to just use the [label] as the semantic identifier for persisting
      visilibity, then just set [override_label_for_visibility] to None.  *)
  ; visible_by_default : bool
  }
[@@deriving fields ~getters]

val create
  :  ?override_label_for_visibility:string
  -> string
  -> visible_by_default:bool
  -> t

val create_all_visible : string list -> t list
