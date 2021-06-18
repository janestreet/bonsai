open! Core
open! Import

type t =
  { on_activate : Ui_event.t option
  ; on_deactivate : Ui_event.t option
  ; after_display : Ui_event.t option
  }

module Collection : sig
  type nonrec t = t Path.Map.t

  val has_after_display : t -> bool
  val empty : t
  val diff : t -> t -> Ui_event.t
end
