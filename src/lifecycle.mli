open! Core
open! Import

type t =
  { on_activate : unit Ui_effect.t option
  ; on_deactivate : unit Ui_effect.t option
  ; after_display : unit Ui_effect.t option
  }

module Collection : sig
  type nonrec t = t Path.Map.t

  val has_after_display : t -> bool
  val empty : t
  val diff : t -> t -> unit Ui_effect.t
end
