open! Core
open! Import

type t =
  { on_activate : unit Ui_effect.t option
  ; on_deactivate : unit Ui_effect.t option
  ; before_display : unit Ui_effect.t option
  ; after_display : unit Ui_effect.t option
  }

module Collection : sig
  type nonrec t = t Path.Map.t

  val has_after_display : t -> bool

  (** Gets the [before_display]s that occur in [new_] but not [old]. If you want all the
      [before_display]s, use [~old:empty]. Returns [None] if there are no
      [before_display]s left to run. *)
  val get_before_display : old:t -> new_:t -> unit Ui_effect.t option

  (** Gathers the activations and deactivations that have occurred between [old] and
      [new_], as well as all the [after_display]s in [new_]. *)
  val get_after_display : old:t -> new_:t -> unit Ui_effect.t

  val empty : t
  val merge : t Incr.t -> t Incr.t -> t Incr.t
end
