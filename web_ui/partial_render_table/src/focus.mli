open! Core
open! Bonsai_web
module Collated := Incr_map_collate.Collated

module By_row : sig
  type ('k, 'presence) t

  val focused : ('k, 'presence) t -> 'presence
  val focus_up : ('k, 'presence) t -> unit Effect.t
  val focus_down : ('k, 'presence) t -> unit Effect.t
  val page_up : ('k, 'presence) t -> unit Effect.t
  val page_down : ('k, 'presence) t -> unit Effect.t
  val unfocus : ('k, 'presence) t -> unit Effect.t

  (** [focus k] sets the focus to the row keyed by k. *)
  val focus : ('k, 'presence) t -> 'k -> unit Effect.t

  (** [focus_index n] sets the focus to the nth row from the top of the
      entire table. The first row is 0, the second is 1, and so on. *)
  val focus_index : ('k, 'presence) t -> int -> unit Effect.t

  type 'k optional = ('k, 'k option) t

  module Expert : sig
    (** [keyless] disables selecting a row by key, or returning the keyed row.
        It can be useful for unifying controls for several tables with different keys.
        It is cursed. *)
    val keyless : 'a optional -> Nothing.t optional
  end
end

module Kind : sig
  type ('a, 'presence, 'k) t =
    | None : (unit, unit, 'k) t
    | By_row :
        { on_change : ('k option -> unit Effect.t) Value.t
        ; compute_presence : 'k option Value.t -> 'presence Computation.t
        }
        -> (('k, 'presence) By_row.t, 'presence, 'k) t
end

type ('kind, 'key) t =
  { focus : 'kind
  ; visually_focused : 'key option
  }

val component
  :  ('kind, 'presence, 'key) Kind.t
  -> ('key, 'cmp) Bonsai.comparator
  -> collated:('key, 'data) Collated.t Value.t
  -> range:(int * int) Value.t
  -> scroll_to_index:(int -> unit Effect.t) Value.t
  -> ('kind, 'key) t Computation.t

val get_focused : ('r, 'presence, _) Kind.t -> 'r Value.t -> 'presence Value.t

val get_on_row_click
  :  ('r, _, 'key) Kind.t
  -> 'r Value.t
  -> ('key -> unit Effect.t) Value.t
