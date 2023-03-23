open! Core
open! Bonsai_web
module Collated := Incr_map_collate.Collated

module By_row : sig
  type ('k, 'presence) t =
    { focused : 'presence
    ; unfocus : unit Effect.t
    ; focus_up : unit Effect.t
    ; focus_down : unit Effect.t
    ; page_up : unit Effect.t
    ; page_down : unit Effect.t
    ; focus : 'k -> unit Effect.t
    ; focus_index : int -> unit Effect.t
    (** [focus_index n] sets the focus to the nth row from the top of the
        entire table. The first row is 0, the second is 1, and so on. *)
    }
  [@@deriving fields]

  type 'k optional = ('k, 'k option) t
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
