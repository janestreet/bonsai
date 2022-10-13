open! Core
open! Bonsai_web
module Collated := Incr_map_collate.Collated
module Map_list := Incr_map_collate.Map_list

module By_row : sig
  type ('k, 'presence) t =
    { focused : 'presence
    ; unfocus : unit Effect.t
    ; focus_up : unit Effect.t
    ; focus_down : unit Effect.t
    ; page_up : unit Effect.t
    ; page_down : unit Effect.t
    ; focus : 'k -> unit Effect.t
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
  -> header_height:int Value.t
  -> row_height:int
  -> range:(int * int) Value.t
  -> midpoint_of_container:int Value.t
  -> path:string Value.t
  -> ('kind, 'key) t Computation.t

val get_focused : ('r, 'presence, _) Kind.t -> 'r Value.t -> 'presence Value.t

val get_on_row_click
  :  ('r, _, 'key) Kind.t
  -> 'r Value.t
  -> ('key -> unit Effect.t) Value.t

module For_testing : sig
  module Triple : sig
    type 'k t =
      { key : 'k
      ; id : Map_list.Key.t
      ; index : int
      }
    [@@deriving sexp, equal]
  end

  module Range_response : sig
    type 'k t =
      | Yes
      | No_but_this_one_is of 'k Triple.t
      | Indeterminate
  end

  (** Attempts to find a k/v pair within the given [range] that is close to
      a row given by the triple (key, id, index).  The way that the search
      is actually performed is not a guaranteed to be stable, but is
      documented in the ml. *)
  val find_in_range
    :  range:int * int
    -> collated:('a, 'b) Collated.t
    -> key:'a
    -> id:Map_list.Key.t
    -> index:int
    -> key_equal:('a -> 'a -> bool)
    -> 'a Range_response.t
end
