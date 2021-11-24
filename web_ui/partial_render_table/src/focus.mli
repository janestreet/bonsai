open! Core
open! Bonsai_web
module Collated := Incr_map_collate.Collated

module By_row : sig
  type 'k t =
    { focused : 'k option
    ; unfocus : unit Ui_effect.t
    ; focus_up : unit Ui_effect.t
    ; focus_down : unit Ui_effect.t
    ; page_up : unit Ui_effect.t
    ; page_down : unit Ui_effect.t
    ; focus : 'k -> unit Ui_effect.t
    }
  [@@deriving fields]
end

module Kind : sig
  type ('a, 'k) t =
    | None : (unit, 'k) t
    | By_row : { on_change : ('k option -> unit Effect.t) Value.t } -> ('k By_row.t, 'k) t
end

val component
  :  ('kind, 'key) Kind.t
  -> ('key, 'cmp) Bonsai.comparator
  -> collated:('key, 'data) Collated.t Value.t
  -> rows_covered_by_header:int Value.t
  -> range:(int * int) Value.t
  -> remapped:('key, Int63.t * 'data, 'cmp) Map.t Value.t
  -> path:string Value.t
  -> 'kind Computation.t

val get_focused : ('r, 'key) Kind.t -> 'r Value.t -> 'key option Value.t

val get_row_click_handler
  :  ('r, 'key) Kind.t
  -> 'r Value.t
  -> ('key -> unit Ui_effect.t) Value.t option

module For_testing : sig
  module Triple : sig
    type 'k t =
      { key : 'k
      ; id : Int63.t
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
    -> id:Int63.t
    -> index:int
    -> key_equal:('a -> 'a -> bool)
    -> 'a Range_response.t
end
