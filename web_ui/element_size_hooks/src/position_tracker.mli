open! Core
open Bonsai_web

module Position : sig
  type t =
    { top : int
    ; left : int
    ; height : int
    ; width : int
    }
  [@@deriving sexp, equal]
end

module Position_tracker : sig
  type ('key, 'cmp) t = private
    { positions : ('key, Position.t, 'cmp) Base.Map.t
    ; get_attr : 'key -> Vdom.Attr.t
    ; update : unit Effect.t
    }
end

(** Tracks a [Position.t] for many dom nodes in "bulk".

    [get_attr id] can be attached to a dom node to track its position which
    will appear as an entry inside of the [positions] map.

    Sadly, there is not a JavaScript Browser API for listening to position
    changes, so the [update] effect needs to be scheduled for the
    [positions] map to be recomputed.
*)

val component
  :  ('key, 'cmp) Bonsai.comparator
  -> ('key, 'cmp) Position_tracker.t Computation.t

module For_testing : sig
  type t

  val type_id : t Type_equal.Id.t
  val hook_name : string
  val change_positions : (t * Position.t) list -> unit
end
