open! Core
open! Bonsai_web

module Dimensions : sig
  type t =
    { width : float
    ; height : float
    }
  [@@deriving sexp, equal]
end

module Action : sig
  type 'a item =
    | Set of 'a * Dimensions.t
    | Remove of 'a

  type 'a t = 'a item list
end

module Options : sig
  type 'a maybe_stale =
    | Fresh of 'a
    | Stale of 'a
  [@@deriving sexp, equal]

  (** [Prune_stale] will remove all the items from the output map whenever the element is
      removed from the DOM, while [Keep_stale] will keep the previous values around, but mark them
      as being stale. *)
  type 'a t =
    | Prune_stale : Dimensions.t t
    | Keep_stale : Dimensions.t maybe_stale t
    | Ignore_stale : Dimensions.t t
end

(** When attached to a Vdom node, this attribute will monitor the size of this
    node, and report any changes to the size through the provided callback.

    Note: in almost all cases, you'll want to have box-sizing: border-box set
    on your node in order for it to measure the size of the element by looking
    at the border-size.  *)

val component
  :  ('key, 'cmp) Bonsai.comparator
  -> 'contained Options.t
  -> (('key, 'contained, 'cmp) Base.Map.t * ('key -> Vdom.Attr.t)) Computation.t

(** [component'] is a more general version of [component] that doesn't build the map for
    you. *)
val component'
  :  ?sexp_of_key:('key -> Sexp.t)
  -> 'contained Options.t
  -> ('key Action.t -> unit Effect.t) Value.t
  -> ('key -> Vdom.Attr.t) Computation.t

module For_testing : sig
  type t

  val type_id : t Type_equal.Id.t
  val hook_name : string
  val change_sizes : (t * Dimensions.t) list -> unit
end
