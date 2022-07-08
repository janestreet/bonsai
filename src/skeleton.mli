open! Core
open! Import
module Bonsai_value = Value

(** This module provides skeleton versions of [Value.t] and [Component.t]
    that are easier to traverse, and two-way serializable. *)

module Id : sig
  type t [@@deriving hash, sexp, compare]

  val to_int : t -> int
  val of_type_id : _ Type_equal.Id.t -> int
  val of_int : int -> t
end

(** Represents a ['a Value.t] *)
module Value : sig
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.t option
    ; id : Id.t
    }

  and kind =
    | Constant
    | Lazy
    | Incr
    | Named
    | Cutoff of { t : t }
    | Mapn of { inputs : t list }
  [@@deriving sexp]

  val of_value : 'a Value.t -> t

  (** Computes a sexp that shows the structure of the value, it leaves any node_path's and positions
      that add an extra level of indentation making it hard to read in test output. *)
  val minimal_sexp_of_t : t -> Sexp.t

  (** Returns the input to the given node. *)
  val inputs : t -> t list

  (** Turns a value into a string that looks like the OCaml source code that
      would produce a value of the same structure as the provided value. *)
  val to_string_hum : t -> string
end

(** Represents a [(_, _, _, _) Computation.t] *)
module Computation : sig
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.t option
    }

  and kind =
    | Return of { value : Value.t }
    | Leaf01 of
        { input : Value.t
        ; name : string
        }
    | Leaf1 of
        { input : Value.t
        ; name : string
        }
    | Leaf0 of { name : string }
    | Leaf_incr of
        { input : Value.t
        ; name : string
        }
    | Model_cutoff of { t : t }
    | Sub of
        { from : t
        ; via : Id.t
        ; into : t
        ; statefulness : [ `Stateful | `Stateless_from | `Stateless_into ]
        }
    | Store of
        { id : Id.t
        ; value : Value.t
        ; inner : t
        }
    | Fetch of { id : Id.t }
    | Assoc of
        { map : Value.t
        ; key_id : Id.t
        ; data_id : Id.t
        ; by : t
        }
    | Assoc_on of
        { map : Value.t
        ; io_key_id : int
        ; data_id : int
        ; by : t
        }
    | Assoc_simpl of { map : Value.t }
    | Switch of
        { match_ : Value.t
        ; arms : t list
        }
    | Lazy of { t : t option }
    | Wrap of
        { model_id : Id.t
        ; inject_id : Id.t
        ; inner : t
        }
    | With_model_resetter of { t : t }
    | Path
    | Lifecycle of { value : Value.t }
  [@@deriving sexp]

  (**
     Provides a nice way of folding through a [Computation.t], e.g.

     {[
       let count_uids =
         object
           inherit [int] fold as super

           method! uid uid acc =
             let acc = acc + 1 in
             super#uid uid acc
         end
       in
       count_uids#computation t 0
     ]}
  *)
  class ['acc] fold :
    object
      method computation : t -> 'acc -> 'acc
      method computation_kind : kind -> 'acc -> 'acc
      method node_path : Node_path.t lazy_t -> 'acc -> 'acc
      method source_code_position : Lexing.position option -> 'acc -> 'acc

      method statefulness :
        [ `Stateful | `Stateless_from | `Stateless_into ] -> 'acc -> 'acc

      method string : string -> 'acc -> 'acc
      method uid : Id.t -> 'acc -> 'acc
      method value : Value.t -> 'acc -> 'acc
      method value_kind : Value.kind -> 'acc -> 'acc
    end

  (**
     Provides a nice way of mapping over computations and its elements similarly to [fold].
  *)
  class map :
    object
      method computation : t -> t
      method computation_kind : kind -> kind
      method node_path : Node_path.t lazy_t -> Node_path.t lazy_t
      method source_code_position : Lexing.position option -> Lexing.position option

      method statefulness :
        [ `Stateful | `Stateless_from | `Stateless_into ]
        -> [ `Stateful | `Stateless_from | `Stateless_into ]

      method string : string -> string
      method uid : Id.t -> Id.t
      method value : Value.t -> Value.t
      method value_kind : Value.kind -> Value.kind
    end

  val of_computation
    :  ('model, 'dynamic_action, 'static_action, 'result) Computation.t
    -> t

  (** Uid.t's is different between ocaml and javascript build targets, which makes
      printing uids on expect tests tricky. You can use [sanitize_uids_for_testing]
      to make sure that all uids for a given computation are the same for a compilation
      target. *)
  val sanitize_for_testing : t -> t

  (** Computes a sexp that shows the structure of the value, it leaves any node_path's and positions
      that add an extra level of indentation making it hard to read in tests out. *)
  val minimal_sexp_of_t : t -> Sexp.t

  (** Returns the input values for the computation. *)
  val inputs : t -> Value.t list

  (** Returns any "children" bonsai computations that the given computation has. *)
  val children : t -> t list
end
