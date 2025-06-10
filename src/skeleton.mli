open! Core
open! Import
module Bonsai_value = Value
module Bonsai_computation = Computation

(** This module provides skeleton versions of [Value.t] and [Component.t] that are easier
    to traverse, and two-way serializable. *)

module Id : sig
  type t [@@deriving compare, hash, sexp_of]
end

(** Represents a ['a Value.t] *)
module Value : sig
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.t
    }

  and kind =
    | Constant
    | Exception
    | Incr
    | Named of Id.t
    | Cutoff of
        { t : t
        ; added_by_let_syntax : bool
        }
    | Mapn of { inputs : t list }
  [@@deriving sexp_of]

  val of_value : 'a Value.t -> t

  (** Computes a sexp that shows the structure of the value, it leaves any node_path's and
      positions that add an extra level of indentation making it hard to read in test
      output. *)
  val minimal_sexp_of_t : t -> Sexp.t

  (** Returns the input to the given node. *)
  val inputs : t -> t list

  (** Turns a value into a string that looks like the OCaml source code that would produce
      a value of the same structure as the provided value. *)
  val to_string_hum : t -> string
end

(** Represents a [(_, _, _, _) Computation.t] *)
module Computation : sig
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.t
    }

  and kind =
    | Return of { value : Value.t }
    | Leaf01 of { input : Value.t }
    | Leaf1 of { input : Value.t }
    | Leaf0
    | Leaf_incr of { input : Value.t }
    | Model_cutoff of { t : t }
    | Sub of
        { from : t
        ; via : Id.t
        ; into : t
        ; invert_lifecycles : bool
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
        ; cmp_id : Id.t
        ; data_id : Id.t
        ; by : t
        }
    | Assoc_on of
        { map : Value.t
        ; io_key_id : Id.t
        ; model_key_id : Id.t
        ; model_cmp_id : Id.t
        ; data_id : Id.t
        ; by : t
        }
    | Assoc_simpl of { map : Value.t }
    | Switch of
        { match_ : Value.t
        ; arms : t list
        }
    | Lazy of { t : t option }
    | Fix_define of
        { result : t
        ; initial_input : Value.t
        ; fix_id : Id.t
        ; input_id : Id.t
        }
    | Fix_recurse of
        { input : Value.t
        ; input_id : Id.t
        ; fix_id : Id.t
        }
    | Wrap of
        { model_id : Id.t
        ; inject_id : Id.t
        ; inner : t
        }
    | With_model_resetter of
        { reset_id : Id.t
        ; inner : t
        }
    | Path
    | Lifecycle of { value : Value.t }
    | Identity of { t : t }
    | Computation_watcher of
        { inner : t
        ; free_vars : Id.t list
        }
  [@@deriving sexp_of]

  val of_computation : 'result Computation.t -> t

  (** Uid.t's is different between ocaml and javascript build targets, which makes
      printing uids on expect tests tricky. You can use [sanitize_uids_for_testing] to
      make sure that all uids for a given computation are the same for a compilation
      target. *)
  val sanitize_for_testing : t -> t

  (** Computes a sexp that shows the structure of the value, it leaves any node_path's and
      positions that add an extra level of indentation making it hard to read in tests
      out. *)
  val minimal_sexp_of_t : t -> Sexp.t

  (** Returns the input values for the computation. *)
  val inputs : t -> Value.t list

  (** Returns any "children" bonsai computations that the given computation has. *)
  val children : t -> t list
end

module Traverse : sig
  (** Provides a nice way of folding through a [Computation.t], e.g.

      {[
        let count_uids =
          object
            inherit [int] fold as super

            method! id id acc =
              let acc = acc + 1 in
              super#id id acc
          end
        in
        count_uids#computation t 0
      ]} *)
  class ['acc] fold : object
    method bool : bool -> 'acc -> 'acc
    method computation : Computation.t -> 'acc -> 'acc
    method computation_kind : Computation.kind -> 'acc -> 'acc
    method id : Id.t -> 'acc -> 'acc
    method lazy_ : ('a -> 'acc -> 'acc) -> 'a lazy_t -> 'acc -> 'acc
    method list : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
    method node_path : Node_path.t -> 'acc -> 'acc
    method option : ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
    method source_code_position : Source_code_position.t -> 'acc -> 'acc
    method value : Value.t -> 'acc -> 'acc
    method value_kind : Value.kind -> 'acc -> 'acc
  end

  (** Provides a nice way of mapping over computations and its elements similarly to
      [fold]. *)
  class map : object
    method bool : bool -> bool
    method computation : Computation.t -> Computation.t
    method computation_kind : Computation.kind -> Computation.kind
    method id : Id.t -> Id.t
    method lazy_ : ('a -> 'a) -> 'a lazy_t -> 'a lazy_t
    method list : ('a -> 'a) -> 'a list -> 'a list
    method node_path : Node_path.t -> Node_path.t
    method option : ('a -> 'a) -> 'a option -> 'a option
    method source_code_position : Source_code_position.t -> Source_code_position.t
    method value : Value.t -> Value.t
    method value_kind : Value.kind -> Value.kind
  end
end

module Counts : sig
  module Computation : sig
    type t [@@deriving sexp_of]
  end

  module Value : sig
    type t [@@deriving sexp_of]
  end

  type t [@@deriving sexp_of]

  val computation : t -> Computation.t
  val value : t -> Value.t
  val get : ?pre_process:bool -> 'a Bonsai_computation.t -> t
end
