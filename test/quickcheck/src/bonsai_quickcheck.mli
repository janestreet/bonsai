open! Core

val clear_log : unit -> unit
val read_log : unit -> Sexp.t

module Map_comparator : sig
  type (!'cmp_a, !'cmp_b) comparator_witness
end

module Effect_func_comparator : sig
  type !'cmp comparator_witness
end

module Witness : sig
  type ('a, 'cmp) t =
    | Unit : (unit, Unit.comparator_witness) t
    | Int : (int, Int.comparator_witness) t
    | Either :
        ('a, 'cmp_a) t * ('b, 'cmp_b) t
        -> (('a, 'b) Either.t, ('cmp_a, 'cmp_b) Either.comparator_witness) t
    | Tuple :
        ('a, 'cmp_a) t * ('b, 'cmp_b) t
        -> (('a, 'b) Tuple2.t, ('cmp_a, 'cmp_b) Tuple2.comparator_witness) t
    | Map :
        ('k, 'k_cmp) t * ('v, 'v_cmp) t
        -> (('k, 'v, 'k_cmp) Map.t, ('k_cmp, 'v_cmp) Map_comparator.comparator_witness) t
    | Effect_func :
        ('a, 'cmp) t
        -> ('a -> unit Bonsai.Effect.t, 'cmp Effect_func_comparator.comparator_witness) t

  type packed = T : ('a, 'cmp) t -> packed

  val equal : ('w, 'cmp) t -> 'w -> 'w -> bool
end

module Function : sig
  type ('input, 'output) t =
    | Identity : ('input, 'input) t
    | Const : 'output -> ('input, 'output) t
    | Add_const : int -> (int, int) t
    | Snd : ('a * 'b, 'b) t
    | Map_tuple : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
    | Make_either : [ `First | `Second ] -> ('a * 'b, ('a, 'b) Either.t) t
end

module Value : sig
  type 'a t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a, 'cmp) Witness.t * ('a, 'b) Function.t -> 'b t
    | Real_value : 'a Bonsai.Value.t -> 'a t
    | Var : 'a -> 'a t
    | Both :
        { first : 'a t
        ; first_witness : ('a, _) Witness.t
        ; second : 'b t
        ; second_witness : ('b, _) Witness.t
        }
        -> ('a * 'b) t

  type packed =
    | T :
        { unpacked : 'a t
        ; witness : ('a, 'cmp) Witness.t
        }
        -> packed
end

module Computation : sig
  type 'a t =
    | Return : 'a Value.t -> 'a t
    | Subst : 'a t * ('a, 'cmp) Witness.t * ('a Value.t -> 'b t) -> 'b t
    | Subst2 :
        { tuple_computation : ('f * 's) t
        ; first_witness : ('f, _) Witness.t
        ; second_witness : ('s, _) Witness.t
        ; f : 'f Value.t -> 's Value.t -> 'r t
        }
        -> 'r t
    | Switch :
        { either_value : ('f, 's) Either.t Value.t
        ; first_witness : ('f, _) Witness.t
        ; second_witness : ('s, _) Witness.t
        ; f_first : 'f Value.t -> 'r t
        ; f_second : 's Value.t -> 'r t
        }
        -> 'r t
    | Assoc :
        { map_value : ('k, 'v, 'cmp) Map.t Value.t
        ; key_witness : ('k, 'cmp) Witness.t
        ; value_witness : ('v, _) Witness.t
        ; result_witness : ('r, _) Witness.t
        ; f : 'k Value.t -> 'v Value.t -> 'r t
        }
        -> ('k, 'r, 'cmp) Map.t t
    | State :
        { default_model : 'a
        ; default_witness : ('a, _) Witness.t
        }
        -> ('a * ('a -> unit Bonsai.Effect.t)) t

  type packed =
    | T :
        { unpacked : 'a t
        ; witness : ('a, 'cmp) Witness.t
        }
        -> packed
end

module Top_level_computation : sig
  val quickcheck_generator : Computation.packed Quickcheck.Generator.t
end

module Packed_real_computation : sig
  type t =
    | T :
        { unpacked : 'a Bonsai.Computation.t
        ; witness : ('a, 'cmp) Witness.t
        }
        -> t
end

val to_packed_real_computation : Computation.packed -> Packed_real_computation.t
val packed_computation_to_ocaml_code : ?indent:int -> Computation.packed -> string

type 'w incoming

val witness_to_result_spec
  :  ('w, 'cmp) Witness.t
  -> (module Bonsai_test.Result_spec.S with type incoming = 'w incoming and type t = 'w)

val actions_generator : ('a, 'cmp) Witness.t -> 'a incoming Quickcheck.Generator.t option
