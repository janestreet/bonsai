open Core

type 'a leaf = private Leaf
type ('from, 'into) sub = private Sub
type 'a model_resetter = private Model_resetter
type ('inner, 'outer) wrap = private Wrap
type 'inner wrap_static = private Wrap_static
type ('inner, 'outer) wrap_dynamic = private Wrap_dynamic
type switch = private Switch
type lazy_ = private Lazy
type ('bound, 'extra) bind = private Bind
type ('key, 'inner) assoc = private Assoc
type ('io_key, 'model_key, 'inner) assoc_on = private Assoc_on
type 'a id

type 'a t = private
  | Leaf_static : 'static -> 'static leaf t
  | Leaf_dynamic : 'dynamic -> 'dynamic leaf t
  | Sub_from : 'a t -> ('a, _) sub t
  | Sub_into : 'a t -> (_, 'a) sub t
  | Wrap_inner : 'a t -> ('a, _) wrap t
  | Wrap_outer : 'outer -> (_, 'outer) wrap t
  | Model_reset_inner : 'a t -> 'a model_resetter t
  | Model_reset_outer : _ model_resetter t
  | Switch :
      { branch : int
      ; action : 'a t
      ; type_id : 'a id
      }
      -> switch t
  | Lazy :
      { action : 'a t
      ; type_id : 'a id
      }
      -> lazy_ t
  | Bind :
      { action : 'a t
      ; type_id : 'a id
      ; bound : 'k
      ; bound_id : 'k Var_id.t
      ; comparator : ('k, _) Comparator.Module.t
      (** [extra] is used to hold the [gather]'d [Computation.packed_info] for the
          computation that is yielding this action. *)
      ; extra : 'extra
      ; extra_id : 'extra Var_id.t
      }
      -> ('k, 'extra) bind t
  (*_ We use [comparator] for:
      - Initializing maps where necessary
      - Compare function for stabilization tracker
      - Sexp_of function for paths + debugging/logging *)
  | Assoc :
      { key : 'key
      ; action : 'a t
      ; id : 'key Var_id.t
      ; comparator : ('key, _) Comparator.Module.t
      }
      -> ('key, 'a) assoc t
  | Assoc_on :
      { io_key : 'io_key
      ; model_key : 'model_key
      ; action : 'a t
      ; io_id : 'io_key Var_id.t
      ; io_comparator : ('io_key, _) Comparator.Module.t
      }
      -> ('io_key, 'model_key, 'a) assoc_on t

module Type_id : sig
  type 'a action := 'a t
  type 'a t := 'a id

  val nothing : Nothing.t leaf t

  val leaf
    :  'action Var_id.t
    -> action_name:string
    -> sexp_of_action:('action -> Sexp.t) option
    -> 'action leaf t

  val sub : from:'from t -> into:'into t -> ('from, 'into) sub t
  val wrap : inner:'a t -> outer:'outer Var_id.t -> ('a, 'outer) wrap t
  val model_reset : 'a t -> 'a model_resetter t
  val switch : switch t
  val lazy_ : lazy_ t
  val bind : bound_id:'a Var_id.t -> extra_id:'extra Var_id.t -> ('a, 'extra) bind t
  val assoc : key:'key Var_id.t -> action:'a t -> ('key, 'a) assoc t

  val assoc_on
    :  io_key:'io_key Var_id.t
    -> model_key:'model_key Var_id.t
    -> sexp_of_model_key:('model_key -> Sexp.t)
    -> action:'a t
    -> ('io_key, 'model_key, 'a) assoc_on t

  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val sexp_of_t : 'a t -> Sexp.t
  val to_sexp : 'a t -> 'a action -> Sexp.t
end

val static_leaf : 'static -> 'static leaf t
val dynamic_leaf : 'dynamic -> 'dynamic leaf t
val sub_from : 'a t -> ('a, _) sub t
val sub_into : 'a t -> (_, 'a) sub t
val wrap_inner : 'a t -> ('a, _) wrap t
val wrap_outer : 'outer -> (_, 'outer) wrap t
val model_reset_inner : 'a t -> 'a model_resetter t
val model_reset_outer : _ model_resetter t
val switch : branch:int -> type_id:'a id -> 'a t -> switch t
val lazy_ : type_id:'a id -> 'a t -> lazy_ t

val bind
  :  bound:'k
  -> bound_id:'k Var_id.t
  -> comparator:('k, _) Comparator.Module.t
  -> extra:'extra
  -> extra_id:'extra Var_id.t
  -> type_id:'a id
  -> 'a t
  -> ('k, 'extra) bind t

val assoc
  :  key:'key
  -> id:'key Var_id.t
  -> comparator:('key, _) Comparator.Module.t
  -> 'a t
  -> ('key, 'a) assoc t

val assoc_on
  :  io_key:'io_key
  -> io_id:'io_key Var_id.t
  -> io_comparator:('io_key, _) Comparator.Module.t
  -> model_key:'model_key
  -> 'a t
  -> ('io_key, 'model_key, 'a) assoc_on t
