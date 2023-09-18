open Core

type 'a leaf = private Leaf
type ('from, 'into) sub = private Sub
type 'a model_resetter = private Model_resetter
type ('inner, 'outer) wrap = private Wrap
type 'inner wrap_static = private Wrap_static
type ('inner, 'outer) wrap_dynamic = private Wrap_dynamic
type switch = private Switch
type lazy_ = private Lazy
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
  | Assoc :
      { key : 'key
      ; action : 'a t
      ; id : 'key Type_equal.Id.t
      ; compare : 'key -> 'key -> int
      }
      -> ('key, 'a) assoc t
  | Assoc_on :
      { io_key : 'io_key
      ; model_key : 'model_key
      ; action : 'a t
      ; io_id : 'io_key Type_equal.Id.t
      ; io_compare : 'io_key -> 'io_key -> int
      }
      -> ('io_key, 'model_key, 'a) assoc_on t

module Type_id : sig
  type 'a action := 'a t
  type 'a t := 'a id

  val nothing : Nothing.t leaf t
  val leaf : 'a Type_equal.Id.t -> 'a leaf t
  val sub : from:'from t -> into:'into t -> ('from, 'into) sub t
  val wrap : inner:'a t -> outer:'outer Type_equal.Id.t -> ('a, 'outer) wrap t
  val model_reset : 'a t -> 'a model_resetter t
  val switch : switch t
  val lazy_ : lazy_ t
  val assoc : key:'key Type_equal.Id.t -> action:'a t -> ('key, 'a) assoc t

  val assoc_on
    :  io_key:'io_key Type_equal.Id.t
    -> model_key:'model_key Type_equal.Id.t
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

val assoc
  :  key:'key
  -> id:'key Type_equal.Id.t
  -> compare:('key -> 'key -> int)
  -> 'a t
  -> ('key, 'a) assoc t

val assoc_on
  :  io_key:'io_key
  -> io_id:'io_key Type_equal.Id.t
  -> io_compare:('io_key -> 'io_key -> int)
  -> model_key:'model_key
  -> 'a t
  -> ('io_key, 'model_key, 'a) assoc_on t
