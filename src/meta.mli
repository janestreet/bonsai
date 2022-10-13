
(** [Meta.Model] and [Meta.Action] contain the necessary functions associated with the
    hidden model and action types of Bonsai components.  These are stored in
    [Computation.info]. *)

open! Core
open! Import

module type Type_id = sig
  type 'a t [@@deriving sexp_of]

  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val to_type_id : 'a t -> 'a Type_equal.Id.t
  val to_sexp : 'a t -> 'a -> Sexp.t
  val nothing : Nothing.t t
  val unit : unit t
end

module Model : sig
  module Type_id : Type_id

  type 'a t =
    { default : 'a
    ; equal : 'a -> 'a -> bool
    ; type_id : 'a Type_id.t
    ; sexp_of : 'a -> Sexp.t
    ; of_sexp : Sexp.t -> 'a
    }

  module Hidden : sig
    type 'a model

    type t =
      | T :
          { model : 'm
          ; info : 'm model
          ; t_of_sexp : Sexp.t -> t
          }
          -> t
    [@@deriving equal, sexp_of]

    val create : 'a model -> 'a -> t
    val lazy_ : t option model
  end
  with type 'a model := 'a t

  val unit : unit t
  val both : 'a t -> 'b t -> ('a * 'b) t

  val map
    :  ('k, 'cmp) comparator
    -> 'k Type_equal.Id.t
    -> 'cmp Type_equal.Id.t
    -> 'a t
    -> ('k, 'a, 'cmp) Map.t t

  val of_module : (module Model with type t = 'a) -> default:'a -> name:string -> 'a t
end

module Action : sig
  module Type_id : Type_id

  type 'a t = 'a Type_id.t

  module Hidden : sig
    type 'a action

    type 'key t =
      | T :
          { action : 'a
          ; type_id : 'a action
          ; key : 'key
          }
          -> 'key t
    [@@deriving sexp_of]

    val unit : unit t action
    val int : int t action
  end
  with type 'a action := 'a t

  val nothing : Nothing.t t
  val both : 'a t -> 'b t -> ('a, 'b) Either.t t
  val map : 'k Type_equal.Id.t -> 'a t -> ('k * 'a) t

  val map_for_assoc_on
    :  'io_key Type_equal.Id.t
    -> 'model_key Type_equal.Id.t
    -> 'a t
    -> ('io_key * 'model_key * 'a) t

  val of_module : (module Action with type t = 'a) -> name:string -> 'a t
end

module Multi_model : sig
  type t

  val sexp_of_t : (int -> Sexp.t) -> t -> Sexp.t
  val t_of_sexp : t -> Sexp.t -> t
  val find_exn : t -> int -> Model.Hidden.t
  val set : t -> key:int -> data:Model.Hidden.t -> t
  val of_models : Model.Hidden.t Int.Map.t -> t
  val model_info : t -> t Model.t
end
