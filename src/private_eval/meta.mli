(** [Meta.Model] and [Meta.Action] contain the necessary functions associated with the
    hidden model and action types of Bonsai components. These are stored in
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
    }

  module Hidden : sig
      type 'a model

      type t =
        | T :
            { model : 'm
            ; info : 'm model
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
    :  ('k, 'cmp) Comparator.Module.t
    -> 'k Type_equal.Id.t
    -> 'cmp Type_equal.Id.t
    -> 'a t
    -> ('k, 'a, 'cmp) Map.t t

  val map_on
    :  ('k, 'cmp) Comparator.Module.t
    -> ('k_io, 'cmp_io) Comparator.Module.t
    -> 'k Type_equal.Id.t
    -> 'k_io Type_equal.Id.t
    -> 'cmp Type_equal.Id.t
    -> 'a t
    -> ('k, 'k_io * 'a, 'cmp) Map.t t

  val of_module
    :  sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool) option
    -> default:'a
    -> name:string
    -> 'a t
end

module Multi_model : sig
  type t

  val sexp_of_t : (int -> Sexp.t) -> t -> Sexp.t
  val find_exn : t -> int -> Model.Hidden.t
  val set : t -> key:int -> data:Model.Hidden.t -> t
  val to_models : t -> Model.Hidden.t Int.Map.t
  val of_models : Model.Hidden.t Int.Map.t -> t
  val model_info : t -> t Model.t
end

module Input : sig
  type 'a t [@@deriving sexp_of]

  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val unit : unit t
  val both : 'a t -> 'b t -> ('a * 'b) t

  val map
    :  'k Type_equal.Id.t
    -> 'cmp Type_equal.Id.t
    -> 'a t
    -> ('k, 'a, 'cmp) Map.t option t

  val create : ?name:string -> unit -> 'a t

  module Hidden : sig
    type 'a input = 'a t

    type 'key t =
      | T :
          { input : 'input
          ; type_id : 'input input
          ; key : 'key
          }
          -> 'key t

    val unit : unit t input
    val int : int t option input
  end
end
