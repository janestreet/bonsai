(** [Meta.Model] and [Meta.Action] contain the necessary functions associated with the
    hidden model and action types of Bonsai components. These are stored in
    [Computation.info]. *)

open! Core
open! Import

module type Type_id = sig
  type 'a t [@@deriving sexp_of]

  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val to_var_id : 'a t -> 'a Var_id.t
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

      type 'extra t =
        | T :
            { model : 'm
            ; info : 'm model
            ; extra : 'extra
            }
            -> 'extra t
      [@@deriving sexp_of]

      val equal : _ t -> _ t -> bool
      val create : 'a model -> 'a -> 'extra -> 'extra t
      val lazy_ : unit t option model
      val bind : unit -> 'a t option model
    end
    with type 'a model := 'a t

  val unit : unit t
  val both : 'a t -> 'b t -> ('a * 'b) t

  val map
    :  ('k, 'cmp) Comparator.Module.t
    -> 'k Var_id.t
    -> 'cmp Var_id.t
    -> 'a t
    -> ('k, 'a, 'cmp) Map.t t

  val map_on
    :  ('k, 'cmp) Comparator.Module.t
    -> ('k_io, 'cmp_io) Comparator.Module.t
    -> 'k Var_id.t
    -> 'k_io Var_id.t
    -> 'cmp Var_id.t
    -> 'a t
    -> ('k, 'k_io * 'a, 'cmp) Map.t t

  val of_module
    :  sexp_of_model:('a -> Sexp.t) option
    -> equal:('a -> 'a -> bool) option
    -> default:'a
    -> name:string
    -> 'a t
end

module Multi_model : sig
  type ('k, 'extra, 'cmp) t

  val sexp_of_t : ('k -> Sexp.t) -> ('k, _, _) t -> Sexp.t
  val remove : ('k, 'extra, 'cmp) t -> 'k -> ('k, 'extra, 'cmp) t
  val find_exn : ('k, 'extra, _) t -> 'k -> 'extra Model.Hidden.t
  val find : ('k, 'extra, _) t -> 'k -> 'extra Model.Hidden.t option

  val set
    :  ('k, 'extra, 'cmp) t
    -> key:'k
    -> data:'extra Model.Hidden.t
    -> ('k, 'extra, 'cmp) t

  val to_models : ('k, 'extra, 'cmp) t -> ('k, 'extra Model.Hidden.t, 'cmp) Map.t

  val of_models
    :  ('k, 'extra Model.Hidden.t, 'cmp) Map.t
    -> id:'k Var_id.t
    -> cmp_id:'cmp Var_id.t
    -> extra_id:'extra Var_id.t
    -> ('k, 'extra, 'cmp) t

  val empty
    :  ('a, 'b) Comparator.Module.t
    -> id:'a Var_id.t
    -> cmp_id:'b Var_id.t
    -> extra_id:'extra Var_id.t
    -> ('a, 'extra, 'b) t

  val model_info : ('k, 'extra, 'cmp) t -> ('k, 'extra, 'cmp) t Model.t
end

module Input : sig
  type 'a t [@@deriving sexp_of]

  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val unit : unit t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val map : 'k Var_id.t -> 'cmp Var_id.t -> 'a t -> ('k, 'a, 'cmp) Map.t t
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
    val int : int t input
  end
end
