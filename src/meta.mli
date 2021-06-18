
(** [Meta.Model] and [Meta.Action] contain the necessary functions associated with the
    hidden model and action types of Bonsai components.  These are stored in
    [Computation.packed]. *)

open! Core
open! Import

val unit_type_id : unit Type_equal.Id.t

module Model : sig
  type 'a t =
    { default : 'a
    ; equal : 'a -> 'a -> bool
    ; type_id : 'a Type_equal.Id.t
    ; sexp_of : 'a -> Sexp.t
    ; of_sexp : Sexp.t -> 'a
    }

  val unit : unit t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val map : ('k, 'cmp) comparator -> 'a t -> ('k, 'a, 'cmp) Map.t t
  val of_module : (module Model with type t = 'a) -> default:'a -> name:string -> 'a t
end

module Action : sig
  type 'a t = 'a Type_equal.Id.t

  val nothing : Nothing.t t
  val both : 'a t -> 'b t -> ('a, 'b) Either.t t
  val map : ('k, 'cmp) comparator -> 'a t -> ('k * 'a) t
  val of_module : (module Action with type t = 'a) -> name:string -> 'a t
end
