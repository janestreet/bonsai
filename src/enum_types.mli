open! Core_kernel
open! Import

module Case_action : sig
  type 'key t =
    | T :
        { action : 'a
        ; type_id : 'a Type_equal.Id.t
        ; key : 'key
        }
        -> 'key t
  [@@deriving sexp_of]

  val type_id : ('key -> Sexp.t) -> 'key t Type_equal.Id.t
end

module Case_model : sig
  type t =
    | T :
        { model : 'm
        ; info : 'm Meta.Model.t
        ; t_of_sexp : Sexp.t -> t
        }
        -> t
  [@@deriving equal, sexp_of]

  val create : 'a Meta.Model.t -> 'a -> t
end

module Multi_model : sig
  type ('k, 'cmp) t

  val sexp_of_t : ('k -> Sexp.t) -> ('k, Case_model.t, 'a) Map.t -> Sexp.t

  val t_of_sexp
    :  ('k, 'cmp) comparator
    -> ('k, Case_model.t, 'cmp) Map.t
    -> Sexp.t
    -> ('k, Case_model.t, 'cmp) Map.t

  val find_exn : ('k, 'cmp) t -> 'k -> Case_model.t
  val set : ('k, 'cmp) t -> key:'k -> data:Case_model.t -> ('k, 'cmp) t

  val model_info
    :  ('k, 'cmp) comparator
    -> ('k, Case_model.t, 'cmp) Map.t
    -> ('k, 'cmp) t Meta.Model.t
end
