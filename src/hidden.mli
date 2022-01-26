open! Core
open! Import

module Action : sig
  type 'key t =
    | T :
        { action : 'a
        ; type_id : 'a Meta.Action.t
        ; key : 'key
        }
        -> 'key t
  [@@deriving sexp_of]

  val type_id : ('key -> Sexp.t) -> 'key t Type_equal.Id.t
end

module Model : sig
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

  val sexp_of_t : ('k -> Sexp.t) -> ('k, Model.t, 'a) Map.t -> Sexp.t

  val t_of_sexp
    :  ('k, 'cmp) comparator
    -> ('k, Model.t, 'cmp) Map.t
    -> Sexp.t
    -> ('k, Model.t, 'cmp) Map.t

  val find_exn : ('k, 'cmp) t -> 'k -> Model.t
  val set : ('k, 'cmp) t -> key:'k -> data:Model.t -> ('k, 'cmp) t

  val model_info
    :  ('k, 'cmp) comparator
    -> ('k, Model.t, 'cmp) Map.t
    -> ('k, 'cmp) t Meta.Model.t
end
