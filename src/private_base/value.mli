open! Core
open! Import

module Name_source : sig
  type t =
    | Sub of Source_code_position.t
    | Assoc_like_key
    | Assoc_like_data
    | Wrap_model
    | Wrap_inject
    | App_input
    | Model_resetter
    | Inverted_lifecycles_dependency
    | Fix_recurse
end

type _ without_position =
  | Constant : 'a Lazy.t -> 'a without_position
  | Incr : 'a Incr.t -> 'a without_position
  | Named : Name_source.t * 'a Type_equal.Id.t -> 'a without_position
  | Both : 'a t * 'b t -> ('a * 'b) without_position
  | Cutoff :
      { t : 'a t
      ; equal : 'a -> 'a -> bool
      ; added_by_let_syntax : bool
      }
      -> 'a without_position
  | Map :
      { t : 'a t
      ; f : 'a -> 'b
      }
      -> 'b without_position
  | Map2 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; f : 't1 -> 't2 -> 'r
      }
      -> 'r without_position
  | Map3 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; f : 't1 -> 't2 -> 't3 -> 'r
      }
      -> 'r without_position
  | Map4 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 'r
      }
      -> 'r without_position
  | Map5 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; t5 : 't5 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 'r
      }
      -> 'r without_position
  | Map6 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; t5 : 't5 t
      ; t6 : 't6 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 'r
      }
      -> 'r without_position
  | Map7 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; t5 : 't5 t
      ; t6 : 't6 t
      ; t7 : 't7 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 'r
      }
      -> 'r without_position
  | Exception : exn -> 'r without_position

and 'a t =
  { value : 'a without_position
  ; here : Source_code_position.t
  }

include Applicative.S with type 'a t := 'a t
include Applicative.Let_syntax with type 'a t := 'a t
include Mapn with type 'a t := 'a t

val named : here:[%call_pos] -> Name_source.t -> 'a Type_equal.Id.t -> 'a t

val cutoff
  :  here:[%call_pos]
  -> added_by_let_syntax:bool
  -> 'a t
  -> equal:('a -> 'a -> bool)
  -> 'a t

val eval : Environment.t -> 'a t -> 'a Incr.t
val of_incr : here:[%call_pos] -> 'a Incr.t -> 'a t
val return_lazy : here:[%call_pos] -> 'a Lazy.t -> 'a t
val return_exn : here:[%call_pos] -> exn -> 'a t
val transpose_opt : 'a t option -> 'a option t
