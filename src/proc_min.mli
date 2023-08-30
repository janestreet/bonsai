open! Core
open! Import

val read : 'a Value.t -> 'a Computation.t

val sub
  :  ?here:Lexing.position
  -> 'via Computation.t
  -> f:('via Value.t -> 'a Computation.t)
  -> 'a Computation.t

val switch
  :  here:Lexing.position
  -> match_:int Value.t
  -> branches:int
  -> with_:(int -> 'a Computation.t)
  -> 'a Computation.t

module Proc_incr : sig
  val value_cutoff : 'a Value.t -> equal:('a -> 'a -> bool) -> 'a Computation.t

  val compute_with_clock
    :  'a Value.t
    -> f:(Time_source.t -> 'a Incr.t -> 'b Incr.t)
    -> 'b Computation.t

  val of_module
    :  (module Component_s_incr
          with type Input.t = 'input
           and type Model.t = 'model
           and type Result.t = 'result)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> equal:('model -> 'model -> bool)
    -> default_model:'model
    -> 'input Value.t
    -> 'result Computation.t
end

module Dynamic_scope : sig
  val fetch
    :  id:'a Type_equal.Id.t
    -> default:'b
    -> for_some:('a -> 'b)
    -> 'b Computation.t

  val store
    :  id:'a Type_equal.Id.t
    -> value:'a Value.t
    -> inner:'b Computation.t
    -> 'b Computation.t
end

module Edge : sig
  val lifecycle : Lifecycle.t option Value.t -> unit Computation.t
end

module Computation_status : sig
  type 'input t =
    | Active of 'input
    | Inactive
  [@@deriving sexp_of]
end

val state_machine1
  :  ?sexp_of_action:('action -> Sexp.t)
  -> ?reset:('action Apply_action_context.t -> 'model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:
       ('action Apply_action_context.t
        -> 'input Computation_status.t
        -> 'model
        -> 'action
        -> 'model)
  -> 'input Value.t
  -> ('model * ('action -> unit Effect.t)) Computation.t

val state_machine0
  :  ?reset:('action Apply_action_context.t -> 'model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:('action Apply_action_context.t -> 'model -> 'action -> 'model)
  -> unit
  -> ('model * ('action -> unit Effect.t)) Computation.t

val assoc
  :  ('k, 'cmp) comparator
  -> ('k, 'v, 'cmp) Map_intf.Map.t Value.t
  -> f:('k Value.t -> 'v Value.t -> 'result Computation.t)
  -> ('k, 'result, 'cmp) Map_intf.Map.t Computation.t

val assoc_on
  :  ('io_k, 'io_cmp) comparator
  -> ('model_k, 'model_cmp) comparator
  -> ('io_k, 'v, 'io_cmp) Map_intf.Map.t Value.t
  -> get_model_key:('io_k -> 'v -> 'model_k)
  -> f:('io_k Value.t -> 'v Value.t -> 'a Computation.t)
  -> ('io_k, 'a, 'io_cmp) Map_intf.Map.t Computation.t

val lazy_ : 'a Computation.t lazy_t -> 'a Computation.t

val wrap
  :  ?reset:('action Apply_action_context.t -> 'model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:('action Apply_action_context.t -> 'a -> 'model -> 'action -> 'model)
  -> f:('model Value.t -> ('action -> unit Effect.t) Value.t -> 'a Computation.t)
  -> unit
  -> 'a Computation.t

val with_model_resetter
  :  (reset:unit Effect.t Value.t -> 'a Computation.t)
  -> 'a Computation.t

val path : Path.t Computation.t
