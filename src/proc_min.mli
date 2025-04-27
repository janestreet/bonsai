open! Core
open! Import

val read : here:[%call_pos] -> 'a Value.t -> 'a Computation.t

val sub
  :  here:[%call_pos]
  -> 'via Computation.t
  -> f:local_ ('via Value.t -> 'a Computation.t)
  -> 'a Computation.t

val switch
  :  here:Lexing.position
  -> match_:int Value.t
  -> branches:int
  -> with_:local_ (int -> 'a Computation.t)
  -> 'a Computation.t

module Proc_incr : sig
  val value_cutoff
    :  here:[%call_pos]
    -> 'a Value.t
    -> equal:('a -> 'a -> bool)
    -> 'a Computation.t

  val compute_with_clock
    :  here:[%call_pos]
    -> 'a Value.t
    -> f:(Time_source.t -> 'a Incr.t -> 'b Incr.t)
    -> 'b Computation.t

  val of_module
    :  here:[%call_pos]
    -> (module Component_s_incr
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
    :  here:[%call_pos]
    -> id:'a Type_equal.Id.t
    -> default:'b
    -> for_some:('a -> 'b)
    -> unit
    -> 'b Computation.t

  val store
    :  here:[%call_pos]
    -> id:'a Type_equal.Id.t
    -> value:'a Value.t
    -> inner:'b Computation.t
    -> unit
    -> 'b Computation.t
end

val watch_computation
  :  here:Source_code_position.t
  -> log_model_before:bool
  -> log_model_after:bool
  -> log_action:bool
  -> log_incr_info:bool
  -> log_watcher_positions:bool
  -> log_dependency_definition_position:bool
  -> label:string option
  -> 'a Computation.t
  -> 'a Computation.t

module Edge : sig
  val lifecycle : here:[%call_pos] -> Lifecycle.t option Value.t -> unit Computation.t
end

module Computation_status : sig
  type 'input t =
    | Active of 'input
    | Inactive
  [@@deriving sexp_of]
end

val state_machine_with_input
  :  here:[%call_pos]
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?reset:(('action, unit) Apply_action_context.t -> 'model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:
       (('action, unit) Apply_action_context.t
        -> 'input Computation_status.t
        -> 'model
        -> 'action
        -> 'model)
  -> 'input Value.t
  -> ('model * ('action -> unit Effect.t)) Computation.t

val state_machine
  :  here:[%call_pos]
  -> ?reset:(('action, unit) Apply_action_context.t -> 'model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:(('action, unit) Apply_action_context.t -> 'model -> 'action -> 'model)
  -> unit
  -> ('model * ('action -> unit Effect.t)) Computation.t

val assoc
  :  here:[%call_pos]
  -> ('k, 'cmp) Comparator.Module.t
  -> ('k, 'v, 'cmp) Map.t Value.t
  -> f:local_ ('k Value.t -> 'v Value.t -> 'result Computation.t)
  -> ('k, 'result, 'cmp) Map.t Computation.t

val assoc_on
  :  here:[%call_pos]
  -> ('io_k, 'io_cmp) Comparator.Module.t
  -> ('model_k, 'model_cmp) Comparator.Module.t
  -> ('io_k, 'v, 'io_cmp) Map.t Value.t
  -> get_model_key:('io_k -> 'v -> 'model_k)
  -> f:local_ ('io_k Value.t -> 'v Value.t -> 'a Computation.t)
  -> ('io_k, 'a, 'io_cmp) Map.t Computation.t

val fix
  :  here:[%call_pos]
  -> 'input Value.t
  -> f:
       local_ (recurse:('input Value.t -> 'result Computation.t)
               -> 'input Value.t
               -> 'result Computation.t)
  -> 'result Computation.t

val lazy_ : here:[%call_pos] -> 'a Computation.t lazy_t -> 'a Computation.t

val wrap
  :  here:[%call_pos]
  -> ?reset:(('action, unit) Apply_action_context.t -> 'model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:
       (('action, unit) Apply_action_context.t
        -> 'a Computation_status.t
        -> 'model
        -> 'action
        -> 'model)
  -> f:local_ ('model Value.t -> ('action -> unit Effect.t) Value.t -> 'a Computation.t)
  -> unit
  -> 'a Computation.t

val with_model_resetter
  :  here:[%call_pos]
  -> local_ (reset:unit Effect.t Value.t -> 'a Computation.t)
  -> 'a Computation.t

val with_inverted_lifecycle_ordering
  :  here:[%call_pos]
  -> compute_dep:'input Computation.t
  -> local_ ('input Value.t -> 'a Computation.t)
  -> 'a Computation.t

val path : here:[%call_pos] -> unit -> Path.t Computation.t
