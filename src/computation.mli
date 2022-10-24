open! Core
open! Import

type ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action =
  inject_dynamic:('dynamic_action -> unit Effect.t)
  -> inject_static:('static_action -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'input
  -> 'model
  -> 'dynamic_action
  -> 'model

type ('dynamic_action, 'static_action, 'model) static_apply_action =
  inject_dynamic:('dynamic_action -> unit Effect.t)
  -> inject_static:('static_action -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'model
  -> 'static_action
  -> 'model

type ('model, 'dynamic_action, 'static_action, 'result) eval_fun =
  environment:Environment.t
  -> path:Path.t
  -> clock:Incr.Clock.t
  -> model:'model Incr.t
  -> inject_dynamic:('dynamic_action -> unit Effect.t)
  -> inject_static:('static_action -> unit Effect.t)
  -> ('model, 'dynamic_action, 'result) Snapshot.t

type ('dynamic_action, 'static_action, 'model) reset =
  inject_dynamic:('dynamic_action -> unit Effect.t)
  -> inject_static:('static_action -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'model
  -> 'model

type ('model, 'dynamic_action, 'static_action, 'result) info =
  { model : 'model Meta.Model.t
  ; dynamic_action : 'dynamic_action Meta.Action.t
  ; static_action : 'static_action Meta.Action.t
  ; apply_static : ('dynamic_action, 'static_action, 'model) static_apply_action
  ; run : ('model, 'dynamic_action, 'static_action, 'result) eval_fun
  ; reset : ('dynamic_action, 'static_action, 'model) reset
  }

type 'result packed_info = T : (_, _, _, 'result) info -> 'result packed_info

type 'result kind =
  | Return : 'result Value.t -> 'result kind
  | Leaf01 :
      { model : 'model Meta.Model.t
      ; dynamic_action : 'dynamic_action Meta.Action.t
      ; static_action : 'static_action Meta.Action.t
      ; apply_dynamic :
          ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action
      ; apply_static : ('dynamic_action, 'static_action, 'model) static_apply_action
      ; reset : ('dynamic_action, 'static_action, 'model) reset
      ; input : 'input Value.t
      }
      -> ('model * ('dynamic_action -> unit Effect.t) * ('static_action -> unit Effect.t))
           kind
  | Leaf1 :
      { model : 'model Meta.Model.t
      ; dynamic_action : 'dynamic_action Meta.Action.t
      ; apply_action : ('input, 'dynamic_action, Nothing.t, 'model) dynamic_apply_action
      ; reset : ('dynamic_action, Nothing.t, 'model) reset
      ; input : 'input Value.t
      }
      -> ('model * ('dynamic_action -> unit Effect.t)) kind
  | Leaf0 :
      { model : 'model Meta.Model.t
      ; static_action : 'static_action Meta.Action.t
      ; apply_action : (Nothing.t, 'static_action, 'model) static_apply_action
      ; reset : (Nothing.t, 'static_action, 'model) reset
      ; compute : inject:('static_action -> unit Effect.t) -> 'model -> 'result
      }
      -> 'result kind
  | Leaf_incr :
      { model : 'model Meta.Model.t
      ; dynamic_action : 'dynamic_action Meta.Action.t
      ; input : 'input Value.t
      ; reset : ('dynamic_action, Nothing.t, 'model) reset
      ; apply_dynamic :
          'input Incr.t
          -> inject:('dynamic_action -> unit Effect.t)
          -> (schedule_event:(unit Effect.t -> unit)
              -> 'model
              -> 'dynamic_action
              -> 'model)
               Incr.t
      ; compute :
          Incr.Clock.t
          -> 'input Incr.t
          -> 'model Incr.t
          -> inject:('dynamic_action -> unit Effect.t)
          -> 'result Incr.t
      }
      -> 'result kind
  | Model_cutoff : 'result t -> 'result kind
  | Sub :
      { from : 'via t
      ; via : 'via Type_equal.Id.t
      ; into : 'result t
      ; here : Source_code_position.t option
      }
      -> 'result kind
  | Store :
      { id : 'x Type_equal.Id.t
      ; value : 'x Value.t
      ; inner : 'result t
      }
      -> 'result kind
  | Fetch :
      { id : 'a Type_equal.Id.t
      ; default : 'result
      ; for_some : 'a -> 'result
      }
      -> 'result kind
  | Assoc :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; key_comparator : ('k, 'cmp) comparator
      ; key_id : 'k Type_equal.Id.t
      ; cmp_id : 'cmp Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : 'result t
      }
      -> ('k, 'result, 'cmp) Map.t kind
  | Assoc_on :
      { map : ('io_key, 'v, 'io_cmp) Map.t Value.t
      ; io_comparator : ('io_key, 'io_cmp) comparator
      ; model_comparator : ('model_key, 'model_cmp) comparator
      ; io_key_id : 'io_key Type_equal.Id.t
      ; model_key_id : 'model_key Type_equal.Id.t
      ; model_cmp_id : 'model_cmp Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : 'result t
      ; get_model_key : 'io_key -> 'v -> 'model_key
      }
      -> ('io_key, 'result, 'io_cmp) Map.t kind
  | Assoc_simpl :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; by : Path.t -> 'k -> 'v -> 'result
      }
      -> ('k, 'result, 'cmp) Map.t kind
  | Switch :
      { match_ : int Value.t
      ; arms : 'result t Int.Map.t
      }
      -> 'result kind
  | Lazy : 'result t lazy_t -> 'result kind
  | Wrap :
      { wrapper_model : 'outer_model Meta.Model.t
      ; action_id : 'outer_dynamic_action Meta.Action.t
      ; inject_id : ('outer_dynamic_action -> unit Effect.t) Type_equal.Id.t
      ; model_id : 'outer_model Type_equal.Id.t
      ; inner : 'result t
      ; dynamic_apply_action :
          ('result, 'outer_dynamic_action, Nothing.t, 'outer_model) dynamic_apply_action
      ; reset : ('outer_dynamic_action, Nothing.t, 'outer_model) reset
      }
      -> 'result kind
  | With_model_resetter :
      { reset_id : unit Effect.t Type_equal.Id.t
      ; inner : 'result t
      }
      -> 'result kind
  | Path : Path.t kind
  | Lifecycle : Lifecycle.t option Value.t -> unit kind
  | Identity : 'result t -> 'result kind

and 'result t = private
  { kind : 'result kind
  ; id : 'result Type_equal.Id.t
  ; info : 'result packed_info lazy_t
  }

module type Gather_impl = sig
  val gather : 'result t -> 'result packed_info

  val wrap_computation
    :  pack:('result kind -> 'result packed_info lazy_t -> 'result t)
    -> 'result kind
    -> 'result t
end

module Proc_min (G : Gather_impl) : sig
  val wrap_computation : 'result kind -> 'result t
  val gather : 'result t -> 'result packed_info
  val read : 'a Value.t -> 'a t
  val sub : ?here:Lexing.position -> 'via t -> f:('via Value.t -> 'a t) -> 'a t
  val switch : match_:int Value.t -> branches:int -> with_:(int -> 'a t) -> 'a t

  module Proc_incr : sig
    val value_cutoff : 'a Value.t -> equal:('a -> 'a -> bool) -> 'a t
    val model_cutoff : 'a t -> 'a t

    val compute_with_clock
      :  'a Value.t
      -> f:(Incr.Clock.t -> 'a Incr.t -> 'b Incr.t)
      -> 'b t

    val of_module
      :  (module Component_s_incr
           with type Input.t = 'input
            and type Model.t = 'model
            and type Result.t = 'result)
      -> default_model:'model
      -> 'input Value.t
      -> 'result t
  end

  module Dynamic_scope : sig
    val fetch : id:'a Type_equal.Id.t -> default:'b -> for_some:('a -> 'b) -> 'b t
    val store : id:'a Type_equal.Id.t -> value:'a Value.t -> inner:'b t -> 'b t
  end

  module Edge : sig
    val lifecycle : Lifecycle.t option Value.t -> unit t
  end

  val state_machine01
    :  (module Model with type t = 'model)
    -> (module Action with type t = 'dynamic_action)
    -> (module Action with type t = 'static_action)
    -> ?reset:('dynamic_action, 'static_action, 'model) reset
    -> default_model:'model
    -> apply_dynamic:
         ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action
    -> apply_static:('dynamic_action, 'static_action, 'model) static_apply_action
    -> 'input Value.t
    -> ('model * ('dynamic_action -> unit Effect.t) * ('static_action -> unit Effect.t)) t

  val state_machine1
    :  (module Model with type t = 'model)
    -> (module Action with type t = 'action)
    -> ?reset:
         (inject:('action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'model
          -> 'model)
    -> default_model:'model
    -> apply_action:
         (inject:('action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'input
          -> 'model
          -> 'action
          -> 'model)
    -> 'input Value.t
    -> ('model * ('action -> unit Effect.t)) t

  val state_machine0
    :  ?reset:
      (inject:('action -> unit Effect.t)
       -> schedule_event:(unit Effect.t -> unit)
       -> 'model
       -> 'model)
    -> (module Model with type t = 'model)
    -> (module Action with type t = 'action)
    -> default_model:'model
    -> apply_action:
         (inject:('action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'model
          -> 'action
          -> 'model)
    -> ('model * ('action -> unit Effect.t)) t

  val assoc
    :  ('k, 'cmp) comparator
    -> ('k, 'v, 'cmp) Map_intf.Map.t Value.t
    -> f:('k Value.t -> 'v Value.t -> 'result t)
    -> ('k, 'result, 'cmp) Map_intf.Map.t t

  val assoc_on
    :  ('io_k, 'io_cmp) comparator
    -> ('model_k, 'model_cmp) comparator
    -> ('io_k, 'v, 'io_cmp) Map_intf.Map.t Value.t
    -> get_model_key:('io_k -> 'v -> 'model_k)
    -> f:('io_k Value.t -> 'v Value.t -> 'a t)
    -> ('io_k, 'a, 'io_cmp) Map_intf.Map.t t

  val lazy_ : 'a t lazy_t -> 'a t

  val wrap
    :  ?reset:
      (inject:('action -> unit Effect.t)
       -> schedule_event:(unit Effect.t -> unit)
       -> 'model
       -> 'model)
    -> (module Model with type t = 'model)
    -> default_model:'model
    -> apply_action:
         (inject:('action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'a
          -> 'model
          -> 'action
          -> 'model)
    -> f:('model Value.t -> ('action -> unit Effect.t) Value.t -> 'a t)
    -> 'a t

  val with_model_resetter : (reset:unit Effect.t Value.t -> 'a t) -> 'a t
  val path : Path.t t
end
