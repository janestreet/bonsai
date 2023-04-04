open! Core
open! Import

type ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action =
  inject_dynamic:('dynamic_action -> unit Effect.t)
  -> inject_static:('static_action -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'input option
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

type ('model, 'dynamic_action, 'static_action, 'input, 'result) eval_fun =
  environment:Environment.t
  -> path:Path.t
  -> clock:Incr.Clock.t
  -> model:'model Incr.t
  -> inject_dynamic:('dynamic_action -> unit Effect.t)
  -> inject_static:('static_action -> unit Effect.t)
  -> ('model, 'dynamic_action, 'input, 'result) Snapshot.t

type ('dynamic_action, 'static_action, 'model) reset =
  inject_dynamic:('dynamic_action -> unit Effect.t)
  -> inject_static:('static_action -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'model
  -> 'model

type ('model, 'dynamic_action, 'static_action, 'input, 'result) info =
  { model : 'model Meta.Model.t
  ; input : 'input Meta.Input.t
  ; dynamic_action : 'dynamic_action Meta.Action.t
  ; static_action : 'static_action Meta.Action.t
  ; apply_static : ('dynamic_action, 'static_action, 'model) static_apply_action
  ; apply_dynamic : ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action
  ; run : ('model, 'dynamic_action, 'static_action, 'input, 'result) eval_fun
  ; reset : ('dynamic_action, 'static_action, 'model) reset
  }

type 'result packed_info = T : (_, _, _, _, 'result) info -> 'result packed_info

type 'result t =
  | Return : 'result Value.t -> 'result t
  | Leaf01 :
      { model : 'model Meta.Model.t
      ; input_id : 'input Meta.Input.t
      ; dynamic_action : 'dynamic_action Meta.Action.t
      ; static_action : 'static_action Meta.Action.t
      ; apply_dynamic :
          ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action
      ; apply_static : ('dynamic_action, 'static_action, 'model) static_apply_action
      ; reset : ('dynamic_action, 'static_action, 'model) reset
      ; input : 'input Value.t
      }
      -> ('model * ('dynamic_action -> unit Effect.t) * ('static_action -> unit Effect.t))
           t
  | Leaf1 :
      { model : 'model Meta.Model.t
      ; input_id : 'input Meta.Input.t
      ; dynamic_action : 'dynamic_action Meta.Action.t
      ; apply_action : ('input, 'dynamic_action, Nothing.t, 'model) dynamic_apply_action
      ; reset : ('dynamic_action, Nothing.t, 'model) reset
      ; input : 'input Value.t
      }
      -> ('model * ('dynamic_action -> unit Effect.t)) t
  | Leaf0 :
      { model : 'model Meta.Model.t
      ; static_action : 'static_action Meta.Action.t
      ; apply_action : (Nothing.t, 'static_action, 'model) static_apply_action
      ; reset : (Nothing.t, 'static_action, 'model) reset
      }
      -> ('model * ('static_action -> unit Effect.t)) t
  | Leaf_incr :
      { input : 'input Value.t
      ; input_id : 'input Meta.Input.t
      ; compute : Incr.Clock.t -> 'input Incr.t -> 'result Incr.t
      }
      -> 'result t
  | Sub :
      { from : 'via t
      ; via : 'via Type_equal.Id.t
      ; into : 'result t
      ; here : Source_code_position.t option
      }
      -> 'result t
  | Store :
      { id : 'x Type_equal.Id.t
      ; value : 'x Value.t
      ; inner : 'result t
      }
      -> 'result t
  | Fetch :
      { id : 'a Type_equal.Id.t
      ; default : 'result
      ; for_some : 'a -> 'result
      }
      -> 'result t
  | Assoc :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; key_comparator : ('k, 'cmp) comparator
      ; key_id : 'k Type_equal.Id.t
      ; cmp_id : 'cmp Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : 'result t
      }
      -> ('k, 'result, 'cmp) Map.t t
  | Assoc_on :
      { map : ('io_key, 'v, 'io_cmp) Map.t Value.t
      ; io_comparator : ('io_key, 'io_cmp) comparator
      ; model_comparator : ('model_key, 'model_cmp) comparator
      ; io_key_id : 'io_key Type_equal.Id.t
      ; io_cmp_id : 'io_cmp Type_equal.Id.t
      ; model_key_id : 'model_key Type_equal.Id.t
      ; model_cmp_id : 'model_cmp Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : 'result t
      ; get_model_key : 'io_key -> 'v -> 'model_key
      }
      -> ('io_key, 'result, 'io_cmp) Map.t t
  | Assoc_simpl :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; by : Path.t -> 'k -> 'v -> 'result
      }
      -> ('k, 'result, 'cmp) Map.t t
  | Switch :
      { match_ : int Value.t
      ; arms : 'result t Int.Map.t
      ; here : Source_code_position.t
      }
      -> 'result t
  | Lazy : 'result t lazy_t -> 'result t
  | Wrap :
      { wrapper_model : 'outer_model Meta.Model.t
      ; action_id : 'outer_dynamic_action Meta.Action.t
      ; result_id : 'result Meta.Input.t
      ; inject_id : ('outer_dynamic_action -> unit Effect.t) Type_equal.Id.t
      ; model_id : 'outer_model Type_equal.Id.t
      ; inner : 'result t
      ; dynamic_apply_action :
          ('result, 'outer_dynamic_action, Nothing.t, 'outer_model) dynamic_apply_action
      ; reset : ('outer_dynamic_action, Nothing.t, 'outer_model) reset
      }
      -> 'result t
  | With_model_resetter :
      { reset_id : unit Effect.t Type_equal.Id.t
      ; inner : 'result t
      }
      -> 'result t
  | Path : Path.t t
  | Lifecycle : Lifecycle.t option Value.t -> unit t
