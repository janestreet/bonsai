open! Core
open! Import

type ('input, 'action, 'model) apply_action =
  inject:('action Action.t -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'input option
  -> 'model
  -> 'action Action.t
  -> 'model

type ('model, 'action, 'input, 'result, 'extra) eval_fun =
  environment:Environment.t
  -> path:Path.t
  -> clock:Time_source.t
  -> model:'model Incr.t
  -> inject:('action Action.t -> unit Effect.t)
  -> (('model, 'input, 'result) Snapshot.t * 'extra) Trampoline.t

type ('action, 'model) reset =
  inject:('action Action.t -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'model
  -> 'model

type ('model, 'action, 'input, 'result, 'extra) info =
  { model : 'model Meta.Model.t
  ; input : 'input Meta.Input.t
  ; action : 'action Action.id
  ; apply_action : ('input, 'action, 'model) apply_action
  ; run : ('model, 'action, 'input, 'result, 'extra) eval_fun
  ; reset : ('action, 'model) reset
  ; can_contain_path : bool
  }

type ('result, 'extra) packed_info =
  | T : (_, _, _, 'result, 'extra) info -> ('result, 'extra) packed_info

type 'result t =
  | Return : 'result Value.t -> 'result t
  | Leaf1 :
      { model : 'model Meta.Model.t
      ; input_id : 'input Meta.Input.t
      ; dynamic_action : 'dynamic_action Type_equal.Id.t
      ; apply_action :
          inject:('dynamic_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'input option
          -> 'model
          -> 'dynamic_action
          -> 'model
      ; reset :
          inject:('dynamic_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'model
          -> 'model
      ; input : 'input Value.t
      }
      -> ('model * ('dynamic_action -> unit Effect.t)) t
  | Leaf0 :
      { model : 'model Meta.Model.t
      ; static_action : 'static_action Type_equal.Id.t
      ; apply_action :
          inject:('static_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'model
          -> 'static_action
          -> 'model
      ; reset :
          inject:('static_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'model
          -> 'model
      }
      -> ('model * ('static_action -> unit Effect.t)) t
  | Leaf_incr :
      { input : 'input Value.t
      ; compute : Time_source.t -> 'input Incr.t -> 'result Incr.t
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
      ; can_contain_path : bool
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
      ; result_id : 'result Meta.Input.t
      ; action_id : 'outer_action Type_equal.Id.t
      ; inject_id : ('outer_action -> unit Effect.t) Type_equal.Id.t
      ; model_id : 'outer_model Type_equal.Id.t
      ; inner : 'result t
      ; dynamic_apply_action :
          inject:('outer_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'result option
          -> 'outer_model
          -> 'outer_action
          -> 'outer_model
      ; reset :
          inject:('outer_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> 'outer_model
          -> 'outer_model
      }
      -> 'result t
  | With_model_resetter :
      { reset_id : unit Effect.t Type_equal.Id.t
      ; inner : 'result t
      }
      -> 'result t
  | Path : Path.t t
  | Lifecycle : Lifecycle.t option Value.t -> unit t
