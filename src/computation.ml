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

type ('model, 'dynamic_action, 'static_action, 'result) without_id =
  | Return : 'result Value.t -> (unit, Nothing.t, Nothing.t, 'result) without_id
  | Leaf01 :
      { input : 'input Value.t
      ; dynamic_apply_action :
          ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action
      ; name : string
      }
      -> ( 'model
         , 'dynamic_action
         , 'static_action
         , 'model * ('dynamic_action -> unit Effect.t) * ('static_action -> unit Effect.t)
         )
           without_id
  | Leaf1 :
      { input : 'input Value.t
      ; dynamic_apply_action :
          ('input, 'dynamic_action, Nothing.t, 'model) dynamic_apply_action
      ; name : string
      }
      -> ( 'model
         , 'dynamic_action
         , Nothing.t
         , 'model * ('dynamic_action -> unit Effect.t) )
           without_id
  | Leaf0 :
      { (* [Leaf0] does not contain a [static_apply_action] for the same reason
           that we distinguish between dynamic and static actions at all. Because
           they do not depend on input, the static actions are not computed
           incrementally, and therefore can be collected into the top-level
           computation during the construction of the computation. *)
        compute : inject:('static_action -> unit Effect.t) -> 'model -> 'result
      ; name : string
      }
      -> ('model, Nothing.t, 'static_action, 'result) without_id
  | Leaf_incr :
      { input : 'input Value.t
      ; dynamic_apply_action :
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
      ; name : string
      }
      -> ('model, 'dynamic_action, Nothing.t, 'result) without_id
  | Model_cutoff :
      { t : ('m, 'dynamic_action, 'static_action, 'r) t
      ; model : 'm Meta.Model.t
      }
      -> ('m, 'dynamic_action, 'static_action, 'r) without_id
  | Subst :
      { from : ('m1, 'dynamic_action1, 'static_action1, 'r1) t
      ; via : 'r1 Type_equal.Id.t
      ; into : ('m2, 'dynamic_action2, 'static_action2, 'r2) t
      ; here : Source_code_position.t option
      }
      -> ( 'm1 * 'm2
         , ('dynamic_action1, 'dynamic_action2) Either.t
         , ('static_action1, 'static_action2) Either.t
         , 'r2 )
           without_id
  | Subst_stateless_from :
      { from : (unit, Nothing.t, Nothing.t, 'r1) t
      ; via : 'r1 Type_equal.Id.t
      ; into : ('m, 'dynamic_action, 'static_action, 'r2) t
      ; here : Source_code_position.t option
      }
      -> ('m, 'dynamic_action, 'static_action, 'r2) without_id
  | Subst_stateless_into :
      { from : ('m, 'dynamic_action, 'static_action, 'r1) t
      ; via : 'r1 Type_equal.Id.t
      ; into : (unit, Nothing.t, Nothing.t, 'r2) t
      ; here : Source_code_position.t option
      }
      -> ('m, 'dynamic_action, 'static_action, 'r2) without_id
  | Store :
      { id : 'x Type_equal.Id.t
      ; value : 'x Value.t
      ; inner : ('m, 'dynamic_action, 'static_action, 'r) t
      }
      -> ('m, 'dynamic_action, 'static_action, 'r) without_id
  | Fetch :
      { id : 'a Type_equal.Id.t
      ; default : 'r
      ; for_some : 'a -> 'r
      }
      -> (unit, Nothing.t, Nothing.t, 'r) without_id
  | Assoc :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; key_compare : 'k -> 'k -> int
      ; key_id : 'k Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : ('model, 'dynamic_action, 'static_action, 'result) t
      ; model_info : 'model Meta.Model.t
      ; action_info : 'dynamic_action Meta.Action.t
      ; result_by_k : ('result_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
      ; model_by_k : ('model_by_k, ('k, 'model, 'cmp) Map.t) Type_equal.t
      }
      -> ('model_by_k, 'k * 'dynamic_action, 'k * 'static_action, 'result_by_k) without_id
  | Assoc_on :
      { map : ('io_key, 'v, 'io_cmp) Map.t Value.t
      ; io_key_compare : 'io_key -> 'io_key -> int
      ; model_key_comparator : ('model_key, 'model_cmp) Comparator.t
      ; io_key_id : 'io_key Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : ('model, 'dynamic_action, 'static_action, 'result) t
      ; get_model_key : 'io_key -> 'v -> 'model_key
      ; model_info : 'model Meta.Model.t
      ; action_info : 'dynamic_action Meta.Action.t
      ; result_by_k : ('result_by_io_key, ('io_key, 'result, 'io_cmp) Map.t) Type_equal.t
      ; model_by_model_key :
          ('model_by_model_key, ('model_key, 'model, 'model_cmp) Map.t) Type_equal.t
      }
      -> ( 'model_by_model_key
         , 'io_key * 'model_key * 'dynamic_action
         , 'io_key * 'model_key * 'static_action
         , 'result_by_io_key )
           without_id
  | Assoc_simpl :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; by : Path.t -> 'k -> 'v -> 'result
      ; result_by_k : ('result_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
      }
      -> (unit, Nothing.t, Nothing.t, 'result_by_k) without_id
  | Switch :
      { match_ : int Value.t
      ; arms : 'r packed Int.Map.t
      }
      -> ( (int, Int.comparator_witness) Hidden.Multi_model.t
         , int Hidden.Action.t
         , int Hidden.Action.t
         , 'r )
           without_id
  (* Lazy wraps the model in an option because otherwise you could make
     infinitely sized models (by eagerly expanding a recursive model) which
     would stack-overflow during eval.  [None] really means "unchanged from the
     default", and is used to halt the the eager expansion. *)
  | Lazy :
      'r packed Lazy.t
      -> ( Hidden.Model.t option
         , unit Hidden.Action.t
         , unit Hidden.Action.t
         , 'r )
           without_id
  | Wrap :
      { model_id : 'outer_model Type_equal.Id.t
      ; inject_id : ('outer_dynamic_action -> unit Effect.t) Type_equal.Id.t
      ; inner : ('inner_model, 'inner_dynamic_action, 'inner_static_action, 'result) t
      ; dynamic_apply_action :
          ('result, 'outer_dynamic_action, Nothing.t, 'outer_model) dynamic_apply_action
      }
      -> ( 'outer_model * 'inner_model
         , ('outer_dynamic_action, 'inner_dynamic_action) Either.t
         , 'inner_static_action
         , 'result )
           without_id
  | With_model_resetter :
      ('m, 'dynamic_action, 'static_action, 'r) t
      -> ( 'm
         , 'dynamic_action
         , (unit, 'static_action) Either.t
         , 'r * unit Effect.t )
           without_id
  | Path : (unit, Nothing.t, Nothing.t, Path.t) without_id
  | Lifecycle :
      Lifecycle.t option Value.t
      -> (unit, Nothing.t, Nothing.t, unit) without_id

and ('model, 'dynamic_action, 'static_action, 'r) t =
  { t : ('model, 'dynamic_action, 'static_action, 'r) without_id
  ; id : 'r Type_equal.Id.t
  }

and 'r packed =
  | T :
      { t : ('model, 'dynamic_action, 'static_action, 'r) t
      ; dynamic_action : 'dynamic_action Meta.Action.t
      ; static_action : 'static_action Meta.Action.t
      ; model : 'model Meta.Model.t
      ; apply_static : ('dynamic_action, 'static_action, 'model) static_apply_action
      }
      -> 'r packed
