open! Core
open! Import

module rec T : sig
  type ('input, 'action, 'model) apply_action =
    inject:('action Action.t -> unit Effect.t)
    -> schedule_event:(unit Effect.t -> unit)
    -> 'input option
    -> 'model
    -> 'action Action.t
    -> 'model

  type ('model, 'action, 'input, 'result, 'extra) eval_fun =
    environment:Environment.t
    -> fix_envs:Environment.Recursive.t
    -> path:Path.t
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
    ; may_contain : May_contain.Unresolved.t
    }

  type ('result, 'extra) packed_info =
    | T : (_, _, _, 'result, 'extra) info -> ('result, 'extra) packed_info
end =
  T

and Recursive_scopes : sig
  type t

  val empty : t
  val add_exn : t -> key:'a Fix_id.t -> data:('a, unit) T.packed_info lazy_t -> t
  val add_overwriting : t -> key:'a Fix_id.t -> data:('a, unit) T.packed_info lazy_t -> t
  val find : t -> 'a Fix_id.t -> ('a, unit) T.packed_info lazy_t option
  val find_exn : t -> 'a Fix_id.t -> ('a, unit) T.packed_info lazy_t
end = struct
  include
    Univ_map.Make
      (Fix_id)
      (struct
        type 'a t = ('a, unit) T.packed_info lazy_t

        let sexp_of_t _sexp_of_a = sexp_of_opaque
      end)

  let add_overwriting t ~key ~data = update t key ~f:(fun _ -> data)
end

include T

type 'result t =
  | Return :
      { value : 'result Value.t
      ; here : Source_code_position.t
      }
      -> 'result t
  | Leaf1 :
      { model : 'model Meta.Model.t
      ; input_id : 'input Meta.Input.t
      ; dynamic_action : 'dynamic_action Type_equal.Id.t
      ; apply_action :
          inject:('dynamic_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> time_source:Ui_time_source.t
          -> 'input option
          -> 'model
          -> 'dynamic_action
          -> 'model
      ; reset :
          inject:('dynamic_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> time_source:Ui_time_source.t
          -> 'model
          -> 'model
      ; input : 'input Value.t
      ; here : Source_code_position.t
      }
      -> ('model * ('dynamic_action -> unit Effect.t)) t
  | Leaf0 :
      { model : 'model Meta.Model.t
      ; static_action : 'static_action Type_equal.Id.t
      ; apply_action :
          inject:('static_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> time_source:Ui_time_source.t
          -> 'model
          -> 'static_action
          -> 'model
      ; reset :
          inject:('static_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> time_source:Ui_time_source.t
          -> 'model
          -> 'model
      ; here : Source_code_position.t
      }
      -> ('model * ('static_action -> unit Effect.t)) t
  | Leaf_incr :
      { input : 'input Value.t
      ; compute : Time_source.t -> 'input Incr.t -> 'result Incr.t
      ; here : Source_code_position.t
      }
      -> 'result t
  | Sub :
      { from : 'via t
      ; via : 'via Type_equal.Id.t
      ; into : 'result t
      ; invert_lifecycles : bool
      ; here : Source_code_position.t
      }
      -> 'result t
  | Store :
      { id : 'x Type_equal.Id.t
      ; value : 'x Value.t
      ; inner : 'result t
      ; here : Source_code_position.t
      }
      -> 'result t
  | Fetch :
      { id : 'a Type_equal.Id.t
      ; default : 'result
      ; for_some : 'a -> 'result
      ; here : Source_code_position.t
      }
      -> 'result t
  | Assoc :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; key_comparator : ('k, 'cmp) Comparator.Module.t
      ; key_id : 'k Type_equal.Id.t
      ; cmp_id : 'cmp Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : 'result t
      ; here : Source_code_position.t
      }
      -> ('k, 'result, 'cmp) Map.t t
  | Assoc_on :
      { map : ('io_key, 'v, 'io_cmp) Map.t Value.t
      ; io_comparator : ('io_key, 'io_cmp) Comparator.Module.t
      ; model_comparator : ('model_key, 'model_cmp) Comparator.Module.t
      ; io_key_id : 'io_key Type_equal.Id.t
      ; io_cmp_id : 'io_cmp Type_equal.Id.t
      ; model_key_id : 'model_key Type_equal.Id.t
      ; model_cmp_id : 'model_cmp Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : 'result t
      ; get_model_key : 'io_key -> 'v -> 'model_key
      ; here : Source_code_position.t
      }
      -> ('io_key, 'result, 'io_cmp) Map.t t
  | Assoc_simpl :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; by : Path.t -> 'k -> 'v -> 'result
      ; may_contain : May_contain.Resolved.t
      ; here : Source_code_position.t
      }
      -> ('k, 'result, 'cmp) Map.t t
  | Switch :
      { match_ : int Value.t
      ; arms : 'result t Int.Map.t
      ; here : Source_code_position.t
      }
      -> 'result t
  | Lazy :
      { t : 'result t lazy_t
      ; here : Source_code_position.t
      }
      -> 'result t
  (* [Fix_define] and [Fix_recurse] allow for recursive Bonsai computations. They are more
     restrictive than [Lazy] and can therefore be introspected and optimized at
     [gather]-time.

     These constructors can pose implementation considerations because they introduce
     bidirectional dependencies: [Fix_define]'s result depends on its body which may
     contain [Fix_recurse]s. And, a [Fix_recurse] is effectively evaluated to the body of
     the enclosing [Fix_define]. *)
  | Fix_define :
      { fix_id : 'result Fix_id.t
      ; initial_input : 'input Value.t
      ; input_id : 'input Type_equal.Id.t
      ; result : 'result t
      ; here : Source_code_position.t
      }
      -> 'result t
  (** [Fix_define] defines an enclosing "recursive scope", whose body may contain
      instances of [Fix_recurse]. *)
  | Fix_recurse :
      { input : 'input Value.t
      ; input_id : 'input Type_equal.Id.t
      ; fix_id : 'result Fix_id.t
      ; here : Source_code_position.t
      }
      -> 'result t
  (** [Fix_recurse] is a recursive call to the [Fix_define] identified by [fix_id]. *)
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
          -> time_source:Ui_time_source.t
          -> 'result option
          -> 'outer_model
          -> 'outer_action
          -> 'outer_model
      ; reset :
          inject:('outer_action -> unit Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> time_source:Ui_time_source.t
          -> 'outer_model
          -> 'outer_model
      ; here : Source_code_position.t
      }
      -> 'result t
  | With_model_resetter :
      { reset_id : unit Effect.t Type_equal.Id.t
      ; inner : 'result t
      ; here : Source_code_position.t
      }
      -> 'result t
  | Path : { here : Source_code_position.t } -> Path.t t
  | Lifecycle :
      { lifecycle : Lifecycle.t option Value.t
      ; here : Source_code_position.t
      }
      -> unit t
  | Computation_watcher :
      { inner : 'result t
      ; enable_watcher : bool
      ; here : Source_code_position.t
      ; free_vars : Computation_watcher.Type_id_location_map.t
      ; config : Computation_watcher.Config.t
      ; queue : Computation_watcher.Output_queue.t option
      ; value_id_observation_definition_positions :
          (Computation_watcher.Source_code_positions.finalized
             Computation_watcher.Source_code_positions.t
          * Computation_watcher.Config.t)
            Computation_watcher.Id_location_hashmap.t
            option
      }
      -> 'result t

type 'result gather_fun =
  recursive_scopes:Recursive_scopes.t
  -> time_source:Time_source.t
  -> 'result t
  -> ('result, unit) packed_info Trampoline.t

let source_code_position (type result) (computation : result t) =
  match computation with
  | Sub { here; _ }
  | Switch { here; _ }
  | Return { here; _ }
  | Leaf1 { here; _ }
  | Leaf0 { here; _ }
  | Leaf_incr { here; _ }
  | Store { here; _ }
  | Fetch { here; _ }
  | Assoc { here; _ }
  | Assoc_on { here; _ }
  | Assoc_simpl { here; _ }
  | Lazy { here; _ }
  | Fix_define { here; _ }
  | Fix_recurse { here; _ }
  | Wrap { here; _ }
  | With_model_resetter { here; _ }
  | Path { here; _ }
  | Lifecycle { here; _ }
  | Computation_watcher { here; _ } -> here
;;
