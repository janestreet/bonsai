open! Core
open! Import
module Source_code_positions = Computation_watcher.Source_code_positions
module Output_queue = Computation_watcher.Output_queue

module Types = struct
  module Down = struct
    type t =
      { source_code_positions : Source_code_positions.pending Source_code_positions.t
      ; visited_stores : Type_equal.Id.Uid.Set.t
      ; watcher_queue : Output_queue.t
      ; config : Computation_watcher.Config.t
      ; enable_watcher : bool
      ; should_run_computation_watcher : bool
      ; value_id_observation_definition_positions :
          (Source_code_positions.finalized Source_code_positions.t
          * Computation_watcher.Config.t)
            Computation_watcher.Id_location_hashmap.t
      }
  end

  module Acc = Unit

  module Up = struct
    type t = Computation_watcher.Type_id_location_map.t

    let empty = Computation_watcher.Type_id_location_map.empty
    let combine = Computation_watcher.Type_id_location_map.merge
    let empty_for_lazy = empty
  end
end

let rewrite_resetter
  ~reset
  ~model
  ~(down : Types.Down.t)
  ~(kind : [ `State_machine0 | `State_machine1 | `Wrap ])
  ~watcher_queue
  ~source_code_positions
  =
  let reset ~inject ~schedule_event ~time_source reset_model =
    let new_model = reset ~inject ~schedule_event ~time_source reset_model in
    if not (model.Meta.Model.equal new_model reset_model)
    then
      Queue.enqueue
        watcher_queue
        (Computation_watcher.Node.Reset
           { source_code_positions =
               Source_code_positions.extract_finalized source_code_positions
           ; config = down.config
           ; model_before = reset_model
           ; model_after = new_model
           ; sexp_of_model = model.Meta.Model.sexp_of
           ; kind
           });
    new_model
  in
  reset
;;

let apply_action_wrapper
  ~model
  ~action
  ~(down : Types.Down.t)
  ~apply_action_model
  ~apply_action_action
  ~(kind : [ `Wrap | `State_machine1 | `State_machine0 ])
  ~watcher_queue
  ~source_code_positions
  partially_applied_apply_action
  =
  let new_model = partially_applied_apply_action apply_action_model apply_action_action in
  if not (phys_equal apply_action_model new_model)
  then
    Queue.enqueue
      watcher_queue
      (Computation_watcher.Node.State_machine_like
         { source_code_positions =
             Source_code_positions.extract_finalized source_code_positions
         ; config = down.config
         ; model_before = apply_action_model
         ; model_after = new_model
         ; sexp_of_model = model.Meta.Model.sexp_of
         ; action = Some apply_action_action
         ; sexp_of_action = Some (Type_equal.Id.to_sexp action)
         ; kind
         });
  new_model
;;

let rewrite_apply_action_leaf1
  ~apply_action
  ~model
  ~action
  ~down
  ~(kind : [> `Wrap | `State_machine1 ])
  ~watcher_queue
  ~source_code_positions
  =
  let apply_action
    ~inject
    ~schedule_event
    ~time_source
    apply_action_input
    apply_action_model
    apply_action_action
    =
    apply_action ~inject ~schedule_event ~time_source apply_action_input
    |> apply_action_wrapper
         ~model
         ~action
         ~down
         ~apply_action_model
         ~apply_action_action
         ~kind
         ~watcher_queue
         ~source_code_positions
  in
  apply_action
;;

let rewrite_apply_action_leaf0
  ~apply_action
  ~model
  ~action
  ~down
  ~watcher_queue
  ~source_code_positions
  =
  let apply_action
    ~inject
    ~schedule_event
    ~time_source
    apply_action_model
    apply_action_action
    =
    apply_action ~inject ~schedule_event ~time_source
    |> apply_action_wrapper
         ~model
         ~action
         ~down
         ~apply_action_model
         ~apply_action_action
         ~kind:`State_machine0
         ~watcher_queue
         ~source_code_positions
  in
  apply_action
;;

module F (Recurse : Fix_transform.Recurse with module Types := Types) = struct
  open Trampoline.Let_syntax

  let transform_v
    (type a)
    ({ Types.Down.source_code_positions
     ; should_run_computation_watcher = _
     ; config
     ; watcher_queue
     ; enable_watcher
     ; value_id_observation_definition_positions
     ; visited_stores = _
     } as down)
    ()
    ({ value; here } as v : a Value.t)
    =
    (* We only want to transform values if we're below a Computation_watcher node, which
       is what [enable_watcher] signifies *)
    match enable_watcher with
    | false -> Recurse.on_value down () `Skipping_over v
    | true ->
      (match value with
       | Named (_, id) ->
         ( ()
         , Computation_watcher.Type_id_location_map.singleton
             id
             (Source_code_positions.add_dependency_definition source_code_positions here)
         , v )
       | Incr incr_value ->
         let id = Incremental.For_analyzer.node_id (Incr.pack incr_value) in
         let has_been_set =
           Computation_watcher.Id_location_hashmap.update_and_check_if_value_set
             ~id:(`Incr id)
             ~update_data:
               ( Source_code_positions.add_dependency_definition source_code_positions here
                 |> Source_code_positions.extract_finalized
               , config )
             value_id_observation_definition_positions
         in
         let value_node =
           match has_been_set with
           | `Not_set ->
             let new_incr =
               Computation_watcher.instrument_incremental_node
                 ~here:[%here]
                 ~id:(`Incr id)
                 ~watcher_queue
                 ~value_id_observation_definition_positions
                 incr_value
             in
             { Value.value = Incr new_incr; here }
           | `Already_set -> v
         in
         (), Computation_watcher.Type_id_location_map.empty, value_node
       | Map _ | Map2 _ | Map3 _ | Map4 _ | Map5 _ | Map6 _ | Map7 _ ->
         Recurse.on_value
           { down with
             source_code_positions =
               Source_code_positions.add_depended_on_at source_code_positions here
           }
           ()
           `Skipping_over
           v
       (* For any other node, just perform the default *)
       | _ -> Recurse.on_value down () `Skipping_over v)
  ;;

  let transform_c
    (type a)
    (Types.Down.
       { source_code_positions
       ; config
       ; should_run_computation_watcher
       ; watcher_queue
       ; enable_watcher
       ; visited_stores
       ; value_id_observation_definition_positions
       } as down)
    ()
    (t : a Computation.t)
    =
    match t with
    | Sub { via; from = _; into = _; here = _; invert_lifecycles = _ } ->
      let%bind (), free_vars, c = Recurse.on_computation down () `Skipping_over t in
      (* the values bound in a sub are no longer free, so remove them *)
      return ((), Computation_watcher.Type_id_location_map.remove free_vars via, c)
    | Fix_define { fix_id = _; initial_input = _; input_id; result = _; here = _ } ->
      let%bind (), free_vars, c = Recurse.on_computation down () `Skipping_over t in
      (* input_id is no longer free, remove it. Fix_define is the top-level node for
         Fix_recurse *)
      return ((), Computation_watcher.Type_id_location_map.remove free_vars input_id, c)
    | Assoc { map = _; key_comparator = _; key_id; cmp_id = _; data_id; by = _; here = _ }
      ->
      let%bind (), free_vars, c = Recurse.on_computation down () `Skipping_over t in
      (* Assoc_like_key and Assoc_like_data are both Named values, must remove both as
         they are no longer free *)
      let removed_key =
        Computation_watcher.Type_id_location_map.remove free_vars key_id
      in
      let removed_key_and_data =
        Computation_watcher.Type_id_location_map.remove removed_key data_id
      in
      return ((), removed_key_and_data, c)
    | Assoc_on
        { map = _
        ; io_comparator = _
        ; model_comparator = _
        ; io_key_id
        ; io_cmp_id = _
        ; model_key_id = _
        ; model_cmp_id = _
        ; data_id
        ; by = _
        ; get_model_key = _
        ; here = _
        } ->
      let%bind (), free_vars, c = Recurse.on_computation down () `Skipping_over t in
      (* Assoc_like_key and Assoc_like_data are both Named values, must remove both as
         they are no longer free.

         The key that is added to [Environment] is [io_key_id]
      *)
      let removed_key =
        Computation_watcher.Type_id_location_map.remove free_vars io_key_id
      in
      let removed_key_and_data =
        Computation_watcher.Type_id_location_map.remove removed_key data_id
      in
      return ((), removed_key_and_data, c)
    | Leaf0 { model; static_action; apply_action; reset; here } when enable_watcher ->
      let apply_action =
        rewrite_apply_action_leaf0
          ~apply_action
          ~model
          ~action:static_action
          ~watcher_queue
          ~down
          ~source_code_positions:
            (Computation_watcher.Source_code_positions.add_dependency_definition
               source_code_positions
               here)
      in
      let reset =
        rewrite_resetter
          ~reset
          ~model
          ~down
          ~source_code_positions:
            (Computation_watcher.Source_code_positions.add_dependency_definition
               source_code_positions
               here)
          ~kind:`State_machine0
          ~watcher_queue
      in
      return
        ( ()
        , Types.Up.empty
        , Computation.Leaf0 { model; static_action; apply_action; reset; here } )
    | Leaf1 { model; input_id; dynamic_action; apply_action; input; reset; here }
      when enable_watcher ->
      let (), free_vars, input = transform_v down () input in
      let apply_action =
        rewrite_apply_action_leaf1
          ~model
          ~action:dynamic_action
          ~apply_action
          ~down
          ~source_code_positions:
            (Computation_watcher.Source_code_positions.add_dependency_definition
               source_code_positions
               here)
          ~kind:`State_machine1
          ~watcher_queue
      in
      let reset =
        rewrite_resetter
          ~reset
          ~model
          ~down
          ~source_code_positions:
            (Computation_watcher.Source_code_positions.add_dependency_definition
               source_code_positions
               here)
          ~kind:`State_machine1
          ~watcher_queue
      in
      return
        ( ()
        , free_vars
        , Computation.Leaf1
            { model; input_id; dynamic_action; apply_action; input; reset; here } )
    | Wrap
        { wrapper_model
        ; result_id
        ; action_id
        ; inject_id
        ; model_id
        ; inner
        ; dynamic_apply_action
        ; reset
        ; here
        }
      when enable_watcher ->
      let%bind (), free_vars, inner = Recurse.on_computation down () `Directly_on inner in
      (* Wrap_model and Wrap_inject are both Named values, must remove both as they are no
         longer free.
      *)
      let removed_model =
        Computation_watcher.Type_id_location_map.remove free_vars model_id
      in
      let free_vars =
        Computation_watcher.Type_id_location_map.remove removed_model inject_id
      in
      let dynamic_apply_action, reset =
        match enable_watcher with
        | true ->
          let dynamic_apply_action =
            rewrite_apply_action_leaf1
              ~model:wrapper_model
              ~action:action_id
              ~apply_action:dynamic_apply_action
              ~kind:`Wrap
              ~watcher_queue
              ~down
              ~source_code_positions:
                (Computation_watcher.Source_code_positions.add_dependency_definition
                   source_code_positions
                   here)
          in
          let reset =
            rewrite_resetter
              ~reset
              ~model:wrapper_model
              ~down
              ~source_code_positions:
                (Computation_watcher.Source_code_positions.add_dependency_definition
                   source_code_positions
                   here)
              ~kind:`Wrap
              ~watcher_queue
          in
          dynamic_apply_action, reset
        | false -> dynamic_apply_action, reset
      in
      return
        ( ()
        , free_vars
        , Computation.Wrap
            { wrapper_model
            ; result_id
            ; action_id
            ; inject_id
            ; model_id
            ; inner
            ; dynamic_apply_action
            ; reset
            ; here
            } )
    | With_model_resetter { inner; reset_id; here } ->
      let%bind (), free_vars, inner = Recurse.on_computation down () `Directly_on inner in
      return
        ( ()
        , Computation_watcher.Type_id_location_map.remove free_vars reset_id
        , Computation.With_model_resetter { inner; reset_id; here } )
    | Computation_watcher
        { inner
        ; here
        ; free_vars = _
        ; config = inner_config
        ; queue = _
        ; value_id_observation_definition_positions = _
        ; enable_watcher = _
        } ->
      (* [enable_watcher] should only be set to true once we've hit a
         [Computation_watcher] node and [should_run_computation_watcher] is true.
         Redefining here so that both [Down] and [Computation_watcher] receive the proper
         value
      *)
      let enable_watcher = should_run_computation_watcher in
      let config = Computation_watcher.Config.merge config inner_config in
      let%bind (), free_vars, inner =
        Recurse.on_computation
          { Types.Down.source_code_positions =
              Computation_watcher.Source_code_positions.add_watcher
                source_code_positions
                here
                inner_config.label
          ; config
          ; watcher_queue
          ; should_run_computation_watcher
          ; enable_watcher
          ; visited_stores
          ; value_id_observation_definition_positions
          }
          ()
          `Directly_on
          inner
      in
      return
        ( ()
        , free_vars
        , Computation.Computation_watcher
            { inner
            ; here
            ; free_vars
            ; config
            ; queue = Some watcher_queue
            ; value_id_observation_definition_positions =
                Some value_id_observation_definition_positions
            ; enable_watcher
            } )
    | Store { id; value; inner; here } ->
      let%bind (), free_vars, c =
        Recurse.on_computation
          { down with visited_stores = Set.add visited_stores (Type_equal.Id.uid id) }
          ()
          `Directly_on
          inner
      in
      return
        ( ()
        , Computation_watcher.Type_id_location_map.remove free_vars id
        , Computation.Store { id; value; inner = c; here } )
    | Fetch ({ id; default = _; for_some = _; here } as t) ->
      let source_code_positions =
        match
          Set.exists visited_stores ~f:(fun store_id ->
            Type_equal.Id.Uid.equal (Type_equal.Id.uid id) store_id)
        with
        | true ->
          Computation_watcher.Type_id_location_map.singleton
            id
            (Computation_watcher.Source_code_positions.add_dependency_definition
               source_code_positions
               here)
        | false -> Computation_watcher.Type_id_location_map.empty
      in
      return ((), source_code_positions, Computation.Fetch t)
    | _ -> Recurse.on_computation down () `Skipping_over t
  ;;
end

open Fix_transform.Make (Types) (F)

let run ~watcher_queue c =
  let top_config =
    (* These values must all be set to false initially. The fields will switch to [true]
       only when they hit `Computation_watcher` nodes that have configs with [true] values
    *)
    { Computation_watcher.Config.log_model_before = false
    ; log_model_after = false
    ; log_action = false
    ; log_incr_info = false
    ; log_watcher_positions = false
    ; log_dependency_definition_position = false
    ; label = None
    }
  in
  let (), _free_vars, r =
    Trampoline.run
      (transform_c
         { Types.Down.source_code_positions =
             Computation_watcher.Source_code_positions.empty
         ; config = top_config
         ; watcher_queue
           (* `enable_watcher` will be set by the `Computation_watcher` node and will
              propagate downwards from said node onwards *)
         ; enable_watcher =
             false
             (* This flag lets us know that we should set [enable_watcher] once we've
                reached any [Computation_watcher] node *)
         ; should_run_computation_watcher = true
         ; visited_stores = Type_equal.Id.Uid.Set.empty
         ; value_id_observation_definition_positions =
             Computation_watcher.Id_location_hashmap.create ()
         }
         ()
         c)
  in
  r
;;
