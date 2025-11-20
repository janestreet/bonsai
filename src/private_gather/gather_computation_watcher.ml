open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

(* extends the environment with a new version of the incremental identified by [id] with
   an incremental that enqueues relevant info to the watcher queue whenever it updates *)
let enqueue_value
  ~source_code_positions
  ~watcher_queue
  ~config
  ~value_id_observation_definition_positions
  environment
  id
  =
  match Environment.find environment id with
  | None ->
    (* not being able to find this value is probably a bug, but let's not crash inside
       this debugging utility. *)
    eprint_s [%message "BUG" [%here] "value not found in environment"];
    environment
  | Some value ->
    let uid = Type_equal.Id.uid id in
    let has_been_set =
      Computation_watcher.Id_location_hashmap.update_and_check_if_value_set
        ~id:(`Named uid)
        ~update_data:(source_code_positions, config)
        value_id_observation_definition_positions
    in
    (match has_been_set with
     | `Already_set -> environment
     | `Not_set ->
       let loud_value =
         Computation_watcher.instrument_incremental_node
           ~here:[%here]
           ~id:(`Named uid)
           ~watcher_queue
           ~value_id_observation_definition_positions
           value
       in
       Environment.add_overwriting environment ~key:id ~data:loud_value)
;;

let f
  (type a)
  ~(gather : a Computation.gather_fun)
  ~enable_watcher
  ~recursive_scopes
  ~time_source
  ~inner
  ~here:_
  ~free_vars
  ~config
  ~watcher_queue
  ~value_id_observation_definition_positions
  =
  match
    ( `Enable_watcher enable_watcher
    , `Queue watcher_queue
    , `Positions value_id_observation_definition_positions )
  with
  | `Enable_watcher false, _, _ ->
    (* If watcher isn't enabled, we shouldn't be printing anything *)
    let%bind.Trampoline (T inner) = gather ~recursive_scopes ~time_source inner in
    Trampoline.return (Computation.T inner)
  | `Enable_watcher true, `Queue None, `Positions None
  | `Enable_watcher true, `Queue None, `Positions (Some _)
  | `Enable_watcher true, `Queue (Some _), `Positions None ->
    (* If watcher is enabled and we are missing one or both of [watcher_queue] and
       [value_id_observation_definition_positions], something's gone wrong and we need to
       raise
    *)
    Core.raise_s
      [%message
        "BUG" [%here] "watcher queue or value_id_observation_definition_positions is none"]
  | ( `Enable_watcher true
    , `Queue (Some watcher_queue)
    , `Positions (Some value_id_observation_definition_positions) ) ->
    let%bind.Trampoline (T inner) = gather ~recursive_scopes ~time_source inner in
    let run ~environment ~fix_envs ~path ~model ~inject =
      let environment =
        Computation_watcher.Type_id_location_map.fold
          free_vars
          ~init:environment
          { f =
              (fun env
                id
                (Computation_watcher.Source_code_positions.Finalized
                  source_code_positions) ->
                enqueue_value
                  ~source_code_positions
                  ~watcher_queue
                  ~config
                  ~value_id_observation_definition_positions
                  env
                  id)
          }
      in
      inner.run ~environment ~fix_envs ~path ~model ~inject
    in
    Trampoline.return (Computation.T { inner with run })
;;
