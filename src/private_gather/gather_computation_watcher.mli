open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  gather:'a Computation.gather_fun
  -> enable_watcher:bool
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> inner:'a Computation.t
  -> here:Lexing.position
  -> free_vars:Computation_watcher.Type_id_location_map.t
  -> config:Computation_watcher.Config.t
  -> watcher_queue:Computation_watcher.Output_queue.t option
  -> value_id_observation_definition_positions:
       (Computation_watcher.Source_code_positions.finalized
          Computation_watcher.Source_code_positions.t
       * Computation_watcher.Config.t)
         Computation_watcher.Id_location_hashmap.t
         option
  -> ('a, unit) Computation.packed_info Trampoline.t
