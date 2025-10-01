open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

let f ~gather ~recursive_scopes ~time_source ~id ~value ~inner =
  let%bind.Trampoline (Computation.T gathered) =
    gather ~recursive_scopes ~time_source inner
  in
  let run ~environment ~fix_envs ~path ~model ~inject =
    let value = Value.eval environment value in
    let environment = Environment.add_overwriting environment ~key:id ~data:value in
    gathered.run ~environment ~fix_envs ~path ~model ~inject
  in
  Trampoline.return
    (Computation.T
       { run
       ; input = gathered.input
       ; model = gathered.model
       ; action = gathered.action
       ; apply_action = gathered.apply_action
       ; reset = gathered.reset
       ; may_contain = gathered.may_contain
       })
;;
