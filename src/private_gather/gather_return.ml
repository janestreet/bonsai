open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

let f ~value ~here =
  let run ~environment ~fix_envs:_ ~path:_ ~model:_ ~inject:_ =
    let result = Value.eval environment value in
    Trampoline.return
      (Snapshot.create ~here ~result ~input:Input.static ~lifecycle:None, ())
  in
  Trampoline.return
    (Computation.T
       { model = Meta.Model.unit
       ; input = Meta.Input.unit
       ; action = Action.Type_id.nothing
       ; apply_action = unusable_apply_action
       ; reset = reset_unit_model
       ; run
       ; may_contain =
           May_contain.Unresolved.non_recursive ~path:No ~lifecycle:No ~input:No
       })
;;
