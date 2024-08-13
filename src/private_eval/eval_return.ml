open! Core
open! Import

let f ~value =
  let run ~environment ~fix_envs:_ ~path:_ ~model:_ ~inject:_ =
    let result = Value.eval environment value in
    Trampoline.return (Snapshot.create ~result ~input:Input.static ~lifecycle:None, ())
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
