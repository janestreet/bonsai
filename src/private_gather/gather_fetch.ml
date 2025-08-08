open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

let f ~id ~default ~for_some ~here =
  let run ~environment ~fix_envs:_ ~path:_ ~model:_ ~inject:_ =
    let result =
      match Environment.find environment id with
      | None -> Incr.return default
      | Some x -> Incr.map x ~f:(fun a -> for_some a)
    in
    Trampoline.return
      (Snapshot.create ~here ~result ~lifecycle:None ~input:Input.static, ())
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
