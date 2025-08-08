open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

let f ~map ~by ~may_contain ~here =
  let run ~environment ~fix_envs:_ ~path ~model:_ ~inject:_ =
    let map_input = Value.eval environment map in
    let result = Incr_map.mapi map_input ~f:(fun ~key ~data -> by path key data) in
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
       ; may_contain = May_contain.Unresolved.of_resolved may_contain
       })
;;
