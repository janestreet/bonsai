open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

let do_nothing_lifecycle = Incr.return Lifecycle.Collection.empty

let f ~lifecycle ~here =
  let run ~environment ~fix_envs:_ ~path ~model:_ ~inject:_ =
    let lifecycle =
      match%pattern_bind Value.eval environment lifecycle with
      | Some lifecycle ->
        let%map lifecycle in
        Path.Map.singleton path lifecycle
      | None -> do_nothing_lifecycle
    in
    Trampoline.return
      ( Snapshot.create
          ~here
          ~result:(Incr.return ())
          ~input:Input.static
          ~lifecycle:(Some lifecycle)
      , () )
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
           May_contain.Unresolved.non_recursive
             ~path:Yes_or_maybe
             ~lifecycle:Yes_or_maybe
             ~input:No
       })
;;
