open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

let f ~gather ~recursive_scopes ~time_source ~inner ~reset_id ~here =
  let%bind.Trampoline (Computation.T
                        ({ model; input; action; apply_action; run; reset; may_contain }
                         as gathered_inner))
    =
    gather ~recursive_scopes ~time_source inner
  in
  let inner_stateless =
    let same_model = Meta.Model.Type_id.same_witness in
    let same_action = Action.Type_id.same_witness in
    let open Option.Let_syntax in
    let%bind a = same_model model.type_id Meta.Model.unit.type_id in
    let%bind b = same_action action Action.Type_id.nothing in
    Some (a, b)
  in
  match inner_stateless with
  | Some (T, T) ->
    let run ~environment:env ~fix_envs ~path ~model:_ ~inject:_ =
      let environment = Environment.add_exn ~key:reset_id ~data:ignore_effect env in
      run ~environment ~fix_envs ~path ~model:unit_model ~inject:unreachable_action
    in
    Trampoline.return (Computation.T { gathered_inner with run })
  | None ->
    let wrap_inner inject = Action.model_reset_inner >>> inject in
    let run ~environment ~fix_envs ~path ~model ~inject =
      let environment =
        environment
        |> Environment.add_exn
             ~key:reset_id
             ~data:(Incr.return (Lazy_inject.make inject Action.model_reset_outer))
      in
      let%bind.Trampoline snapshot, () =
        run ~environment ~fix_envs ~path ~model ~inject:(wrap_inner inject)
      in
      let result = Snapshot.result snapshot in
      Trampoline.return
        ( Snapshot.create
            ~here
            ~result
            ~input:(Snapshot.input snapshot)
            ~lifecycle:(Snapshot.lifecycle snapshot)
        , () )
    in
    let apply_action ~inject ~schedule_event i m = function
      | Action.Model_reset_outer -> reset ~inject:(wrap_inner inject) ~schedule_event m
      | Model_reset_inner inner ->
        apply_action ~inject:(wrap_inner inject) ~schedule_event i m inner
    in
    let reset ~inject ~schedule_event m =
      reset ~inject:(wrap_inner inject) ~schedule_event m
    in
    Trampoline.return
      (Computation.T
         { model
         ; input
         ; action = Action.Type_id.model_reset action
         ; apply_action
         ; run
         ; reset
         ; may_contain
         })
;;
