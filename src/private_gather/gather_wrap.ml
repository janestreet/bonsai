open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

let f
  ~(gather : _ Computation.gather_fun)
  ~recursive_scopes
  ~time_source
  ~wrapper_model
  ~action_id
  ~result_id
  ~inject_id
  ~model_id
  ~inner
  ~dynamic_apply_action
  ~reset:reset_me
  ~here
  =
  let%bind.Trampoline (Computation.T
                        { model = inner_model
                        ; input = inner_input
                        ; action = inner_action
                        ; apply_action
                        ; run
                        ; reset
                        ; may_contain
                        })
    =
    gather ~recursive_scopes ~time_source inner
  in
  let wrap_inner inject = Action.wrap_inner >>> inject in
  let wrap_outer inject = Action.wrap_outer >>> inject in
  let run ~environment ~fix_envs ~path ~model ~inject =
    annotate ~here Model model;
    let%pattern_bind outer_model, inner_model = model in
    annotate ~here Model outer_model;
    let%bind.Trampoline inner_snapshot, () =
      let outer_inject = Lazy_inject.make (wrap_outer inject) in
      let environment =
        environment
        |> Environment.add_exn ~key:model_id ~data:outer_model
        |> Environment.add_exn ~key:inject_id ~data:(Incr.return outer_inject)
      in
      run ~environment ~fix_envs ~path ~model:inner_model ~inject:(wrap_inner inject)
    in
    let inner_result = Snapshot.result inner_snapshot in
    let input =
      Input.merge
        (Snapshot.input inner_snapshot)
        (Input.dynamic (Snapshot.result inner_snapshot))
    in
    Trampoline.return
      ( Snapshot.create
          ~here
          ~result:inner_result
          ~input
          ~lifecycle:(Snapshot.lifecycle inner_snapshot)
      , () )
  in
  let model = Meta.Model.both wrapper_model inner_model in
  let apply_action ~inject ~schedule_event input (outer_model, inner_model) action =
    match action with
    | Action.Wrap_outer action_outer ->
      let inject = Lazy_inject.make (wrap_outer inject) in
      let new_outer_model =
        dynamic_apply_action
          ~inject
          ~schedule_event
          ~time_source
          (Option.map input ~f:snd)
          outer_model
          action_outer
      in
      new_outer_model, inner_model
    | Wrap_inner action_inner ->
      let new_inner_model =
        apply_action
          ~inject:(wrap_inner inject)
          ~schedule_event
          (Option.map input ~f:fst)
          inner_model
          action_inner
      in
      outer_model, new_inner_model
  in
  let reset ~inject ~schedule_event (outer_model, inner_model) =
    let outer_model =
      reset_me ~inject:(wrap_outer inject) ~schedule_event ~time_source outer_model
    in
    let inner_model = reset ~inject:(wrap_inner inject) ~schedule_event inner_model in
    outer_model, inner_model
  in
  let may_contain =
    let for_wrap_node =
      May_contain.Unresolved.non_recursive ~path:No ~lifecycle:No ~input:Yes_or_maybe
    in
    May_contain.Unresolved.merge may_contain for_wrap_node
  in
  Trampoline.return
    (Computation.T
       { model
       ; input = Meta.Input.both inner_input result_id
       ; action = Action.Type_id.wrap ~inner:inner_action ~outer:action_id
       ; apply_action
       ; run
       ; reset
       ; may_contain
       })
;;
