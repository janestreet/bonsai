open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

let define
  ~gather
  ~recursive_scopes
  ~time_source
  ~fix_id
  ~initial_input
  ~input_id
  ~result
  ~here:_
  =
  let rec inner_packed =
    lazy
      (let recursive_scopes =
         Computation.Recursive_scopes.add_exn
           recursive_scopes
           ~key:fix_id
           ~data:inner_packed
       in
       Trampoline.run (gather ~recursive_scopes ~time_source result))
  in
  let (T
        { model
        ; input
        ; action
        ; apply_action
        ; run
        ; reset
        ; may_contain = inner_may_contain
        })
    =
    force inner_packed
  in
  let may_contain =
    May_contain.Unresolved.remove_dependency ~on:fix_id inner_may_contain
  in
  let run ~environment ~fix_envs ~path ~model ~inject =
    let resolved = Environment.Recursive.resolve_may_contain fix_envs may_contain in
    let fix_envs =
      Environment.Recursive.add_overwriting
        fix_envs
        ~key:fix_id
        ~data:{ environment; resolved_contain = resolved }
    in
    let initial_input_incr = Value.eval environment initial_input in
    run
      ~environment:
        (Environment.add_exn environment ~key:input_id ~data:initial_input_incr)
      ~fix_envs
      ~path
      ~model
      ~inject
  in
  Trampoline.return
    (Computation.T { model; input; action; apply_action; reset; run; may_contain })
;;

let recurse ~recursive_scopes ~input ~input_id ~fix_id ~here =
  let wrap_lazy ~type_id inject = Action.lazy_ ~type_id >>> inject in
  let model = Meta.Model.Hidden.lazy_ in
  let gathered = Computation.Recursive_scopes.find_exn recursive_scopes fix_id in
  let may_contain = May_contain.Unresolved.recursive fix_id in
  let run ~environment ~fix_envs ~path ~model ~inject =
    let input_incr = Value.eval environment input in
    let ({ environment; resolved_contain = _ } : Environment.Recursive.entry) =
      Environment.Recursive.find_exn fix_envs fix_id
    in
    let (T
          { model = model_info
          ; input = input_info
          ; action = action_info
          ; run
          ; apply_action = _
          ; reset = _
          ; may_contain = _
          })
      =
      force gathered
    in
    let input_model =
      let%map model in
      let (Meta.Model.Hidden.T { model; info; _ }) =
        Option.value
          model
          ~default:(Meta.Model.Hidden.create model_info model_info.default)
      in
      let witness = Meta.Model.Type_id.same_witness_exn info.type_id model_info.type_id in
      Type_equal.conv witness model
    in
    let environment =
      Environment.add_overwriting environment ~key:input_id ~data:input_incr
    in
    let%bind.Trampoline snapshot, () =
      run
        ~environment
        ~fix_envs
        ~path
        ~model:input_model
        ~inject:(wrap_lazy ~type_id:action_info inject)
    in
    let input =
      Input.map (Snapshot.input snapshot) ~f:(fun input ->
        Meta.Input.Hidden.T { input; type_id = input_info; key = () })
    in
    Trampoline.return
      ( Snapshot.create
          ~here
          ~input
          ~result:(Snapshot.result snapshot)
          ~lifecycle:(Snapshot.lifecycle snapshot)
      , () )
  in
  let apply_action
    ~inject
    ~schedule_event
    input
    model
    (Action.Lazy { action; type_id = action_type_id })
    =
    (* forcing the lazy is fine because actions are finite in length *)
    let (T
          { model = model_info
          ; input = input_info
          ; action = action_info
          ; apply_action
          ; run = _
          ; reset = _
          ; may_contain = _
          })
      =
      force gathered
    in
    let (Meta.Model.Hidden.T { model = chosen_model; info = chosen_model_info; _ }) =
      Option.value model ~default:(Meta.Model.Hidden.create model_info model_info.default)
    in
    let T = Action.Type_id.same_witness_exn action_type_id action_info in
    let T =
      Meta.Model.Type_id.same_witness_exn chosen_model_info.type_id model_info.type_id
    in
    let new_model =
      match input with
      | Some (Meta.Input.Hidden.T { input; type_id = input_type_id; key = () }) ->
        let T = Meta.Input.same_witness_exn input_type_id input_info in
        apply_action
          ~inject:(wrap_lazy ~type_id:action_info inject)
          ~schedule_event
          (Some input)
          chosen_model
          action
      | None ->
        apply_action
          ~inject:(wrap_lazy ~type_id:action_info inject)
          ~schedule_event
          None
          chosen_model
          action
    in
    Some (Meta.Model.Hidden.create model_info new_model)
  in
  let reset' ~inject ~schedule_event model =
    let (T
          { model = model_info
          ; action = action_info
          ; reset
          ; apply_action = _
          ; may_contain = _
          ; run = _
          ; input = _
          })
      =
      force gathered
    in
    let (Meta.Model.Hidden.T { model = chosen_model; info = chosen_model_info; _ }) =
      model
    in
    let T =
      Meta.Model.Type_id.same_witness_exn chosen_model_info.type_id model_info.type_id
    in
    let new_model =
      reset ~inject:(wrap_lazy ~type_id:action_info inject) ~schedule_event chosen_model
    in
    Meta.Model.Hidden.create model_info new_model
  in
  let reset ~inject ~schedule_event model =
    (* If the model is None, then you can't descend into the reset because it will force
       the lazy, but that doesn't matter because there's nothing to reset anyway. *)
    match model with
    | None -> None
    | Some model -> Some (reset' ~inject ~schedule_event model)
  in
  Trampoline.return
    (Computation.T
       { model
       ; input = Meta.Input.Hidden.unit
       ; action = Action.Type_id.lazy_
       ; apply_action
       ; run
       ; reset
       ; may_contain
       })
;;
