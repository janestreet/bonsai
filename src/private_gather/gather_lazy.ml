open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

let f
  ~(gather : _ Computation.gather_fun)
  ~recursive_scopes
  ~time_source
  ~lazy_computation
  ~here
  =
  let wrap_lazy ~type_id inject = Action.lazy_ ~type_id >>> inject in
  let model = Meta.Model.Hidden.lazy_ in
  let gathered =
    lazy_computation
    |> Lazy.map ~f:(fun c ->
      (Timer.timer ()).time `Gather ~f:(fun () ->
        Trampoline.run (gather ~recursive_scopes ~time_source c)))
  in
  let run ~environment ~fix_envs ~path ~model ~inject =
    let (Computation.T
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
    annotate ~here Model model;
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
          ; run = _
          ; input = _
          ; may_contain = _
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
       ; may_contain = May_contain.Unresolved.lazy_
       })
;;
