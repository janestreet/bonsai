open! Core_kernel
open! Import
open Incr.Let_syntax

let rec eval
  : type model action result.
    Environment.t
    -> model Incr.t
    -> inject:(action -> Event.t)
    -> (model, action, result) Computation.t
    -> (model, action, result) Snapshot.t Incr.t
  =
  fun environment model ~inject computation ->
  match computation with
  | Return var ->
    let%map result = Value.eval environment var in
    Snapshot.create ~result ~apply_action:(fun ~schedule_event:_ action ->
      Nothing.unreachable_code action)
  | Leaf { input; apply_action; compute; name = _ } ->
    let%mapn input = Value.eval environment input
    and model = model in
    let result = compute ~inject input model in
    let apply_action = apply_action ~inject input model in
    Snapshot.create ~result ~apply_action
  | Leaf_incr { input; apply_action; compute; name = _ } ->
    let input = Value.eval environment input in
    let%mapn result = compute ~inject input model
    and apply_action = apply_action ~inject input model in
    Snapshot.create ~result ~apply_action
  | Model_cutoff { t; model = { Meta.Model.equal; _ } } ->
    let model = Incr.map model ~f:Fn.id in
    Incr.set_cutoff model (Incr.Cutoff.of_equal equal);
    eval environment model ~inject t
  | Subst { from; via; into } ->
    let from =
      let inject e = inject (First e) in
      let model = Incr.map model ~f:Tuple2.get1 in
      eval environment model ~inject from
    in
    let from_result = Incr.map from ~f:Snapshot.result in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let into =
      let inject e = inject (Second e) in
      let model = Incr.map model ~f:Tuple2.get2 in
      eval environment model ~inject into
    in
    let%mapn apply_action =
      let%mapn from = from
      and into = into
      and m1, m2 = model in
      let apply_action_from = Snapshot.apply_action from in
      let apply_action_into = Snapshot.apply_action into in
      fun ~schedule_event action ->
        match action with
        | First action1 -> apply_action_from action1 ~schedule_event, m2
        | Second action2 -> m1, apply_action_into action2 ~schedule_event
    and result =
      let%map into = into in
      Snapshot.result into
    in
    Snapshot.create ~result ~apply_action
  | Assoc
      { map
      ; by
      ; key_id
      ; data_id
      ; model_info
      ; result_by_k = T
      ; input_by_k = T
      ; model_by_k = T
      } ->
    let map_input = Value.eval environment map in
    let input_and_models_map =
      Incr_map.merge map_input model ~f:(fun ~key:_ ->
        function
        | `Left input -> Some (input, model_info.default)
        | `Right _ -> None
        | `Both input_and_models -> Some input_and_models)
    in
    let snapshot_map =
      Incr_map.mapi' input_and_models_map ~f:(fun ~key ~data:input_and_models ->
        let%pattern_bind value, model = input_and_models in
        let environment =
          (* It is safe to reuse the same [key_id] and [data_id] for each pair in the map,
             since they all start with a fresh "copy" of the outer environment. *)
          environment
          |> Environment.add_exn ~key:key_id ~data:(Incr.const key)
          |> Environment.add_exn ~key:data_id ~data:value
        in
        let inject action = inject (key, action) in
        eval environment ~inject model by)
    in
    let results_map =
      Incr_map.mapi snapshot_map ~f:(fun ~key:_ ~data:snapshot ->
        Snapshot.result snapshot)
    in
    let apply_action =
      let%mapn snapshot_map = snapshot_map
      and model = model in
      fun ~schedule_event action ->
        let id, action = action in
        match Map.find snapshot_map id with
        | None -> model
        (* drop it on the floor *)
        | Some snapshot ->
          let data = Snapshot.apply_action snapshot ~schedule_event action in
          Map.set model ~key:id ~data
    in
    let%mapn apply_action = apply_action
    and result = results_map in
    Snapshot.create ~result ~apply_action
  | Enum { which; out_of; key_equal; key_and_cmp = T; sexp_of_key = _ } ->
    let key = Value.eval environment which in
    Incremental.set_cutoff key (Incremental.Cutoff.of_equal key_equal);
    let%bind key = key in
    let (T { t; model = model_info; action = action_info }) = Map.find_exn out_of key in
    let chosen_model =
      Incremental.map model ~f:(fun map ->
        let (Enum_types.Case_model.T { model; info; t_of_sexp = _ }) =
          Enum_types.Multi_model.find_exn map key
        in
        let equal = Type_equal.Id.same_witness_exn info.type_id model_info.type_id in
        Type_equal.conv equal model)
    in
    let inject action =
      inject (Enum_types.Case_action.T { action; type_id = action_info; key })
    in
    let%mapn snapshot = eval environment chosen_model ~inject t
    and model = model in
    let apply_action
          ~schedule_event
          (Enum_types.Case_action.T { action; type_id; key = key' })
      =
      match key_equal key' key, Type_equal.Id.same_witness type_id action_info with
      | true, Some T ->
        let new_model = Snapshot.apply_action snapshot ~schedule_event action in
        let new_model = Enum_types.Case_model.create model_info new_model in
        Enum_types.Multi_model.set model ~key ~data:new_model
      | _ -> model
    in
    Snapshot.create ~apply_action ~result:(Snapshot.result snapshot)
  | Wrap { model_id; inject_id; inner; apply_action } ->
    let%pattern_bind outer_model, inner_model = model in
    let inject_outer a = inject (Either.First a) in
    let inject_inner a = inject (Either.Second a) in
    let%mapn outer_model = outer_model
    and inner_model = inner_model
    and inner_snapshot =
      let environment =
        environment
        |> Environment.add_exn ~key:model_id ~data:outer_model
        |> Environment.add_exn ~key:inject_id ~data:(Incr.return inject_outer)
      in
      eval environment inner_model ~inject:inject_inner inner
    in
    let inner_result = Snapshot.result inner_snapshot in
    Snapshot.create ~result:inner_result ~apply_action:(fun ~schedule_event action ->
      match action with
      | First action1 ->
        let new_outer_model =
          apply_action
            ~inject:inject_outer
            ~schedule_event
            inner_result
            outer_model
            action1
        in
        new_outer_model, inner_model
      | Second action2 ->
        let new_inner_model =
          Snapshot.apply_action inner_snapshot ~schedule_event action2
        in
        outer_model, new_inner_model)
;;
