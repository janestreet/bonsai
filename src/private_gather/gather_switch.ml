open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

let f ~gather ~recursive_scopes ~time_source ~match_ ~arms ~here =
  let wrap_switch ~branch ~type_id inject = Action.switch ~branch ~type_id >>> inject in
  let%bind.Trampoline gathered =
    Trampoline.all_map (Map.map arms ~f:(gather ~recursive_scopes ~time_source))
  in
  let may_contain =
    Map.fold
      gathered
      ~init:(May_contain.Unresolved.non_recursive ~path:No ~lifecycle:No ~input:No)
      ~f:(fun ~key:_ ~data:(Computation.T gathered) acc ->
        May_contain.Unresolved.merge acc gathered.may_contain)
  in
  let run ~environment ~fix_envs ~path ~model ~inject =
    let resolved = Environment.Recursive.resolve_may_contain fix_envs may_contain in
    let path_needs_disambiguation =
      let num_contain_path =
        Map.count gathered ~f:(fun (T { may_contain; _ }) ->
          match Environment.Recursive.resolve_may_contain fix_envs may_contain with
          | { path = Yes_or_maybe; _ } -> true
          | { path = No; _ } -> false)
      in
      num_contain_path > 1
    in
    annotate ~here Switch_model model;
    let index = Value.eval environment match_ in
    let result_input_and_lifecycle =
      let%bind index in
      (* !!!This is a load-bearing bind!!!

         If this bind isn't here, the scope that is created for the bind doesn't exist,
         and old incremental nodes might still be active, and with things like [match%sub]
         or [Bonsai.match_either] can witness old nodes, which can cause [assert false] to
         trigger. *)
      let path =
        if path_needs_disambiguation
        then Path.append path (Path.Elem.Switch index)
        else path
      in
      let (T
            { model = model_info
            ; input = input_info
            ; action = action_info
            ; apply_action = _
            ; reset = _
            ; may_contain = _
            ; run
            })
        =
        Map.find_exn gathered index
      in
      let chosen_model =
        Incremental.map model ~f:(fun map ->
          let (Meta.Model.Hidden.T { model; info }) =
            Meta.Multi_model.find_exn map index
          in
          let equal =
            Meta.Model.Type_id.same_witness_exn info.type_id model_info.type_id
          in
          Type_equal.conv equal model)
      in
      let snapshot, () =
        run
          ~environment
          ~fix_envs
          ~model:chosen_model
          ~path
          ~inject:(wrap_switch ~type_id:action_info ~branch:index inject)
        |> Trampoline.run
      in
      let input =
        let%mapn input = Input.to_incremental (Snapshot.input snapshot) in
        Some (Meta.Input.Hidden.T { input; type_id = input_info; key = index })
      in
      Incr.return
        (Snapshot.result snapshot, input, Snapshot.lifecycle_or_empty ~here snapshot)
    in
    let result = Incr.bind result_input_and_lifecycle ~f:Tuple3.get1
    and input = Incr.bind result_input_and_lifecycle ~f:Tuple3.get2
    and lifecycle = Incr.bind result_input_and_lifecycle ~f:Tuple3.get3 in
    let lifecycle =
      (* if we can prove that none of the switch cases have lifecycle functions, then
         return None, dropping the incremental node on the floor. *)
      match resolved.lifecycle with
      | No -> None
      | Yes_or_maybe -> Some lifecycle
    in
    let input =
      match resolved.input with
      | Yes_or_maybe -> Input.dynamic input
      | No -> Input.static_none
    in
    Trampoline.return (Snapshot.create ~here ~result ~input ~lifecycle, ())
  in
  let apply_action
    ~inject
    ~schedule_event
    input
    model
    (Action.Switch { action; type_id = action_type_id; branch = index })
    =
    let (T
          { model = tm
          ; input = im
          ; action = am
          ; apply_action
          ; run = _
          ; reset = _
          ; may_contain = _
          })
      =
      Map.find_exn gathered index
    in
    let (T { model = chosen_model; info = chosen_model_info; _ }) =
      Meta.Multi_model.find_exn model index
    in
    match
      ( Action.Type_id.same_witness action_type_id am
      , Meta.Model.Type_id.same_witness chosen_model_info.type_id tm.type_id )
    with
    | Some T, Some T ->
      let new_model =
        match Option.join input with
        | Some
            (Meta.Input.Hidden.T
              { input = chosen_input; type_id = chosen_input_info; key = index' }) ->
          (match index = index', Meta.Input.same_witness chosen_input_info im with
           | true, Some T ->
             apply_action
               ~inject:(wrap_switch ~type_id:am ~branch:index inject)
               ~schedule_event
               (Some chosen_input)
               chosen_model
               action
           | _ ->
             apply_action
               ~inject:(wrap_switch ~type_id:am ~branch:index inject)
               ~schedule_event
               None
               chosen_model
               action)
        | None ->
          apply_action
            ~inject:(wrap_switch ~type_id:am ~branch:index inject)
            ~schedule_event
            None
            chosen_model
            action
      in
      let new_model = Meta.Model.Hidden.create tm new_model in
      Meta.Multi_model.set model ~key:index ~data:new_model
    | None, None | Some T, None | None, Some T ->
      let action = Action.Type_id.to_sexp action_type_id action in
      Ui_metrics.Counters.observe Bonsai_switch_action_dropped;
      eprint_s
        [%message
          "an action inside of Bonsai.switch has been dropped because the computation is \
           no longer active"
            (index : int)
            (action : Sexp.t)];
      model
  in
  let reset ~inject ~schedule_event model =
    let f ~key:index ~data:(model : Meta.Model.Hidden.t) =
      let (T { model = chosen_model; info = chosen_model_info; _ }) = model in
      let (T
            { model = tm
            ; input = _
            ; action = am
            ; reset
            ; apply_action = _
            ; may_contain = _
            ; run = _
            })
        =
        Map.find_exn gathered index
      in
      let T = Meta.Model.Type_id.same_witness_exn tm.type_id chosen_model_info.type_id in
      let new_model =
        reset
          ~inject:(wrap_switch ~type_id:am ~branch:index inject)
          ~schedule_event
          chosen_model
      in
      Meta.Model.Hidden.create tm new_model
    in
    Meta.Multi_model.of_models (Map.mapi (Meta.Multi_model.to_models model) ~f)
  in
  let model =
    let models =
      Map.map gathered ~f:(fun (T { model; _ }) ->
        Meta.Model.Hidden.create model model.default)
    in
    Meta.Multi_model.model_info (Meta.Multi_model.of_models models)
  in
  Trampoline.return
    (Computation.T
       { model
       ; input = Meta.Input.Hidden.int
       ; action = Action.Type_id.switch
       ; apply_action
       ; reset
       ; run
       ; may_contain
       })
;;
