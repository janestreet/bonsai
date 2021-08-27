open! Core
open! Import
open Incr.Let_syntax

(* Share this incremental node *)
let unusable_apply_action : (unit, Nothing.t) Snapshot.Apply_action.t =
  Snapshot.Apply_action.non_incremental (fun ~schedule_event:_ () action ->
    Nothing.unreachable_code action)
;;

let do_nothing_lifecycle : Lifecycle.t Path.Map.t Incr.t =
  Incr.return Lifecycle.Collection.empty
;;

let raise_duplicate_path path =
  raise_s
    [%message
      "BUG: [Bonsai.Path.t] should be unique for all components, but duplicate paths \
       were discovered."
        (path : Path.t)]
;;

let merge_lifecycles
  :  Lifecycle.Collection.t Incr.t -> Lifecycle.Collection.t Incr.t
    -> Lifecycle.Collection.t Incr.t
  =
  fun a b ->
  Incr_map.merge a b ~f:(fun ~key -> function
    | `Both _ -> raise_duplicate_path key
    | `Left a -> Some a
    | `Right a -> Some a)
;;

let unzip3_mapi' map ~f =
  let first, second_and_third =
    Incr_map.unzip_mapi' map ~f:(fun ~key ~data ->
      let a, b, c = f ~key ~data in
      let bc = Incr.both b c in
      a, bc)
  in
  let second, third = Incr_map.unzip second_and_third in
  first, second, third
;;

let unit_model = Incr.return ()

let rec eval
  : type model action result.
    environment:Environment.t
    -> path:Path.t
    -> clock:Incr.Clock.t
    -> model:model Incr.t
    -> inject:(action -> unit Effect.t)
    -> (model, action, result) Computation.t
    -> (model, action, result) Snapshot.t
  =
  fun ~environment ~path ~clock ~model ~inject computation ->
  match computation with
  | Return var ->
    let result = Value.eval environment var in
    Snapshot.create ~result ~apply_action:unusable_apply_action ~lifecycle:None
  | Leaf1 { input; apply_action; compute; name = _; kind = _ } ->
    let%pattern_bind result, apply_action =
      let%mapn input = Value.eval environment input
      and model = model in
      compute ~inject input model, apply_action ~inject input
    in
    let apply_action = Snapshot.Apply_action.incremental apply_action in
    Snapshot.create ~result ~apply_action ~lifecycle:None
  | Leaf0 { apply_action; compute; name = _; kind = _ } ->
    let result =
      let%map model = model in
      compute ~inject model
    in
    let apply_action = Snapshot.Apply_action.non_incremental (apply_action ~inject ()) in
    Snapshot.create ~result ~apply_action ~lifecycle:None
  | Leaf_incr { input; apply_action; compute; name = _ } ->
    let input = Value.eval environment input in
    let result = compute ~inject clock input model in
    let apply_action = Snapshot.Apply_action.incremental (apply_action ~inject input) in
    Snapshot.create ~result ~apply_action ~lifecycle:None
  | Model_cutoff { t; model = { Meta.Model.equal; _ } } ->
    let model = Incr.map model ~f:Fn.id in
    Incr.set_cutoff model (Incr.Cutoff.of_equal equal);
    eval ~environment ~path ~clock ~model ~inject t
  | Store { id; value; inner } ->
    let value = Value.eval environment value in
    let environment = Environment.add_overwriting environment ~key:id ~data:value in
    eval ~environment ~path ~clock ~model ~inject inner
  | Fetch id ->
    let result =
      match Environment.find environment id with
      | None -> Incr.return None
      | Some x -> x >>| Option.some
    in
    Snapshot.create ~result ~lifecycle:None ~apply_action:unusable_apply_action
  | Subst { from; via; into; here = _ } ->
    let from =
      let inject e = inject (First e) in
      let model = Incr.map model ~f:Tuple2.get1 in
      let path = Path.append path Path.Elem.Subst_from in
      eval ~environment ~path ~clock ~model ~inject from
    in
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let into =
      let inject e = inject (Second e) in
      let model = Incr.map model ~f:Tuple2.get2 in
      let path = Path.append path Path.Elem.Subst_into in
      eval ~environment ~path ~clock ~model ~inject into
    in
    let apply_action =
      Snapshot.Apply_action.merge
        (Snapshot.apply_action from)
        (Snapshot.apply_action into)
    in
    let result = Snapshot.result into in
    let lifecycle =
      match Snapshot.lifecycle from, Snapshot.lifecycle into with
      | None, None -> None
      | Some l, None | None, Some l -> Some l
      | Some l1, Some l2 -> Some (merge_lifecycles l1 l2)
    in
    Snapshot.create ~result ~apply_action ~lifecycle
  | Subst_stateless { from; via; into; here = _ } ->
    let from =
      let path = Path.append path Path.Elem.Subst_from in
      eval
        ~environment
        ~path
        ~clock
        ~model:unit_model
        ~inject:Nothing.unreachable_code
        from
    in
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let into =
      let path = Path.append path Path.Elem.Subst_into in
      eval ~environment ~path ~clock ~model ~inject into
    in
    let apply_action = Snapshot.apply_action into in
    let result = Snapshot.result into in
    let lifecycle =
      match Snapshot.lifecycle from, Snapshot.lifecycle into with
      | None, None -> None
      | Some l, None | None, Some l -> Some l
      | Some l1, Some l2 -> Some (merge_lifecycles l1 l2)
    in
    Snapshot.create ~result ~apply_action ~lifecycle
  | Assoc
      { map
      ; by
      ; key_compare
      ; key_id
      ; data_id
      ; model_info
      ; action_info
      ; result_by_k = T
      ; input_by_k = T
      ; model_by_k = T
      } ->
    let map_input = Value.eval environment map in
    let input_and_models_map =
      Incr_map.merge map_input model ~f:(fun ~key:_ -> function
        | `Left input -> Some (input, model_info.default)
        | `Right _ -> None
        | `Both input_and_models -> Some input_and_models)
    in
    let create_keyed = unstage (Path.Elem.keyed ~compare:key_compare key_id) in
    let results_map, apply_action_map, lifecycle_map =
      unzip3_mapi' input_and_models_map ~f:(fun ~key ~data:input_and_model ->
        let path = Path.append path Path.Elem.(Assoc (create_keyed key)) in
        let%pattern_bind value, model = input_and_model in
        let environment =
          (* It is safe to reuse the same [key_id] and [data_id] for each pair in the map,
             since they all start with a fresh "copy" of the outer environment. *)
          environment
          |> Environment.add_exn ~key:key_id ~data:(Incr.const key)
          |> Environment.add_exn ~key:data_id ~data:value
        in
        let inject action = inject (key, action) in
        let snapshot = eval ~environment ~path ~clock ~inject ~model by in
        ( Snapshot.result snapshot
        , Snapshot.(Apply_action.to_incremental (apply_action snapshot))
        , Snapshot.lifecycle_or_empty snapshot ))
    in
    let apply_action =
      let%mapn apply_action_map = apply_action_map in
      fun ~schedule_event model action ->
        let id, action = action in
        let specific_model =
          Map.find model id |> Option.value ~default:model_info.default
        in
        match Map.find apply_action_map id with
        | None ->
          let key = Type_equal.Id.to_sexp key_id id in
          let action = Type_equal.Id.to_sexp action_info action in
          eprint_s
            [%message
              "an action inside of Bonsai.assoc as been dropped because the computation \
               is no longer active"
                (key : Sexp.t)
                (action : Sexp.t)];
          model
        (* drop it on the floor *)
        | Some apply_action ->
          let data = apply_action ~schedule_event specific_model action in
          if model_info.equal data model_info.default
          then Map.remove model id
          else Map.set model ~key:id ~data
    in
    let lifecycle =
      Incr_map.unordered_fold_nested_maps
        lifecycle_map
        ~init:Path.Map.empty
        ~add:(fun ~outer_key:_ ~inner_key:key ~data acc ->
          Path.Map.update acc key ~f:(function
            | Some _ -> raise_duplicate_path key
            | None -> data))
        ~remove:(fun ~outer_key:_ ~inner_key:key ~data:_ acc -> Path.Map.remove acc key)
    in
    let apply_action = Snapshot.Apply_action.incremental apply_action in
    Snapshot.create ~result:results_map ~apply_action ~lifecycle:(Some lifecycle)
  | Assoc_simpl
      { map
      ; by
      ; key_id = _
      ; data_id = _
      ; model_info = _
      ; result_by_k = T
      ; input_by_k = T
      ; model_by_k = T
      } ->
    let map_input = Value.eval environment map in
    let result = Incr_map.mapi map_input ~f:(fun ~key ~data -> by path key data) in
    Snapshot.create ~result ~apply_action:unusable_apply_action ~lifecycle:None
  | Enum
      { which; out_of; key_equal; key_type_id; key_compare; key_and_cmp = T; sexp_of_key }
    ->
    let key = Value.eval environment which in
    Incremental.set_cutoff key (Incremental.Cutoff.of_equal key_equal);
    let%pattern_bind result, apply_action, lifecycle =
      let create_keyed = unstage (Path.Elem.keyed ~compare:key_compare key_type_id) in
      let%bind key = key in
      (* !!!This is a load-bearing bind!!!

         If this bind isn't here, the scope that is created for the bind
         doesn't exist, and old incremental nodes might still be active, and
         with things like [match%sub] or [Bonsai.match_either] can witness old
         nodes, which can cause [assert false] to trigger. *)
      let path = Path.append path Path.Elem.(Enum (create_keyed key)) in
      let (T { t; model = model_info; action = action_info }) = Map.find_exn out_of key in
      let chosen_model =
        Incremental.map model ~f:(fun map ->
          let (Hidden.Model.T { model; info; t_of_sexp = _ }) =
            Hidden.Multi_model.find_exn map key
          in
          let equal = Type_equal.Id.same_witness_exn info.type_id model_info.type_id in
          Type_equal.conv equal model)
      in
      let inject action =
        inject (Hidden.Action.T { action; type_id = action_info; key })
      in
      let snapshot = eval ~environment ~model:chosen_model ~path ~clock ~inject t in
      let apply_action =
        let%mapn apply_action =
          Snapshot.(Apply_action.to_incremental (apply_action snapshot))
        in
        fun ~schedule_event
          model
          (Hidden.Action.T { action; type_id = action_type_id; key = key' }) ->
          let (T { model = chosen_model; info = chosen_model_info; _ }) =
            Hidden.Multi_model.find_exn model key
          in
          match
            ( key_equal key' key
            , Type_equal.Id.same_witness action_type_id action_info
            , Type_equal.Id.same_witness chosen_model_info.type_id model_info.type_id )
          with
          | true, Some T, Some T ->
            let new_model = apply_action ~schedule_event chosen_model action in
            let new_model = Hidden.Model.create model_info new_model in
            Hidden.Multi_model.set model ~key ~data:new_model
          | _ ->
            let key = sexp_of_key key in
            let action = Type_equal.Id.to_sexp action_type_id action in
            eprint_s
              [%message
                "an action inside of Bonsai.enum as been dropped because the computation \
                 is no longer active"
                  (key : Sexp.t)
                  (action : Sexp.t)];
            model
      in
      let%mapn result = Snapshot.result snapshot
      and lifecycle = Snapshot.lifecycle_or_empty snapshot
      and apply_action = apply_action in
      result, apply_action, lifecycle
    in
    let apply_action = Snapshot.Apply_action.incremental apply_action in
    Snapshot.create ~apply_action ~result ~lifecycle:(Some lifecycle)
  | Lazy lazy_computation ->
    let (T { t; model = model_info; action = action_info }) = force lazy_computation in
    let input_model =
      let%map model = model in
      let (Hidden.Model.T { model; info; _ }) =
        Option.value model ~default:(Hidden.Model.create model_info model_info.default)
      in
      let witness = Type_equal.Id.same_witness_exn info.type_id model_info.type_id in
      Type_equal.conv witness model
    in
    let inject action =
      inject (Hidden.Action.T { action; type_id = action_info; key = () })
    in
    let snapshot = eval ~environment ~path ~clock ~model:input_model ~inject t in
    let apply_action =
      Snapshot.Apply_action.map
        (Snapshot.apply_action snapshot)
        ~f:(fun apply_action ~schedule_event model action ->
          let (Hidden.Action.T { action; type_id = action_type_id; key = () }) = action in
          let (Hidden.Model.T { model = chosen_model; info = chosen_model_info; _ }) =
            Option.value
              model
              ~default:(Hidden.Model.create model_info model_info.default)
          in
          match
            ( Type_equal.Id.same_witness action_type_id action_info
            , Type_equal.Id.same_witness chosen_model_info.type_id model_info.type_id )
          with
          | Some T, Some T ->
            let new_model = apply_action ~schedule_event chosen_model action in
            Some (Hidden.Model.create model_info new_model)
          | _ ->
            print_s [%message "BUG: type-id mismatch in Bonsai.lazy_"];
            model)
    in
    Snapshot.create
      ~apply_action
      ~result:(Snapshot.result snapshot)
      ~lifecycle:(Snapshot.lifecycle snapshot)
  | Wrap { model_id; inject_id; inner; apply_action } ->
    let%pattern_bind outer_model, inner_model = model in
    let inject_outer a = inject (Either.First a) in
    let inject_inner a = inject (Either.Second a) in
    let inner_snapshot =
      let environment =
        environment
        |> Environment.add_exn ~key:model_id ~data:outer_model
        |> Environment.add_exn ~key:inject_id ~data:(Incr.return inject_outer)
      in
      eval ~environment ~path ~model:inner_model ~clock ~inject:inject_inner inner
    in
    let inner_result = Snapshot.result inner_snapshot in
    let apply_action =
      let%mapn inner_result = inner_result
      and inner_apply_action =
        Snapshot.(Apply_action.to_incremental (apply_action inner_snapshot))
      in
      fun ~schedule_event (outer_model, inner_model) action ->
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
          let new_inner_model = inner_apply_action ~schedule_event inner_model action2 in
          outer_model, new_inner_model
    in
    Snapshot.create
      ~result:inner_result
      ~apply_action:(Snapshot.Apply_action.incremental apply_action)
      ~lifecycle:(Snapshot.lifecycle inner_snapshot)
  | With_model_resetter { t; default_model } ->
    let reset_event = inject (First ()) in
    let inject a = inject (Second a) in
    let snapshot = eval ~environment ~path ~model ~clock ~inject t in
    let apply_action =
      Snapshot.Apply_action.map
        (Snapshot.apply_action snapshot)
        ~f:(fun apply_action ~schedule_event model action ->
          match action with
          | First () -> default_model
          | Second a -> apply_action ~schedule_event model a)
    in
    let result =
      let%map result = Snapshot.result snapshot in
      result, reset_event
    in
    Snapshot.create ~result ~apply_action ~lifecycle:(Snapshot.lifecycle snapshot)
  | Path ->
    Snapshot.create
      ~result:(Incr.return path)
      ~apply_action:unusable_apply_action
      ~lifecycle:None
  | Lifecycle lifecycle ->
    let lifecycle =
      match%pattern_bind Value.eval environment lifecycle with
      | Some lifecycle ->
        let%map lifecycle = lifecycle in
        Path.Map.singleton path lifecycle
      | None -> do_nothing_lifecycle
    in
    Snapshot.create
      ~result:(Incr.return ())
      ~apply_action:unusable_apply_action
      ~lifecycle:(Some lifecycle)
;;
