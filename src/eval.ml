open! Core
open! Import
open Incr.Let_syntax

let unusable_static_apply_action
      ~inject_dynamic:_
      ~inject_static:_
      ~schedule_event:_
      _model
  =
  Nothing.unreachable_code
;;

let () = Incr.State.(set_max_height_allowed t 1024)

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
      annotate Lifecycle_apply_action_pair bc;
      a, bc)
  in
  let second, third = Incr_map.unzip second_and_third in
  first, second, third
;;

let unit_model = Incr.return ()

let rec gather_impl
  : type result. result Computation.kind -> result Computation.packed_info
  =
  let open Computation in
  function
  | Return value ->
    let run ~environment ~path:_ ~clock:_ ~model:_ ~inject_dynamic:_ ~inject_static:_ =
      let result = Value.eval environment value in
      Snapshot.create ~result ~apply_action:Apply_action.impossible ~lifecycle:None
    in
    T
      { model = Meta.Model.unit
      ; dynamic_action = Meta.Action.nothing
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; run
      }
  | Leaf01 { model; dynamic_action; static_action; apply_dynamic; apply_static; input } ->
    let run ~environment ~path:_ ~clock:_ ~model ~inject_dynamic ~inject_static =
      let input = Value.eval environment input in
      let result =
        let%mapn model = model in
        model, inject_dynamic, inject_static
      in
      let apply_action =
        let%mapn input = input in
        apply_dynamic ~inject_dynamic ~inject_static input
      in
      let apply_action = Apply_action.incremental apply_action in
      Snapshot.create ~result ~apply_action ~lifecycle:None
    in
    T { model; dynamic_action; static_action; apply_static; run }
  | Leaf1 { model; dynamic_action; apply_action; input } ->
    let run ~environment ~path:_ ~clock:_ ~model ~inject_dynamic ~inject_static:_ =
      let input = Value.eval environment input in
      let result =
        let%mapn model = model in
        model, inject_dynamic
      in
      let apply_action =
        let%mapn input = input in
        apply_action ~inject_dynamic ~inject_static:Nothing.unreachable_code input
      in
      let apply_action = Apply_action.incremental apply_action in
      Snapshot.create ~result ~apply_action ~lifecycle:None
    in
    T
      { model
      ; dynamic_action
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; run
      }
  | Leaf0 { model; static_action; apply_action; compute } ->
    let run ~environment:_ ~path:_ ~clock:_ ~model ~inject_dynamic:_ ~inject_static =
      let result =
        let%map model = model in
        compute ~inject:inject_static model
      in
      Snapshot.create ~result ~apply_action:Apply_action.impossible ~lifecycle:None
    in
    T
      { model
      ; dynamic_action = Meta.Action.nothing
      ; static_action
      ; apply_static = apply_action
      ; run
      }
  | Leaf_incr { model; dynamic_action; input; apply_dynamic; compute } ->
    let run ~environment ~path:_ ~clock ~model ~inject_dynamic ~inject_static:_ =
      let input = Value.eval environment input in
      let result = compute ~inject:inject_dynamic clock input model in
      let apply_action =
        Apply_action.incremental (apply_dynamic ~inject:inject_dynamic input)
      in
      Snapshot.create ~result ~apply_action ~lifecycle:None
    in
    T
      { model
      ; dynamic_action
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; run
      }
  | Model_cutoff t ->
    let (T gathered) = gather t in
    let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
      let model = Incr.map model ~f:Fn.id in
      Incr.set_cutoff model (Incr.Cutoff.of_equal gathered.model.equal);
      gathered.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
    in
    T
      { run
      ; model = gathered.model
      ; static_action = gathered.static_action
      ; dynamic_action = gathered.dynamic_action
      ; apply_static = gathered.apply_static
      }
  | Sub { from; via; into; here } ->
    let (T info_from) = gather from in
    let (T info_into) = gather into in
    (match
       ( ( Meta.Model.Type_id.same_witness info_from.model.type_id Meta.Model.unit.type_id
         , Meta.Action.Type_id.same_witness info_from.dynamic_action Meta.Action.nothing
         , Meta.Action.Type_id.same_witness info_from.static_action Meta.Action.nothing )
       , ( Meta.Model.Type_id.same_witness info_into.model.type_id Meta.Model.unit.type_id
         , Meta.Action.Type_id.same_witness info_into.dynamic_action Meta.Action.nothing
         , Meta.Action.Type_id.same_witness info_into.static_action Meta.Action.nothing )
       )
     with
     | (Some T, Some T, Some T), _ ->
       let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
         let from =
           let path = Path.append path Path.Elem.Subst_from in
           info_from.run
             ~environment
             ~path
             ~clock
             ~model:unit_model
             ~inject_dynamic:Nothing.unreachable_code
             ~inject_static:Nothing.unreachable_code
         in
         Snapshot.attribute_positions here from;
         let from_result = Snapshot.result from in
         let environment = Environment.add_exn environment ~key:via ~data:from_result in
         let into =
           let path = Path.append path Path.Elem.Subst_into in
           info_into.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
         in
         let apply_action = Snapshot.apply_action into in
         let result = Snapshot.result into in
         let lifecycle =
           Option.merge
             (Snapshot.lifecycle from)
             (Snapshot.lifecycle into)
             ~f:merge_lifecycles
         in
         Snapshot.create ~result ~apply_action ~lifecycle
       in
       T
         { run
         ; model = info_into.model
         ; dynamic_action = info_into.dynamic_action
         ; static_action = info_into.static_action
         ; apply_static = info_into.apply_static
         }
     | _, (Some T, Some T, Some T) ->
       let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
         let from =
           let path = Path.append path Path.Elem.Subst_from in
           info_from.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
         in
         Snapshot.attribute_positions here from;
         let from_result = Snapshot.result from in
         let environment = Environment.add_exn environment ~key:via ~data:from_result in
         let into =
           let path = Path.append path Path.Elem.Subst_into in
           info_into.run
             ~environment
             ~path
             ~clock
             ~model:unit_model
             ~inject_dynamic:Nothing.unreachable_code
             ~inject_static:Nothing.unreachable_code
         in
         let apply_action = Snapshot.apply_action from in
         let result = Snapshot.result into in
         let lifecycle =
           Option.merge
             (Snapshot.lifecycle from)
             (Snapshot.lifecycle into)
             ~f:merge_lifecycles
         in
         Snapshot.create ~result ~apply_action ~lifecycle
       in
       T
         { run
         ; model = info_from.model
         ; dynamic_action = info_from.dynamic_action
         ; static_action = info_from.static_action
         ; apply_static = info_from.apply_static
         }
     | _ ->
       let apply_static
             ~inject_dynamic
             ~inject_static
             ~schedule_event
             (model_from, model_into)
         = function
           | First action ->
             let inject_static action = inject_static (First action) in
             let inject_dynamic action = inject_dynamic (First action) in
             let model_from =
               info_from.apply_static
                 ~inject_dynamic
                 ~inject_static
                 ~schedule_event
                 model_from
                 action
             in
             model_from, model_into
           | Second action ->
             let inject_static action = inject_static (Second action) in
             let inject_dynamic action = inject_dynamic (Second action) in
             let model_into =
               info_into.apply_static
                 ~inject_dynamic
                 ~inject_static
                 ~schedule_event
                 model_into
                 action
             in
             model_from, model_into
       in
       let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
         let from =
           let inject_dynamic effect = inject_dynamic (First effect) in
           let inject_static effect = inject_static (First effect) in
           let model = Incr.map model ~f:Tuple2.get1 in
           let path = Path.append path Path.Elem.Subst_from in
           info_from.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
         in
         Snapshot.attribute_positions here from;
         let from_result = Snapshot.result from in
         let environment = Environment.add_exn environment ~key:via ~data:from_result in
         let into =
           let inject_dynamic effect = inject_dynamic (Second effect) in
           let inject_static effect = inject_static (Second effect) in
           let model = Incr.map model ~f:Tuple2.get2 in
           let path = Path.append path Path.Elem.Subst_into in
           info_into.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
         in
         let apply_action =
           Apply_action.merge (Snapshot.apply_action from) (Snapshot.apply_action into)
         in
         let result = Snapshot.result into in
         let lifecycle =
           Option.merge
             (Snapshot.lifecycle from)
             (Snapshot.lifecycle into)
             ~f:merge_lifecycles
         in
         Snapshot.create ~result ~apply_action ~lifecycle
       in
       let model = Meta.Model.both info_from.model info_into.model in
       let dynamic_action =
         Meta.Action.both info_from.dynamic_action info_into.dynamic_action
       in
       let static_action =
         Meta.Action.both info_from.static_action info_into.static_action
       in
       T { model; dynamic_action; static_action; apply_static; run })
  | Store { id; value; inner } ->
    let (T gathered) = gather inner in
    let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
      let value = Value.eval environment value in
      let environment = Environment.add_overwriting environment ~key:id ~data:value in
      gathered.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
    in
    T
      { run
      ; model = gathered.model
      ; static_action = gathered.static_action
      ; dynamic_action = gathered.dynamic_action
      ; apply_static = gathered.apply_static
      }
  | Fetch { id; default; for_some } ->
    let run ~environment ~path:_ ~clock:_ ~model:_ ~inject_dynamic:_ ~inject_static:_ =
      let result =
        match Environment.find environment id with
        | None -> Incr.return default
        | Some x -> Incr.map x ~f:(fun a -> for_some a)
      in
      Snapshot.create ~result ~lifecycle:None ~apply_action:Apply_action.impossible
    in
    T
      { model = Meta.Model.unit
      ; dynamic_action = Meta.Action.nothing
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; run
      }
  | Assoc { map; key_comparator; key_id; cmp_id; data_id; by } ->
    let (T { model = model_info; dynamic_action; static_action; apply_static; run }) =
      gather by
    in
    let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
      let map_input = Value.eval environment map in
      let input_and_models_map =
        Incr_map.merge map_input model ~f:(fun ~key:_ -> function
          | `Left input -> Some (input, model_info.default)
          | `Right _ -> None
          | `Both input_and_models -> Some input_and_models)
      in
      let module Cmp = (val key_comparator) in
      let create_keyed =
        unstage (Path.Elem.keyed ~compare:Cmp.comparator.compare key_id)
      in
      let results_map, apply_action_map, lifecycle_map =
        unzip3_mapi' input_and_models_map ~f:(fun ~key ~data:input_and_model ->
          annotate Model_and_input input_and_model;
          let path = Path.append path Path.Elem.(Assoc (create_keyed key)) in
          let%pattern_bind value, model = input_and_model in
          let key_incr = Incr.const key in
          annotate Assoc_key key_incr;
          annotate Assoc_input value;
          let environment =
            (* It is safe to reuse the same [key_id] and [data_id] for each pair in the map,
               since they all start with a fresh "copy" of the outer environment. *)
            environment
            |> Environment.add_exn ~key:key_id ~data:key_incr
            |> Environment.add_exn ~key:data_id ~data:value
          in
          let inject_dynamic action = inject_dynamic (key, action) in
          let inject_static action = inject_static (key, action) in
          let snapshot =
            run ~environment ~path ~clock ~inject_dynamic ~inject_static ~model
          in
          ( Snapshot.result snapshot
          , Snapshot.(Apply_action.to_incremental (apply_action snapshot))
          , Snapshot.lifecycle_or_empty snapshot ))
      in
      annotate Assoc_results results_map;
      annotate Assoc_lifecycles lifecycle_map;
      annotate Assoc_apply_actions apply_action_map;
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
            let action = Meta.Action.Type_id.to_sexp dynamic_action action in
            eprint_s
              [%message
                "an action inside of Bonsai.assoc has been dropped because the \
                 computation is no longer active"
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
            Map.update acc key ~f:(function
              | Some _ -> raise_duplicate_path key
              | None -> data))
          ~remove:(fun ~outer_key:_ ~inner_key:key ~data:_ acc -> Map.remove acc key)
      in
      annotate Assoc_lifecycles lifecycle;
      let apply_action = Apply_action.incremental apply_action in
      Snapshot.create ~result:results_map ~apply_action ~lifecycle:(Some lifecycle)
    in
    let apply_static ~inject_dynamic ~inject_static ~schedule_event model (id, action) =
      let inject_dynamic a = inject_dynamic (id, a) in
      let inject_static a = inject_static (id, a) in
      let specific_model =
        Map.find model id |> Option.value ~default:model_info.default
      in
      let data =
        apply_static ~inject_dynamic ~inject_static ~schedule_event specific_model action
      in
      if model_info.equal data model_info.default
      then Map.remove model id
      else Map.set model ~key:id ~data
    in
    T
      { model = Meta.Model.map key_comparator key_id cmp_id model_info
      ; dynamic_action = Meta.Action.map key_id dynamic_action
      ; static_action = Meta.Action.map key_id static_action
      ; apply_static
      ; run
      }
  | Assoc_on
      { map
      ; io_comparator
      ; model_comparator
      ; io_key_id
      ; model_key_id
      ; model_cmp_id
      ; data_id
      ; by
      ; get_model_key
      } ->
    let module Model_comparator = (val model_comparator) in
    let module Io_comparator = (val io_comparator) in
    let model_key_comparator = Model_comparator.comparator in
    let (T { model = model_info; dynamic_action; static_action; apply_static; run }) =
      gather by
    in
    let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
      let map_input = Value.eval environment map in
      let model_lookup = Incr_map.Lookup.create model ~comparator:model_key_comparator in
      let create_keyed =
        unstage (Path.Elem.keyed ~compare:Io_comparator.comparator.compare io_key_id)
      in
      let results_map, apply_action_map, lifecycle_map =
        unzip3_mapi' map_input ~f:(fun ~key ~data:value ->
          let%pattern_bind results_map, apply_action_map, lifecycle_map =
            let path = Path.append path Path.Elem.(Assoc (create_keyed key)) in
            let key_incr = Incr.const key in
            annotate Assoc_key key_incr;
            annotate Assoc_input value;
            let environment =
              (* It is safe to reuse the same [key_id] and [data_id] for each pair in the map,
                 since they all start with a fresh "copy" of the outer environment. *)
              environment
              |> Environment.add_exn ~key:io_key_id ~data:key_incr
              |> Environment.add_exn ~key:data_id ~data:value
            in
            let model_key =
              let%map value = value in
              get_model_key key value
            in
            Incr.set_cutoff
              model_key
              (Incr.Cutoff.of_compare model_key_comparator.compare);
            let%bind model_key = model_key in
            let inject_dynamic action = inject_dynamic (key, model_key, action) in
            let inject_static action = inject_static (key, model_key, action) in
            let model =
              let%map model = Incr_map.Lookup.find model_lookup model_key in
              Option.value model ~default:model_info.default
            in
            let snapshot =
              run ~environment ~path ~clock ~inject_dynamic ~inject_static ~model
            in
            let%mapn result = Snapshot.result snapshot
            and apply_action =
              Snapshot.(Apply_action.to_incremental (apply_action snapshot))
            and lifecycle = Snapshot.lifecycle_or_empty snapshot in
            result, apply_action, lifecycle
          in
          results_map, apply_action_map, lifecycle_map)
      in
      annotate Assoc_results results_map;
      annotate Assoc_lifecycles lifecycle_map;
      annotate Assoc_apply_actions apply_action_map;
      let apply_action =
        let%mapn apply_action_map = apply_action_map in
        fun ~schedule_event model action ->
          let input_id, model_id, action = action in
          let specific_model =
            Map.find model model_id |> Option.value ~default:model_info.default
          in
          match Map.find apply_action_map input_id with
          | None ->
            let io_key = Type_equal.Id.to_sexp io_key_id input_id in
            let model_key = model_key_comparator.sexp_of_t model_id in
            let action = Meta.Action.Type_id.to_sexp dynamic_action action in
            eprint_s
              [%message
                "an action inside of Bonsai.assoc_on has been dropped because the \
                 computation is no longer active"
                  (io_key : Sexp.t)
                  (model_key : Sexp.t)
                  (action : Sexp.t)];
            model
          (* drop it on the floor *)
          | Some apply_action ->
            let data = apply_action ~schedule_event specific_model action in
            if model_info.equal data model_info.default
            then Map.remove model model_id
            else Map.set model ~key:model_id ~data
      in
      let lifecycle =
        Incr_map.unordered_fold_nested_maps
          lifecycle_map
          ~init:Path.Map.empty
          ~add:(fun ~outer_key:_ ~inner_key:key ~data acc ->
            Map.update acc key ~f:(function
              | Some _ -> raise_duplicate_path key
              | None -> data))
          ~remove:(fun ~outer_key:_ ~inner_key:key ~data:_ acc -> Map.remove acc key)
      in
      annotate Assoc_lifecycles lifecycle;
      let apply_action = Apply_action.incremental apply_action in
      Snapshot.create ~result:results_map ~apply_action ~lifecycle:(Some lifecycle)
    in
    let apply_static
          ~inject_dynamic
          ~inject_static
          ~schedule_event
          model
          (input_id, model_id, action)
      =
      let inject_dynamic a = inject_dynamic (input_id, model_id, a) in
      let inject_static a = inject_static (input_id, model_id, a) in
      let specific_model =
        Map.find model model_id |> Option.value ~default:model_info.default
      in
      let data =
        apply_static ~inject_dynamic ~inject_static ~schedule_event specific_model action
      in
      if model_info.equal data model_info.default
      then Map.remove model model_id
      else Map.set model ~key:model_id ~data
    in
    T
      { model = Meta.Model.map model_comparator model_key_id model_cmp_id model_info
      ; dynamic_action =
          Meta.Action.map_for_assoc_on io_key_id model_key_id dynamic_action
      ; static_action = Meta.Action.map_for_assoc_on io_key_id model_key_id static_action
      ; apply_static
      ; run
      }
  | Assoc_simpl { map; by } ->
    let run ~environment ~path ~clock:_ ~model:_ ~inject_dynamic:_ ~inject_static:_ =
      let map_input = Value.eval environment map in
      let result = Incr_map.mapi map_input ~f:(fun ~key ~data -> by path key data) in
      Snapshot.create ~result ~apply_action:Apply_action.impossible ~lifecycle:None
    in
    T
      { model = Meta.Model.unit
      ; dynamic_action = Meta.Action.nothing
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; run
      }
  | Switch { match_; arms } ->
    let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
      let index = Value.eval environment match_ in
      let%pattern_bind result, apply_action, lifecycle =
        let%bind index = index in
        (* !!!This is a load-bearing bind!!!

           If this bind isn't here, the scope that is created for the bind
           doesn't exist, and old incremental nodes might still be active, and
           with things like [match%sub] or [Bonsai.match_either] can witness old
           nodes, which can cause [assert false] to trigger. *)
        let path = Path.append path (Path.Elem.Switch index) in
        let t = Map.find_exn arms index in
        let (T
               { model = model_info
               ; dynamic_action = dynamic_action_info
               ; static_action = static_action_info
               ; apply_static = _
               ; run
               })
          =
          gather t
        in
        let chosen_model =
          Incremental.map model ~f:(fun map ->
            let (Meta.Model.Hidden.T { model; info; t_of_sexp = _ }) =
              Meta.Multi_model.find_exn map index
            in
            let equal =
              Meta.Model.Type_id.same_witness_exn info.type_id model_info.type_id
            in
            Type_equal.conv equal model)
        in
        let inject_dynamic action =
          inject_dynamic
            (Meta.Action.Hidden.T { action; type_id = dynamic_action_info; key = index })
        in
        let inject_static action =
          inject_static
            (Meta.Action.Hidden.T { action; type_id = static_action_info; key = index })
        in
        let snapshot =
          run ~environment ~model:chosen_model ~path ~clock ~inject_dynamic ~inject_static
        in
        let apply_action =
          let%mapn apply_action =
            Snapshot.(Apply_action.to_incremental (apply_action snapshot))
          in
          fun ~schedule_event
            model
            (Meta.Action.Hidden.T { action; type_id = action_type_id; key = index' }) ->
            let (T { model = chosen_model; info = chosen_model_info; _ }) =
              Meta.Multi_model.find_exn model index
            in
            match
              ( index = index'
              , Meta.Action.Type_id.same_witness action_type_id dynamic_action_info
              , Meta.Model.Type_id.same_witness
                  chosen_model_info.type_id
                  model_info.type_id )
            with
            | true, Some T, Some T ->
              let new_model = apply_action ~schedule_event chosen_model action in
              let new_model = Meta.Model.Hidden.create model_info new_model in
              Meta.Multi_model.set model ~key:index ~data:new_model
            | _ ->
              let action = Meta.Action.Type_id.to_sexp action_type_id action in
              eprint_s
                [%message
                  "an action inside of Bonsai.switch has been dropped because the \
                   computation is no longer active"
                    (index : int)
                    (action : Sexp.t)];
              model
        in
        let%mapn result = Snapshot.result snapshot
        and lifecycle = Snapshot.lifecycle_or_empty snapshot
        and apply_action = apply_action in
        result, apply_action, lifecycle
      in
      let apply_action = Apply_action.incremental apply_action in
      Snapshot.create ~apply_action ~result ~lifecycle:(Some lifecycle)
    in
    let apply_static
          ~inject_dynamic
          ~inject_static
          ~schedule_event
          model
          (action : int Meta.Action.Hidden.t)
      =
      let (T { action; type_id = action_type_id; key = index }) = action in
      let (T { model = chosen_model; info = chosen_model_info; _ }) =
        Meta.Multi_model.find_exn model index
      in
      let inject_static action =
        inject_static
          (Meta.Action.Hidden.T { action; type_id = action_type_id; key = index })
      in
      let (T
             { model = tm; static_action = am; dynamic_action = dm; apply_static; run = _ })
        =
        gather (Map.find_exn arms index)
      in
      let T = Meta.Model.Type_id.same_witness_exn tm.type_id chosen_model_info.type_id in
      let T = Meta.Action.Type_id.same_witness_exn am action_type_id in
      let inject_dynamic action =
        inject_dynamic (Meta.Action.Hidden.T { action; type_id = dm; key = index })
      in
      let new_model =
        apply_static ~inject_dynamic ~inject_static ~schedule_event chosen_model action
      in
      let new_model = Meta.Model.Hidden.create tm new_model in
      Meta.Multi_model.set model ~key:index ~data:new_model
    in
    let model =
      let models =
        Map.map arms ~f:(fun t ->
          let (T { model; _ }) = gather t in
          Meta.Model.Hidden.create model model.default)
      in
      Meta.Multi_model.model_info (Meta.Multi_model.of_models models)
    in
    T
      { model
      ; dynamic_action = Meta.Action.Hidden.int
      ; static_action = Meta.Action.Hidden.int
      ; apply_static
      ; run
      }
  | Lazy lazy_computation ->
    let dynamic_action = Meta.Action.Hidden.unit in
    let static_action = Meta.Action.Hidden.unit in
    let model = Meta.Model.Hidden.lazy_ in
    let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
      let (T
             { model = model_info
             ; dynamic_action = dynamic_action_info
             ; static_action = static_action_info
             ; run
             ; _
             })
        =
        gather (force lazy_computation)
      in
      let input_model =
        let%map model = model in
        let (Meta.Model.Hidden.T { model; info; _ }) =
          Option.value
            model
            ~default:(Meta.Model.Hidden.create model_info model_info.default)
        in
        let witness =
          Meta.Model.Type_id.same_witness_exn info.type_id model_info.type_id
        in
        Type_equal.conv witness model
      in
      let inject_dynamic action =
        inject_dynamic
          (Meta.Action.Hidden.T { action; type_id = dynamic_action_info; key = () })
      in
      let inject_static action =
        inject_static
          (Meta.Action.Hidden.T { action; type_id = static_action_info; key = () })
      in
      let snapshot =
        run ~environment ~path ~clock ~model:input_model ~inject_dynamic ~inject_static
      in
      let apply_action =
        Apply_action.map
          (Snapshot.apply_action snapshot)
          ~f:(fun apply_action ~schedule_event model action ->
            let (Meta.Action.Hidden.T { action; type_id = action_type_id; key = () }) =
              action
            in
            let (Meta.Model.Hidden.T
                   { model = chosen_model; info = chosen_model_info; _ })
              =
              Option.value
                model
                ~default:(Meta.Model.Hidden.create model_info model_info.default)
            in
            match
              ( Meta.Action.Type_id.same_witness action_type_id dynamic_action_info
              , Meta.Model.Type_id.same_witness
                  chosen_model_info.type_id
                  model_info.type_id )
            with
            | Some T, Some T ->
              let new_model = apply_action ~schedule_event chosen_model action in
              Some (Meta.Model.Hidden.create model_info new_model)
            | _ ->
              print_s [%message "BUG: type-id mismatch in Bonsai.lazy_"];
              model)
      in
      Snapshot.create
        ~apply_action
        ~result:(Snapshot.result snapshot)
        ~lifecycle:(Snapshot.lifecycle snapshot)
    in
    let apply_static ~inject_dynamic ~inject_static ~schedule_event model action =
      (* forcing the lazy is fine because actions are finite in length *)
      let (T
             { model = model_info
             ; dynamic_action = dynamic_action_info
             ; static_action = static_action_info
             ; apply_static
             ; _
             })
        =
        gather (force lazy_computation)
      in
      let inject_dynamic action =
        inject_dynamic
          (Meta.Action.Hidden.T { action; type_id = dynamic_action_info; key = () })
      in
      let inject_static action =
        inject_static
          (Meta.Action.Hidden.T { action; type_id = static_action_info; key = () })
      in
      let (Meta.Action.Hidden.T { action; type_id = action_type_id; key = () }) =
        action
      in
      let (Meta.Model.Hidden.T { model = chosen_model; info = chosen_model_info; _ }) =
        Option.value
          model
          ~default:(Meta.Model.Hidden.create model_info model_info.default)
      in
      let T = Meta.Action.Type_id.same_witness_exn action_type_id static_action_info in
      let T =
        Meta.Model.Type_id.same_witness_exn chosen_model_info.type_id model_info.type_id
      in
      let new_model =
        apply_static ~inject_dynamic ~inject_static ~schedule_event chosen_model action
      in
      Some (Meta.Model.Hidden.create model_info new_model)
    in
    T { model; dynamic_action; static_action; apply_static; run }
  | Wrap { wrapper_model; action_id; inject_id; model_id; inner; dynamic_apply_action } ->
    let (T
           { model = inner_model
           ; dynamic_action = inner_dynamic_action
           ; static_action = inner_static_action
           ; apply_static
           ; run
           })
      =
      gather inner
    in
    let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
      let%pattern_bind outer_model, inner_model = model in
      let dynamic_inject_outer a = inject_dynamic (Either.First a) in
      let dynamic_inject_inner a = inject_dynamic (Either.Second a) in
      let inner_snapshot =
        let environment =
          environment
          |> Environment.add_exn ~key:model_id ~data:outer_model
          |> Environment.add_exn ~key:inject_id ~data:(Incr.return dynamic_inject_outer)
        in
        run
          ~environment
          ~path
          ~model:inner_model
          ~clock
          ~inject_dynamic:dynamic_inject_inner
          ~inject_static
      in
      let inner_result = Snapshot.result inner_snapshot in
      let apply_action =
        let%mapn inner_result = inner_result
        and inner_apply_action =
          Snapshot.(Apply_action.to_incremental (apply_action inner_snapshot))
        in
        fun ~schedule_event (outer_model, inner_model) action ->
          match action with
          | First action_outer ->
            let new_outer_model =
              dynamic_apply_action
                ~inject_dynamic:dynamic_inject_outer
                ~inject_static:Nothing.unreachable_code
                ~schedule_event
                inner_result
                outer_model
                action_outer
            in
            new_outer_model, inner_model
          | Second action_inner ->
            let new_inner_model =
              inner_apply_action ~schedule_event inner_model action_inner
            in
            outer_model, new_inner_model
      in
      Snapshot.create
        ~result:inner_result
        ~apply_action:(Apply_action.incremental apply_action)
        ~lifecycle:(Snapshot.lifecycle inner_snapshot)
    in
    let dynamic_action = Meta.Action.both action_id inner_dynamic_action in
    let model = Meta.Model.both wrapper_model inner_model in
    let apply_static ~inject_dynamic ~inject_static ~schedule_event (m1, m2) action =
      let inject_dynamic a = inject_dynamic (Second a) in
      m1, apply_static ~inject_dynamic ~inject_static ~schedule_event m2 action
    in
    T { model; dynamic_action; static_action = inner_static_action; apply_static; run }
  | With_model_resetter { inner; reset_id } ->
    let (T { model; dynamic_action; static_action; apply_static; run }) = gather inner in
    let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
      let reset_event = inject_static (First ()) in
      let inject_static a = inject_static (Second a) in
      let environment =
        environment |> Environment.add_exn ~key:reset_id ~data:(Incr.return reset_event)
      in
      let snapshot =
        run ~environment ~path ~model ~clock ~inject_dynamic ~inject_static
      in
      let apply_action = Snapshot.apply_action snapshot in
      let result = Snapshot.result snapshot in
      Snapshot.create ~result ~apply_action ~lifecycle:(Snapshot.lifecycle snapshot)
    in
    let static_action = Meta.(Action.both Action.Type_id.unit static_action) in
    let apply_static ~inject_dynamic ~inject_static ~schedule_event m =
      let inject_static a = inject_static (Second a) in
      function
      | First () -> model.default
      | Second a -> apply_static ~inject_dynamic ~inject_static ~schedule_event m a
    in
    T { model; static_action; dynamic_action; apply_static; run }
  | Path ->
    let run ~environment:_ ~path ~clock:_ ~model:_ ~inject_dynamic:_ ~inject_static:_ =
      let result = Incr.return path in
      annotate Path result;
      Snapshot.create ~result ~apply_action:Apply_action.impossible ~lifecycle:None
    in
    T
      { model = Meta.Model.unit
      ; dynamic_action = Meta.Action.nothing
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; run
      }
  | Lifecycle lifecycle ->
    let run ~environment ~path ~clock:_ ~model:_ ~inject_dynamic:_ ~inject_static:_ =
      let lifecycle =
        match%pattern_bind Value.eval environment lifecycle with
        | Some lifecycle ->
          let%map lifecycle = lifecycle in
          Path.Map.singleton path lifecycle
        | None -> do_nothing_lifecycle
      in
      Snapshot.create
        ~result:(Incr.return ())
        ~apply_action:Apply_action.impossible
        ~lifecycle:(Some lifecycle)
    in
    T
      { model = Meta.Model.unit
      ; dynamic_action = Meta.Action.nothing
      ; static_action = Meta.Action.nothing
      ; apply_static = unusable_static_apply_action
      ; run
      }
  | Identity t -> gather t

and gather : type result. result Computation.t -> result Computation.packed_info =
  fun { info; _ } -> force info
;;

let wrap_computation ~pack kind = pack kind (lazy (gather_impl kind))
