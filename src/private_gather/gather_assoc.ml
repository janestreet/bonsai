open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

let unzip3_mapi'
  map
  ~(contains_lifecycle : May_contain.resolved May_contain.Single.t)
  ~(contains_input : May_contain.resolved May_contain.Single.t)
  ~comparator
  ~f
  =
  match contains_lifecycle, contains_input with
  | No, No ->
    let results =
      Incr_map.mapi' map ~f:(fun ~key ~data ->
        let a, _, _ = f ~key ~data in
        a)
    in
    results, Incr.return (Map.empty comparator), Incr.return (Map.empty comparator)
  | No, Yes_or_maybe ->
    (* if we know that [f] always returns a triple whose last element (the lifecycle
       incremental) is always the empty lifecycle collection, then we can drop it here,
       and avoid nesting unzips *)
    let first, second =
      Incr_map.unzip_mapi' map ~f:(fun ~key ~data ->
        let a, b, _ = f ~key ~data in
        a, b)
    in
    first, second, Incr.return (Map.empty comparator)
  | Yes_or_maybe, No ->
    (* if we know that [f] always returns a triple whose second element (the input
       incremental) is always the empty lifecycle collection, then we can drop it here,
       and avoid nesting unzips *)
    let first, third =
      Incr_map.unzip_mapi' map ~f:(fun ~key ~data ->
        let a, _, c = f ~key ~data in
        a, c)
    in
    first, Incr.return (Map.empty comparator), third
  | Yes_or_maybe, Yes_or_maybe -> Incr_map.unzip3_mapi' map ~f
;;

let f
  (type k cmp)
  ~gather
  ~recursive_scopes
  ~time_source
  ~map
  ~(key_comparator : (k, cmp) Comparator.Module.t)
  ~key_id
  ~cmp_id
  ~data_id
  ~by
  ~here
  =
  let module Cmp = (val key_comparator) in
  let wrap_assoc ~key inject =
    Action.assoc ~id:key_id ~compare:(Comparator.compare Cmp.comparator) ~key >>> inject
  in
  let%bind.Trampoline (Computation.T
                        { model = model_info
                        ; input = input_info
                        ; action
                        ; apply_action
                        ; run
                        ; reset
                        ; may_contain
                        })
    =
    gather ~recursive_scopes ~time_source by
  in
  let run ~environment ~fix_envs ~path ~model ~inject =
    let resolved = Environment.Recursive.resolve_may_contain fix_envs may_contain in
    let map_input = Value.eval environment map in
    let input_and_models_map =
      Incr_map.merge map_input model ~f:(fun ~key:_ -> function
        | `Left input -> Some (input, model_info.default)
        | `Right _ -> None
        | `Both input_and_models -> Some input_and_models)
    in
    let create_keyed =
      unstage (Path.Elem.keyed ~compare:(Comparator.compare Cmp.comparator) key_id)
    in
    let results_map, input_map, lifecycle_map =
      unzip3_mapi'
        input_and_models_map
        ~comparator:(module Cmp)
        ~contains_lifecycle:resolved.lifecycle
        ~contains_input:resolved.input
        ~f:(fun ~key ~data:input_and_model ->
          annotate ~here Model_and_input input_and_model;
          let path =
            match resolved.path with
            | Yes_or_maybe -> Path.append path Path.Elem.(Assoc (create_keyed key))
            | No -> path
          in
          let%pattern_bind value, model = input_and_model in
          let key_incr = Incr.const key in
          annotate ~here Assoc_key key_incr;
          annotate ~here Assoc_input value;
          let environment =
            (* It is safe to reuse the same [key_id] and [data_id] for each pair in the
               map, since they all start with a fresh "copy" of the outer environment. *)
            environment
            |> Environment.add_exn ~key:key_id ~data:key_incr
            |> Environment.add_exn ~key:data_id ~data:value
          in
          let snapshot, () =
            run ~environment ~fix_envs ~path ~inject:(wrap_assoc ~key inject) ~model
            |> Trampoline.run
          in
          ( Snapshot.result snapshot
          , Input.to_incremental (Snapshot.input snapshot)
          , Snapshot.lifecycle_or_empty ~here snapshot ))
    in
    annotate ~here Assoc_results results_map;
    annotate ~here Assoc_lifecycles lifecycle_map;
    annotate ~here Assoc_inputs input_map;
    let lifecycle =
      (* if we can prove that the body of the assoc doesn't contain a lifecycle node, then
         return None, dropping the constant incremental node on the floor. *)
      match resolved.lifecycle with
      | No -> None
      | Yes_or_maybe ->
        let unfolded =
          Incr_map.unordered_fold_nested_maps
            lifecycle_map
            ~init:Path.Map.empty
            ~add:(fun ~outer_key:_ ~inner_key:key ~data acc ->
              Map.update acc key ~f:(function
                | Some _ -> Path.raise_duplicate key
                | None -> data))
            ~remove:(fun ~outer_key:_ ~inner_key:key ~data:_ acc -> Map.remove acc key)
        in
        annotate ~here Assoc_lifecycles unfolded;
        Some unfolded
    in
    let input =
      match resolved.input with
      | No -> Input.static_none
      | Yes_or_maybe -> Input.dynamic (input_map >>| Option.some)
    in
    Trampoline.return (Snapshot.create ~here ~result:results_map ~input ~lifecycle, ())
  in
  let apply_action
    ~inject
    ~schedule_event
    input
    model
    (Action.Assoc { key; action; id = _; compare = _ })
    =
    let input =
      input |> Option.join |> Option.bind ~f:(fun input -> Map.find input key)
    in
    let specific_model = Map.find model key |> Option.value ~default:model_info.default in
    let data =
      apply_action
        ~inject:(wrap_assoc ~key inject)
        ~schedule_event
        input
        specific_model
        action
    in
    if model_info.equal data model_info.default
    then Map.remove model key
    else Map.set model ~key ~data
  in
  let reset ~inject ~schedule_event model =
    Map.filter_mapi model ~f:(fun ~key ~data ->
      let new_model = reset ~inject:(wrap_assoc ~key inject) ~schedule_event data in
      if model_info.equal new_model model_info.default then None else Some new_model)
  in
  Trampoline.return
    (Computation.T
       { model = Meta.Model.map key_comparator key_id cmp_id model_info
       ; input = Meta.Input.map key_id cmp_id input_info
       ; action = Action.Type_id.assoc ~key:key_id ~action
       ; apply_action
       ; reset
       ; run
       ; may_contain
       })
;;
