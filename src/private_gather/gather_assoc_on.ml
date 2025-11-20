open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

let f
  (type io cmp_io model cmp_model)
  ~gather
  ~recursive_scopes
  ~time_source
  ~map
  ~(io_comparator : (io, cmp_io) Comparator.Module.t)
  ~(model_comparator : (model, cmp_model) Comparator.Module.t)
  ~io_key_id
  ~io_cmp_id
  ~model_key_id
  ~model_cmp_id
  ~data_id
  ~by
  ~get_model_key
  ~here
  =
  let module Model_comparator = (val model_comparator) in
  let module Io_comparator = (val io_comparator) in
  let wrap_assoc_on ~io_key ~model_key inject =
    Action.assoc_on
      ~io_key
      ~model_key
      ~io_id:io_key_id
      ~io_compare:(Comparator.compare Io_comparator.comparator)
    >>> inject
  in
  let model_key_comparator = Model_comparator.comparator in
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
    let create_keyed =
      unstage
        (Path.Elem.keyed ~compare:(Comparator.compare Io_comparator.comparator) io_key_id)
    in
    let results_map, input_map, lifecycle_map =
      Gather_assoc.unzip3_mapi'
        map_input
        ~contains_lifecycle:resolved.lifecycle
        ~contains_input:resolved.input
        ~comparator:(module Io_comparator)
        ~f:(fun ~key:io_key ~data:value ->
          let%pattern_bind results_map, input_map, lifecycle_map =
            let path =
              match resolved.path with
              | Yes_or_maybe -> Path.append path Path.Elem.(Assoc (create_keyed io_key))
              | No -> path
            in
            let key_incr = Incr.const io_key in
            annotate ~here Assoc_key key_incr;
            annotate ~here Assoc_input value;
            let environment =
              (* It is safe to reuse the same [key_id] and [data_id] for each pair in the
                 map, since they all start with a fresh "copy" of the outer environment. *)
              environment
              |> Environment.add_exn ~key:io_key_id ~data:key_incr
              |> Environment.add_exn ~key:data_id ~data:value
            in
            let model_key =
              let%map value in
              get_model_key io_key value
            in
            Incr.set_cutoff
              model_key
              (Incr.Cutoff.of_compare (Comparator.compare model_key_comparator));
            let%bind model_key in
            let model =
              let%map model in
              match Map.find model model_key with
              | Some (_, model) -> model
              | None -> model_info.default
            in
            annotate ~here Model model;
            let snapshot, () =
              run
                ~environment
                ~fix_envs
                ~path
                ~inject:(wrap_assoc_on ~io_key ~model_key inject)
                ~model
              |> Trampoline.run
            in
            let%mapn result = Snapshot.result snapshot
            and input = Input.to_incremental (Snapshot.input snapshot)
            and lifecycle = Snapshot.lifecycle_or_empty ~here snapshot in
            result, input, lifecycle
          in
          results_map, input_map, lifecycle_map)
    in
    annotate ~here Assoc_results results_map;
    annotate ~here Assoc_lifecycles lifecycle_map;
    let lifecycle =
      (* if we can prove that the body of the assoc_on doesn't contain a lifecycle node,
         then return None, dropping the constant incremental node on the floor. *)
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
    (Action.Assoc_on { io_key; model_key; action; io_id = _; io_compare = _ })
    =
    let input =
      input |> Option.join |> Option.bind ~f:(fun input -> Map.find input io_key)
    in
    let specific_model =
      match Map.find model model_key with
      | None -> model_info.default
      | Some (_prev_io_key, model) -> model
    in
    let new_model =
      apply_action
        ~inject:(wrap_assoc_on ~io_key ~model_key inject)
        ~schedule_event
        input
        specific_model
        action
    in
    if model_info.equal new_model model_info.default
    then Map.remove model model_key
    else Map.set model ~key:model_key ~data:(io_key, new_model)
  in
  let reset ~inject ~schedule_event model =
    Map.filter_mapi model ~f:(fun ~key:model_key ~data:(io_key, model) ->
      let new_model =
        reset ~inject:(wrap_assoc_on ~io_key ~model_key inject) ~schedule_event model
      in
      if model_info.equal new_model model_info.default
      then None
      else Some (io_key, new_model))
  in
  Trampoline.return
    (Computation.T
       { model =
           Meta.Model.map_on
             model_comparator
             io_comparator
             model_key_id
             io_key_id
             model_cmp_id
             model_info
       ; input = Meta.Input.map io_key_id io_cmp_id input_info
       ; action =
           Action.Type_id.assoc_on ~io_key:io_key_id ~model_key:model_key_id ~action
       ; apply_action
       ; reset
       ; run
       ; may_contain
       })
;;
