open! Core
open! Import

module Var_from_parent = struct
  type t =
    | None
    | One of Type_equal.Id.Uid.t
    | Two of Type_equal.Id.Uid.t * Type_equal.Id.Uid.t
end

module For_value = struct
  type 'from_parent context =
    { recurse : 'a. 'from_parent -> 'a Value.t -> 'a Value.t Trampoline.t
    ; var_from_parent : Var_from_parent.t
    ; parent_path : Node_path.t Lazy.t
    ; current_path : Node_path.t Lazy.t
    }

  type 'from_parent user_mapper =
    { f :
        'a. 'from_parent context -> 'from_parent -> 'a Value.t -> 'a Value.t Trampoline.t
    }

  let rec descend
    : type a.
      f:'from_parent user_mapper
      -> var_from_parent:Var_from_parent.t
      -> append_to:Node_path.builder
      -> 'from_parent
      -> a Value.t
      -> a Value.t Trampoline.t
    =
    fun ~f ~var_from_parent ~append_to parent { value; here } ->
    let current_path = Node_path.descend append_to in
    let map n v =
      let append_to = Node_path.choice_point current_path n in
      let child_path = Node_path.descend append_to in
      f.f
        { recurse = (fun parent v -> descend ~f ~var_from_parent:None ~append_to parent v)
        ; var_from_parent
        ; parent_path = lazy (Node_path.finalize current_path)
        ; current_path = lazy (Node_path.finalize child_path)
        }
        parent
        v
    in
    let open Trampoline.Let_syntax in
    let%bind value =
      match value with
      | Constant _ -> return value
      | Exception _ -> return value
      | Incr _ -> return value
      | Named _ -> return value
      | Both (a, b) ->
        let%bind a = map 1 a in
        let%bind b = map 2 b in
        return (Value.Both (a, b))
      | Cutoff { equal; added_by_let_syntax; t } ->
        let%bind t = map 1 t in
        return (Value.Cutoff { t; equal; added_by_let_syntax })
      | Map t ->
        let%bind inner = map 1 t.t in
        return (Value.Map { f = t.f; t = inner })
      | Map2 t ->
        let%bind t1 = map 1 t.t1 in
        let%bind t2 = map 2 t.t2 in
        return (Value.Map2 { f = t.f; t1; t2 })
      | Map3 t ->
        let%bind t1 = map 1 t.t1 in
        let%bind t2 = map 2 t.t2 in
        let%bind t3 = map 3 t.t3 in
        return (Value.Map3 { f = t.f; t1; t2; t3 })
      | Map4 t ->
        let%bind t1 = map 1 t.t1 in
        let%bind t2 = map 2 t.t2 in
        let%bind t3 = map 3 t.t3 in
        let%bind t4 = map 4 t.t4 in
        return (Value.Map4 { f = t.f; t1; t2; t3; t4 })
      | Map5 t ->
        let%bind t1 = map 1 t.t1 in
        let%bind t2 = map 2 t.t2 in
        let%bind t3 = map 3 t.t3 in
        let%bind t4 = map 4 t.t4 in
        let%bind t5 = map 5 t.t5 in
        return (Value.Map5 { f = t.f; t1; t2; t3; t4; t5 })
      | Map6 t ->
        let%bind t1 = map 1 t.t1 in
        let%bind t2 = map 2 t.t2 in
        let%bind t3 = map 3 t.t3 in
        let%bind t4 = map 4 t.t4 in
        let%bind t5 = map 5 t.t5 in
        let%bind t6 = map 6 t.t6 in
        return (Value.Map6 { f = t.f; t1; t2; t3; t4; t5; t6 })
      | Map7 t ->
        let%bind t1 = map 1 t.t1 in
        let%bind t2 = map 2 t.t2 in
        let%bind t3 = map 3 t.t3 in
        let%bind t4 = map 4 t.t4 in
        let%bind t5 = map 5 t.t5 in
        let%bind t6 = map 6 t.t6 in
        let%bind t7 = map 7 t.t7 in
        return (Value.Map7 { f = t.f; t1; t2; t3; t4; t5; t6; t7 })
    in
    return { Value.value; here }
  ;;

  let map ~f ~var_from_parent ~parent_path ~append_to parent v =
    let current_path = Node_path.descend append_to in
    f.f
      { recurse = (fun parent v -> descend ~f ~var_from_parent:None ~append_to parent v)
      ; var_from_parent
      ; parent_path
      ; current_path = lazy (Node_path.finalize current_path)
      }
      parent
      v
  ;;

  let id = { f = (fun { recurse; _ } from_parent value -> recurse from_parent value) }
end

module For_computation = struct
  type 'from_parent context =
    { recurse :
        'result.
        'from_parent -> 'result Computation.t -> 'result Computation.t Trampoline.t
    ; var_from_parent : Var_from_parent.t
    ; parent_path : Node_path.t Lazy.t
    ; current_path : Node_path.t Lazy.t
    }

  type 'from_parent user_mapper =
    { f :
        'result.
        'from_parent context
        -> 'from_parent
        -> 'result Computation.t
        -> 'result Computation.t Trampoline.t
    }

  let rec descend
    : type result.
      f:'from_parent user_mapper
      -> for_value:'a For_value.user_mapper
      -> append_to:Node_path.builder
      -> 'from_parent
      -> result Computation.t
      -> result Computation.t Trampoline.t
    =
    fun ~f ~for_value ~append_to parent (computation : _ Computation.t) ->
    let current_path = Node_path.descend append_to in
    let map ?(var_from_parent = Var_from_parent.None) ?choice c =
      let append_to =
        match choice with
        | Some choice -> Node_path.choice_point current_path choice
        | None -> current_path
      in
      let child_path = Node_path.descend append_to in
      f.f
        { recurse = (fun parent c -> descend ~f ~for_value ~append_to parent c)
        ; var_from_parent
        ; parent_path = lazy (Node_path.finalize current_path)
        ; current_path = lazy (Node_path.finalize child_path)
        }
        parent
        c
    in
    let map_value ?(var_from_parent = Var_from_parent.None) ?choice v =
      let append_to =
        match choice with
        | Some choice -> Node_path.choice_point current_path choice
        | None -> current_path
      in
      For_value.map
        ~f:for_value
        ~var_from_parent
        ~parent_path:(lazy (Node_path.finalize current_path))
        ~append_to
        parent
        v
    in
    let open Trampoline.Let_syntax in
    match computation with
    | Return { value; here } ->
      let%bind value = map_value value in
      return (Computation.Return { value; here })
    | Leaf1 t ->
      let%bind input = map_value t.input in
      return (Computation.Leaf1 { t with input })
    | Leaf0 _ -> return computation
    | Leaf_incr t ->
      let%bind input = map_value t.input in
      return (Computation.Leaf_incr { t with input })
    | Sub t ->
      let%bind from =
        map ~var_from_parent:(One (Type_equal.Id.uid t.via)) ~choice:1 t.from
      in
      let%bind into = map ~choice:2 t.into in
      return (Computation.Sub { t with from; into })
    | Store t ->
      let%bind value =
        map_value ~var_from_parent:(One (Type_equal.Id.uid t.id)) ~choice:1 t.value
      in
      let%bind inner = map ~choice:2 t.inner in
      return (Computation.Store { t with value; inner })
    | Fetch _ -> return computation
    | Assoc t ->
      let%bind map' = map_value ~choice:1 t.map in
      let%bind by =
        map
          ~var_from_parent:(Two (Type_equal.Id.uid t.key_id, Type_equal.Id.uid t.data_id))
          ~choice:2
          t.by
      in
      return (Computation.Assoc { t with map = map'; by })
    | Assoc_on t ->
      let%bind map' = map_value ~choice:1 t.map in
      let%bind by =
        map
          ~var_from_parent:
            (Two (Type_equal.Id.uid t.io_key_id, Type_equal.Id.uid t.data_id))
          ~choice:2
          t.by
      in
      return (Computation.Assoc_on { t with map = map'; by })
    | Assoc_simpl t ->
      let%bind map = map_value t.map in
      return (Computation.Assoc_simpl { t with map })
    | Switch { match_; arms; here } ->
      let index = ref 1 in
      let%bind match_ = map_value ~choice:!index match_ in
      let%bind arms =
        Trampoline.all_map
          (Map.map arms ~f:(fun c ->
             incr index;
             map ~choice:!index c))
      in
      return (Computation.Switch { match_; arms; here })
    | Fix_define { result; initial_input; fix_id; input_id; here } ->
      let%bind initial_input = map_value ~choice:1 initial_input in
      let%bind result = map ~choice:1 result in
      return (Computation.Fix_define { result; initial_input; fix_id; input_id; here })
    | Fix_recurse { fix_id; input_id; input; here } ->
      let%bind input = map_value ~choice:1 input in
      return (Computation.Fix_recurse { fix_id; input_id; input; here })
    | Lazy { t; here } ->
      let inner =
        Lazy.map t ~f:(fun t ->
          let t = map t in
          Trampoline.run t)
      in
      return (Computation.Lazy { t = inner; here })
    | Wrap ({ model_id; inject_id; inner; _ } as t) ->
      let%bind inner =
        map
          ~var_from_parent:(Two (Type_equal.Id.uid model_id, Type_equal.Id.uid inject_id))
          inner
      in
      return (Computation.Wrap { t with inner })
    | With_model_resetter { inner; reset_id; here } ->
      let%bind inner = map inner in
      return (Computation.With_model_resetter { inner; reset_id; here })
    | Path _ -> return computation
    | Lifecycle { lifecycle = t; here } ->
      let%bind inner = map_value t in
      return (Computation.Lifecycle { lifecycle = inner; here })
    | Computation_watcher
        { inner
        ; here
        ; free_vars
        ; config
        ; queue
        ; value_id_observation_definition_positions
        ; enable_watcher
        } ->
      let%bind inner = map inner in
      return
        (Computation.Computation_watcher
           { inner
           ; here
           ; free_vars
           ; config
           ; queue
           ; value_id_observation_definition_positions
           ; enable_watcher
           })
  ;;

  let id =
    { f = (fun { recurse; _ } from_parent computation -> recurse from_parent computation)
    }
  ;;
end

let map ~computation_mapper ~value_mapper ~init computation =
  let current_path = Node_path.descend Node_path.empty in
  let parent_path = Node_path.empty in
  let append_to = Node_path.empty in
  let open For_computation in
  Trampoline.run
    ((computation_mapper : _ user_mapper).f
       { recurse =
           (fun parent c ->
             descend ~f:computation_mapper ~for_value:value_mapper ~append_to parent c)
       ; var_from_parent = None
       ; parent_path = lazy (Node_path.finalize parent_path)
       ; current_path = lazy (Node_path.finalize current_path)
       }
       init
       computation)
;;
