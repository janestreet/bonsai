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
    { recurse : 'a. 'from_parent -> 'a Value.t -> 'a Value.t
    ; var_from_parent : Var_from_parent.t
    ; parent_path : Node_path.t Lazy.t
    ; current_path : Node_path.t Lazy.t
    }

  type 'from_parent user_mapper =
    { f : 'a. 'from_parent context -> 'from_parent -> 'a Value.t -> 'a Value.t }

  let rec descend
    : type a.
      f:'from_parent user_mapper
      -> var_from_parent:Var_from_parent.t
      -> append_to:Node_path.builder
      -> 'from_parent
      -> a Value.t
      -> a Value.t
    =
    fun ~f ~var_from_parent ~append_to parent { value; here; id } ->
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
    let value =
      match value with
      | Constant _ -> value
      | Lazy _ -> value
      | Incr _ -> value
      | Named -> value
      | Both (a, b) -> Both (map 1 a, map 2 b)
      | Cutoff t -> Cutoff { t with t = map 1 t.t }
      | Map t -> Map { f = t.f; t = map 1 t.t }
      | Map2 t -> Map2 { f = t.f; t1 = map 1 t.t1; t2 = map 2 t.t2 }
      | Map3 t -> Map3 { f = t.f; t1 = map 1 t.t1; t2 = map 2 t.t2; t3 = map 3 t.t3 }
      | Map4 t ->
        Map4
          { f = t.f; t1 = map 1 t.t1; t2 = map 2 t.t2; t3 = map 3 t.t3; t4 = map 4 t.t4 }
      | Map5 t ->
        Map5
          { f = t.f
          ; t1 = map 1 t.t1
          ; t2 = map 2 t.t2
          ; t3 = map 3 t.t3
          ; t4 = map 4 t.t4
          ; t5 = map 5 t.t5
          }
      | Map6 t ->
        Map6
          { f = t.f
          ; t1 = map 1 t.t1
          ; t2 = map 2 t.t2
          ; t3 = map 3 t.t3
          ; t4 = map 4 t.t4
          ; t5 = map 5 t.t5
          ; t6 = map 6 t.t6
          }
      | Map7 t ->
        Map7
          { f = t.f
          ; t1 = map 1 t.t1
          ; t2 = map 2 t.t2
          ; t3 = map 3 t.t3
          ; t4 = map 4 t.t4
          ; t5 = map 5 t.t5
          ; t6 = map 6 t.t6
          ; t7 = map 7 t.t7
          }
    in
    { value; here; id }
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
end

module For_computation = struct
  type 'from_parent context =
    { recurse :
        'model 'dynamic_action 'static_action 'result.
          'from_parent
        -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
        -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
    ; var_from_parent : Var_from_parent.t
    ; parent_path : Node_path.t Lazy.t
    ; current_path : Node_path.t Lazy.t
    }

  type 'from_parent user_mapper =
    { f :
        'model 'dynamic_action 'static_action 'result.
          'from_parent context
        -> 'from_parent
        -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
        -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
    }

  let rec descend
    : type model dynamic_action static_action result.
      f:'from_parent user_mapper
      -> for_value:'a For_value.user_mapper
      -> append_to:Node_path.builder
      -> 'from_parent
      -> (model, dynamic_action, static_action, result) Computation.t
      -> (model, dynamic_action, static_action, result) Computation.t
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
    let map_packed ?choice (Computation.T t) =
      Computation.T { t with t = map ?choice t.t }
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
    let t : (model, dynamic_action, static_action, result) Computation.without_id =
      match computation.t with
      | Return value -> Return (map_value value)
      | Leaf1 t -> Leaf1 { t with input = map_value t.input }
      | Leaf01 t -> Leaf01 { t with input = map_value t.input }
      | Leaf0 _ -> computation.t
      | Leaf_incr t -> Leaf_incr { t with input = map_value t.input }
      | Model_cutoff t -> Model_cutoff { t with t = map t.t }
      | Subst t ->
        let from =
          map ~var_from_parent:(One (Type_equal.Id.uid t.via)) ~choice:1 t.from
        in
        let into = map ~choice:2 t.into in
        Subst { t with from; into }
      | Subst_stateless_from t ->
        let from =
          map ~var_from_parent:(One (Type_equal.Id.uid t.via)) ~choice:1 t.from
        in
        let into = map ~choice:2 t.into in
        Subst_stateless_from { t with from; into }
      | Subst_stateless_into t ->
        let from =
          map ~var_from_parent:(One (Type_equal.Id.uid t.via)) ~choice:1 t.from
        in
        let into = map ~choice:2 t.into in
        Subst_stateless_into { t with from; into }
      | Store t ->
        let value =
          map_value ~var_from_parent:(One (Type_equal.Id.uid t.id)) ~choice:1 t.value
        in
        let inner = map ~choice:2 t.inner in
        Store { t with value; inner }
      | Fetch _ -> computation.t
      | Assoc t ->
        Assoc
          { t with
            map = map_value ~choice:1 t.map
          ; by =
              map
                ~var_from_parent:
                  (Two (Type_equal.Id.uid t.key_id, Type_equal.Id.uid t.data_id))
                ~choice:2
                t.by
          }
      | Assoc_on t ->
        Assoc_on
          { t with
            map = map_value ~choice:1 t.map
          ; by =
              map
                ~var_from_parent:
                  (Two (Type_equal.Id.uid t.io_key_id, Type_equal.Id.uid t.data_id))
                ~choice:2
                t.by
          }
      | Assoc_simpl t -> Assoc_simpl { t with map = map_value t.map }
      | Switch { match_; arms } ->
        let index = ref 1 in
        Switch
          { match_ = map_value ~choice:1 match_
          ; arms =
              Map.map arms ~f:(fun c ->
                incr index;
                map_packed ~choice:!index c)
          }
      | Lazy t -> Lazy (Lazy.map t ~f:map_packed)
      | Wrap t ->
        Wrap
          { t with
            inner =
              map
                ~var_from_parent:
                  (Two (Type_equal.Id.uid t.model_id, Type_equal.Id.uid t.inject_id))
                t.inner
          }
      | With_model_resetter t -> With_model_resetter (map t)
      | Path -> computation.t
      | Lifecycle t -> Lifecycle (map_value t)
    in
    { computation with t }
  ;;
end

let map ~computation_mapper ~value_mapper ~init computation =
  let current_path = Node_path.descend Node_path.empty in
  let parent_path = Node_path.empty in
  let append_to = Node_path.empty in
  let open For_computation in
  (computation_mapper : _ user_mapper).f
    { recurse =
        (fun parent c ->
           descend ~f:computation_mapper ~for_value:value_mapper ~append_to parent c)
    ; var_from_parent = None
    ; parent_path = lazy (Node_path.finalize parent_path)
    ; current_path = lazy (Node_path.finalize current_path)
    }
    init
    computation
;;
