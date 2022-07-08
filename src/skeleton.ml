open! Core
open! Import

let finalize node_path_builder = lazy (node_path_builder |> Node_path.finalize)

module Bonsai_value = Value

module Id = struct
  include Int

  let of_type_id id =
    id |> Type_equal.Id.uid |> Type_equal.Id.Uid.sexp_of_t |> Int.t_of_sexp
  ;;
end

module Value = struct
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.Stable.V1.t option
    ; id : Id.t
    }

  and kind =
    | Constant
    | Lazy
    | Incr
    | Named
    | Cutoff of { t : t }
    | Mapn of { inputs : t list }
  [@@deriving sexp]

  module Minimal = struct
    type nonrec complete = t

    type t =
      | Constant of { id : Id.t }
      | Lazy
      | Incr
      | Named of { uid : Id.t }
      | Cutoff of { t : t }
      | Mapn of { inputs : t list }
    [@@deriving sexp]

    let rec of_complete (complete : complete) =
      match complete.kind with
      | Constant -> Constant { id = complete.id }
      | Lazy -> Lazy
      | Incr -> Incr
      | Named -> Named { uid = complete.id }
      | Cutoff { t } -> Cutoff { t = of_complete t }
      | Mapn { inputs } -> Mapn { inputs = List.map inputs ~f:of_complete }
    ;;
  end

  let minimal_sexp_of_t t = Minimal.(of_complete t |> sexp_of_t)

  let inputs { kind; node_path = _; here = _; id = _ } =
    match kind with
    | Constant | Incr | Named | Lazy -> []
    | Cutoff { t } -> [ t ]
    | Mapn { inputs } -> inputs
  ;;

  let of_value' : initial_path:Node_path.builder -> 'a Value.t -> t =
    fun ~initial_path value ->
      let rec helper : type a. current_path:Node_path.builder -> a Value.t -> t =
        let module Packed_value = struct
          type t = T : 'a Value.t -> t
        end
        in
        let create_mapn_with_choices ~current_path (values : Packed_value.t list) =
          Mapn
            { inputs =
                List.mapi values ~f:(fun i (T value) ->
                  helper
                    ~current_path:
                      (Node_path.choice_point current_path (i + 1) |> Node_path.descend)
                    value)
            }
        in
        fun ~current_path { value; here; id = outer_id } ->
          let kind =
            match value with
            | Constant _ -> Constant
            | Lazy _ -> Lazy
            | Incr _ -> Incr
            | Named -> Named
            | Cutoff { t; equal = _ } ->
              Cutoff
                { t =
                    helper
                      ~current_path:
                        (Node_path.choice_point current_path 1 |> Node_path.descend)
                      t
                }
            | Map { t; f = _ } -> create_mapn_with_choices ~current_path [ T t ]
            | Both (t1, t2) -> create_mapn_with_choices ~current_path [ T t1; T t2 ]
            | Map2 { t1; t2; f = _ } ->
              create_mapn_with_choices ~current_path [ T t1; T t2 ]
            | Map3 { t1; t2; t3; f = _ } ->
              create_mapn_with_choices ~current_path [ T t1; T t2; T t3 ]
            | Map4 { t1; t2; t3; t4; f = _ } ->
              create_mapn_with_choices ~current_path [ T t1; T t2; T t3; T t4 ]
            | Map5 { t1; t2; t3; t4; t5; f = _ } ->
              create_mapn_with_choices ~current_path [ T t1; T t2; T t3; T t4; T t5 ]
            | Map6 { t1; t2; t3; t4; t5; t6; f = _ } ->
              create_mapn_with_choices ~current_path [ T t1; T t2; T t3; T t4; T t5; T t6 ]
            | Map7 { t1; t2; t3; t4; t5; t6; t7; f = _ } ->
              create_mapn_with_choices
                ~current_path
                [ T t1; T t2; T t3; T t4; T t5; T t6; T t7 ]
          in
          { node_path = finalize current_path; here; kind; id = Id.of_type_id outer_id }
      in
      helper ~current_path:initial_path value
  ;;

  let of_value value = of_value' ~initial_path:(Node_path.descend Node_path.empty) value

  let rec to_string_hum { node_path = _; kind; here = _; id } =
    match kind with
    | Lazy -> "lazy"
    | Constant -> sprintf "constant_%s" (Id.to_string id)
    | Incr -> "incr"
    | Named -> sprintf "x%s" (Id.to_string id)
    | Cutoff { t } -> sprintf "(cutoff %s)" (to_string_hum t)
    | Mapn { inputs } ->
      sprintf "(mapn %s)" (String.concat ~sep:" " (List.map inputs ~f:to_string_hum))
  ;;
end

module Computation = struct
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.Stable.V1.t option
    }

  and kind =
    | Return of { value : Value.t }
    | Leaf01 of
        { input : Value.t
        ; name : string
        }
    | Leaf1 of
        { input : Value.t
        ; name : string
        }
    | Leaf0 of { name : string }
    | Leaf_incr of
        { input : Value.t
        ; name : string
        }
    | Model_cutoff of { t : t }
    | Sub of
        { from : t
        ; via : int
        ; into : t
        ; statefulness : [ `Stateful | `Stateless_from | `Stateless_into ]
        }
    | Store of
        { id : int
        ; value : Value.t
        ; inner : t
        }
    | Fetch of { id : int }
    | Assoc of
        { map : Value.t
        ; key_id : int
        ; data_id : int
        ; by : t
        }
    | Assoc_on of
        { map : Value.t
        ; io_key_id : int
        ; data_id : int
        ; by : t
        }
    | Assoc_simpl of { map : Value.t }
    | Switch of
        { match_ : Value.t
        ; arms : t list
        }
    | Lazy of { t : t option }
    | Wrap of
        { model_id : int
        ; inject_id : int
        ; inner : t
        }
    | With_model_resetter of { t : t }
    | Path
    | Lifecycle of { value : Value.t }
  [@@deriving sexp]

  class ['acc] fold =
    object (self)
      method uid (_ : int) (acc : 'acc) = acc

      method value_kind (value_kind : Value.kind) acc =
        match value_kind with
        | Constant -> acc
        | Lazy -> acc
        | Incr -> acc
        | Named -> acc
        | Cutoff { t } -> self#value t acc
        | Mapn { inputs } -> List.fold inputs ~init:acc ~f:(fun acc x -> self#value x acc)

      method source_code_position (_ : Source_code_position.Stable.V1.t option) acc = acc
      method node_path (_ : Node_path.t Lazy.t) acc = acc

      method value (value : Value.t) acc =
        acc
        |> self#value_kind value.kind
        |> self#source_code_position value.here
        |> self#node_path value.node_path
        |> self#uid value.id

      method string (_ : string) acc = acc

      method statefulness (_ : [ `Stateful | `Stateless_from | `Stateless_into ]) acc =
        acc

      method computation_kind (computation_kind : kind) acc =
        match computation_kind with
        | Return { value } -> self#value value acc
        | Leaf01 { input; name } -> acc |> self#value input |> self#string name
        | Leaf1 { input; name } -> acc |> self#value input |> self#string name
        | Leaf0 { name } -> self#string name acc
        | Leaf_incr { input; name } -> acc |> self#value input |> self#string name
        | Model_cutoff { t } -> self#computation t acc
        | Sub { from; via; into; statefulness } ->
          acc
          |> self#computation from
          |> self#uid via
          |> self#computation into
          |> self#statefulness statefulness
        | Store { id; value; inner } ->
          acc |> self#uid id |> self#value value |> self#computation inner
        | Fetch { id } -> self#uid id acc
        | Assoc { map; key_id; data_id; by }
        | Assoc_on { map; io_key_id = key_id; data_id; by } ->
          acc
          |> self#value map
          |> self#uid key_id
          |> self#uid data_id
          |> self#computation by
        | Assoc_simpl { map } -> acc |> self#value map
        | Switch { match_; arms } ->
          let acc = self#value match_ acc in
          List.fold arms ~init:acc ~f:(fun acc x -> self#computation x acc)
        | Lazy { t } ->
          (match t with
           | None -> acc
           | Some t -> self#computation t acc)
        | Wrap { model_id; inject_id; inner } ->
          acc |> self#uid model_id |> self#uid inject_id |> self#computation inner
        | With_model_resetter { t } -> self#computation t acc
        | Path -> acc
        | Lifecycle { value } -> self#value value acc

      method computation (computation : t) acc =
        acc
        |> self#computation_kind computation.kind
        |> self#source_code_position computation.here
        |> self#node_path computation.node_path
    end

  class map =
    object (self)
      method uid (uid : Id.t) = uid

      method value_kind (value_kind : Value.kind) =
        match value_kind with
        | Constant -> Value.Constant
        | Lazy -> Lazy
        | Incr -> Incr
        | Named -> Named
        | Cutoff { t } -> Cutoff { t = self#value t }
        | Mapn { inputs } -> Mapn { inputs = List.map inputs ~f:self#value }

      method source_code_position (position : Source_code_position.Stable.V1.t option) =
        position

      method node_path (node_path : Node_path.t Lazy.t) = node_path

      method value (value : Value.t) =
        let kind = self#value_kind value.kind in
        let here = self#source_code_position value.here in
        let node_path = self#node_path value.node_path in
        let id = self#uid value.id in
        { kind; here; node_path; id }

      method string (s : string) = s
      method statefulness (s : [ `Stateful | `Stateless_from | `Stateless_into ]) = s

      method computation_kind (computation_kind : kind) =
        match computation_kind with
        | Return { value } -> Return { value = self#value value }
        | Leaf01 { input; name } ->
          Leaf01 { input = self#value input; name = self#string name }
        | Leaf1 { input; name } ->
          Leaf1 { input = self#value input; name = self#string name }
        | Leaf0 { name } -> Leaf0 { name = self#string name }
        | Leaf_incr { input : Value.t; name : string } ->
          Leaf_incr { input = self#value input; name = self#string name }
        | Model_cutoff { t } -> Model_cutoff { t = self#computation t }
        | Sub { from; via; into; statefulness } ->
          Sub
            { from = self#computation from
            ; via = self#uid via
            ; into = self#computation into
            ; statefulness = self#statefulness statefulness
            }
        | Store { id; value; inner } ->
          Store
            { id = self#uid id; value = self#value value; inner = self#computation inner }
        | Fetch { id } -> Fetch { id = self#uid id }
        | Assoc { map; key_id; data_id; by } ->
          Assoc
            { map = self#value map
            ; key_id = self#uid key_id
            ; data_id = self#uid data_id
            ; by = self#computation by
            }
        | Assoc_on { map; io_key_id; data_id; by } ->
          Assoc_on
            { map = self#value map
            ; io_key_id = self#uid io_key_id
            ; data_id = self#uid data_id
            ; by = self#computation by
            }
        | Assoc_simpl { map } -> Assoc_simpl { map = self#value map }
        | Switch { match_; arms } ->
          Switch { match_ = self#value match_; arms = List.map arms ~f:self#computation }
        | Lazy { t } ->
          (match t with
           | None -> Lazy { t = None }
           | Some t -> Lazy { t = Some (self#computation t) })
        | Wrap { model_id; inject_id; inner } ->
          Wrap
            { model_id = self#uid model_id
            ; inject_id = self#uid inject_id
            ; inner = self#computation inner
            }
        | With_model_resetter { t } -> With_model_resetter { t = self#computation t }
        | Path -> Path
        | Lifecycle { value } -> Lifecycle { value = self#value value }

      method computation (computation : t) =
        let kind = self#computation_kind computation.kind in
        let here = self#source_code_position computation.here in
        let node_path = self#node_path computation.node_path in
        { kind; here; node_path }
    end

  let sanitize_for_testing (t : t) =
    let min_uid =
      let find_minimum_ids =
        object
          inherit [int] fold as super
          method! uid uid min_uid = super#uid uid (min min_uid uid)
        end
      in
      find_minimum_ids#computation t Int.max_value
    in
    let min_uid = if min_uid = Int.max_value then 0 else min_uid in
    let replace_old_uids_with_sanitized_ones =
      object
        inherit map as super
        method! uid uid = super#uid (uid - min_uid)
      end
    in
    replace_old_uids_with_sanitized_ones#computation t
  ;;

  let of_computation : ('a, 'b, 'c, 'd) Computation.t -> t =
    fun computation ->
      let rec helper
        : type a b c d. current_path:Node_path.builder -> (a, b, c, d) Computation.t -> t
        =
        fun ~current_path computation ->
          let choice_point choice =
            Node_path.choice_point current_path choice |> Node_path.descend
          in
          let node_path = finalize current_path in
          match computation.t with
          | Return value ->
            { node_path
            ; here = None
            ; kind =
                Return
                  { value =
                      Value.of_value' ~initial_path:(Node_path.descend current_path) value
                  }
            }
          | Leaf01 { name; input; _ } ->
            { node_path
            ; here = None
            ; kind =
                Leaf01
                  { input =
                      Value.of_value' ~initial_path:(Node_path.descend current_path) input
                  ; name
                  }
            }
          | Leaf1 { name; input; _ } ->
            { node_path
            ; here = None
            ; kind =
                Leaf1
                  { input =
                      Value.of_value' ~initial_path:(Node_path.descend current_path) input
                  ; name
                  }
            }
          | Leaf0 { name; _ } -> { node_path; here = None; kind = Leaf0 { name } }
          | Leaf_incr { input; name; _ } ->
            { node_path
            ; here = None
            ; kind =
                Leaf_incr
                  { input =
                      Value.of_value' ~initial_path:(Node_path.descend current_path) input
                  ; name
                  }
            }
          | Model_cutoff { t; _ } ->
            { node_path
            ; here = None
            ; kind =
                Model_cutoff { t = helper ~current_path:(Node_path.descend current_path) t }
            }
          | Subst { from; via; into; here } ->
            let kind =
              Sub
                { from = helper ~current_path:(choice_point 1) from
                ; via = Id.of_type_id via
                ; into = helper ~current_path:(choice_point 2) into
                ; statefulness = `Stateful
                }
            in
            { node_path; here; kind }
          | Subst_stateless_from { from; via; into; here } ->
            let kind =
              Sub
                { from = helper ~current_path:(choice_point 1) from
                ; via = Id.of_type_id via
                ; into = helper ~current_path:(choice_point 2) into
                ; statefulness = `Stateless_from
                }
            in
            { node_path; here; kind }
          | Subst_stateless_into { from; via; into; here } ->
            let kind =
              Sub
                { from = helper ~current_path:(choice_point 1) from
                ; via = Id.of_type_id via
                ; into = helper ~current_path:(choice_point 2) into
                ; statefulness = `Stateless_into
                }
            in
            { node_path; here; kind }
          | Store { id; value; inner } ->
            let kind =
              Store
                { id = Id.of_type_id id
                ; value = Value.of_value' ~initial_path:(choice_point 1) value
                ; inner = helper ~current_path:(choice_point 2) inner
                }
            in
            { node_path; here = None; kind }
          | Fetch { id; _ } ->
            let kind = Fetch { id = Id.of_type_id id } in
            { node_path; here = None; kind }
          | Assoc { map; key_id; data_id; by; _ } ->
            let kind =
              Assoc
                { map = Value.of_value' ~initial_path:(choice_point 1) map
                ; key_id = Id.of_type_id key_id
                ; data_id = Id.of_type_id data_id
                ; by = helper ~current_path:(choice_point 2) by
                }
            in
            { node_path; here = None; kind }
          | Assoc_on { map; io_key_id; data_id; by; _ } ->
            let kind =
              Assoc_on
                { map = Value.of_value' ~initial_path:(choice_point 1) map
                ; io_key_id = Id.of_type_id io_key_id
                ; data_id = Id.of_type_id data_id
                ; by = helper ~current_path:(choice_point 2) by
                }
            in
            { node_path; here = None; kind }
          | Assoc_simpl { map; _ } ->
            let kind =
              Assoc_simpl
                { map = Value.of_value' ~initial_path:(Node_path.descend current_path) map }
            in
            { node_path; here = None; kind }
          | Switch { match_; arms; _ } ->
            (* This form of node_path generation is necessary to achive the same traversal as the one
               in [transform.mli] so that both node paths are in sync. *)
            let index = ref 1 in
            let kind =
              Switch
                { match_ = Value.of_value' ~initial_path:(choice_point 1) match_
                ; arms =
                    Map.fold
                      arms
                      ~init:[]
                      ~f:(fun ~key:_ ~data:(T { t = computation; _ }) acc ->
                        incr index;
                        helper ~current_path:(choice_point !index) computation :: acc)
                    |> List.rev
                }
            in
            { node_path; here = None; kind }
          | Lazy t ->
            let potentially_evaluated =
              (* If lazy has already been forced, then the forced value is stored. *)
              match Lazy.is_val t with
              | false -> None
              | true ->
                let (T { t; _ }) = Lazy.force t in
                let t = helper ~current_path:(Node_path.descend current_path) t in
                Some t
            in
            { node_path; here = None; kind = Lazy { t = potentially_evaluated } }
          | With_model_resetter t ->
            let kind =
              With_model_resetter
                { t = helper ~current_path:(Node_path.descend current_path) t }
            in
            { node_path; here = None; kind }
          | Wrap { inner; model_id; inject_id; _ } ->
            let kind =
              Wrap
                { model_id = Id.of_type_id model_id
                ; inject_id = Id.of_type_id inject_id
                ; inner = helper ~current_path:(Node_path.descend current_path) inner
                }
            in
            { node_path; here = None; kind }
          | Path -> { node_path; here = None; kind = Path }
          | Lifecycle value ->
            let kind =
              Lifecycle
                { value = Value.of_value' ~initial_path:(Node_path.descend current_path) value
                }
            in
            { node_path; here = None; kind }
      in
      helper ~current_path:(Node_path.descend Node_path.empty) computation
  ;;

  module Minimal = struct
    type complete = t

    type t =
      | Return of { value : Value.Minimal.t }
      | Leaf01 of
          { input : Value.Minimal.t
          ; name : string
          }
      | Leaf1 of
          { input : Value.Minimal.t
          ; name : string
          }
      | Leaf0 of { name : string }
      | Leaf_incr of
          { input : Value.Minimal.t
          ; name : string
          }
      | Model_cutoff of { t : t }
      | Sub of
          { from : t
          ; via : Id.t
          ; into : t
          ; statefulness : [ `Stateful | `Stateless_from | `Stateless_into ]
          }
      | Store of
          { id : Id.t
          ; value : Value.Minimal.t
          ; inner : t
          }
      | Fetch of { id : Id.t }
      | Assoc of
          { map : Value.Minimal.t
          ; key_id : Id.t
          ; data_id : Id.t
          ; by : t
          }
      | Assoc_on of
          { map : Value.Minimal.t
          ; io_key_id : Id.t
          ; data_id : Id.t
          ; by : t
          }
      | Assoc_simpl of { map : Value.Minimal.t }
      | Switch of
          { match_ : Value.Minimal.t
          ; arms : t list
          }
      | Lazy of { t : t option }
      | Wrap of
          { model_id : Id.t
          ; inject_id : Id.t
          ; inner : t
          }
      | With_model_resetter of { t : t }
      | Path
      | Lifecycle of { value : Value.Minimal.t }
    [@@deriving sexp]

    let rec of_complete (complete : complete) =
      match complete.kind with
      | Return { value } -> Return { value = Value.Minimal.of_complete value }
      | Leaf01 { input; name } -> Leaf01 { input = Value.Minimal.of_complete input; name }
      | Leaf1 { input; name } -> Leaf1 { input = Value.Minimal.of_complete input; name }
      | Leaf0 { name } -> Leaf0 { name }
      | Leaf_incr { input; name } ->
        Leaf_incr { input = Value.Minimal.of_complete input; name }
      | Model_cutoff { t } -> Model_cutoff { t = of_complete t }
      | Sub { from; via; into; statefulness } ->
        Sub { from = of_complete from; via; into = of_complete into; statefulness }
      | Store { id; value; inner } ->
        Store { id; value = Value.Minimal.of_complete value; inner = of_complete inner }
      | Fetch { id } -> Fetch { id }
      | Assoc { map; key_id; data_id; by } ->
        Assoc
          { map = Value.Minimal.of_complete map; key_id; data_id; by = of_complete by }
      | Assoc_on { map; io_key_id; data_id; by } ->
        Assoc_on
          { map = Value.Minimal.of_complete map; io_key_id; data_id; by = of_complete by }
      | Assoc_simpl { map } -> Assoc_simpl { map = Value.Minimal.of_complete map }
      | Switch { match_; arms } ->
        Switch
          { match_ = Value.Minimal.of_complete match_
          ; arms = List.map arms ~f:of_complete
          }
      | Lazy { t = None } -> Lazy { t = None }
      | Lazy { t = Some t } -> Lazy { t = Some (of_complete t) }
      | Wrap { model_id; inject_id; inner } ->
        Wrap { model_id; inject_id; inner = of_complete inner }
      | With_model_resetter { t } -> With_model_resetter { t = of_complete t }
      | Path -> Path
      | Lifecycle { value } -> Lifecycle { value = Value.Minimal.of_complete value }
    ;;
  end

  let minimal_sexp_of_t t = Minimal.(of_complete t |> sexp_of_t)

  let inputs (t : t) =
    match t.kind with
    | Return { value } -> [ value ]
    | Leaf01 { input; name = _ } -> [ input ]
    | Leaf1 { input; name = _ } -> [ input ]
    | Leaf0 { name = _ } -> []
    | Leaf_incr { input; name = _ } -> [ input ]
    | Model_cutoff _ -> []
    | Sub { from = _; via = _; into = _; statefulness = _ } -> []
    | Store { value; id = _; inner = _ } -> [ value ]
    | Fetch { id = _ } -> []
    | Assoc { map; key_id = _; data_id = _; by = _ }
    | Assoc_on { map; io_key_id = _; data_id = _; by = _ } -> [ map ]
    | Assoc_simpl { map } -> [ map ]
    | Switch { match_; arms = _ } -> [ match_ ]
    | Lazy _ -> []
    | Wrap { model_id = _; inject_id = _; inner = _ } -> []
    | With_model_resetter _ -> []
    | Path -> []
    | Lifecycle { value } -> [ value ]
  ;;

  let children (t : t) =
    match t.kind with
    | Return _ -> []
    | Leaf01 { input = _; name = _ } -> []
    | Leaf1 { input = _; name = _ } -> []
    | Leaf0 { name = _ } -> []
    | Leaf_incr { input = _; name = _ } -> []
    | Model_cutoff { t } -> [ t ]
    | Sub { from; via = _; into; statefulness = _ } -> [ from; into ]
    | Store { id = _; value = _; inner : t } -> [ inner ]
    | Fetch { id = _ } -> []
    | Assoc { map = _; key_id = _; data_id = _; by }
    | Assoc_on { map = _; io_key_id = _; data_id = _; by } -> [ by ]
    | Assoc_simpl { map = _ } -> []
    | Switch { match_ = _; arms = _ } -> []
    | Lazy { t = None } -> []
    | Lazy { t = Some t } -> [ t ]
    | Wrap { model_id = _; inject_id = _; inner } -> [ inner ]
    | With_model_resetter { t } -> [ t ]
    | Path -> []
    | Lifecycle _ -> []
  ;;
end
