open! Core
open! Import

let finalize node_path_builder = lazy (node_path_builder |> Node_path.finalize)

module Bonsai_value = Value

module Id = struct
  include Int

  let of_type_id id =
    id |> Type_equal.Id.uid |> Type_equal.Id.Uid.sexp_of_t |> Int.t_of_sexp
  ;;

  let of_model_type_id id = id |> Meta.Model.Type_id.to_type_id |> of_type_id
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
    | Exception
    | Incr
    | Named
    | Cutoff of
        { t : t
        ; added_by_let_syntax : bool
        }
    | Mapn of { inputs : t list }
  [@@deriving sexp]

  module Minimal = struct
    type nonrec complete = t

    type t =
      | Constant of { id : Id.t }
      | Exception
      | Incr
      | Named of { uid : Id.t }
      | Cutoff of
          { t : t
          ; added_by_let_syntax : bool
          }
      | Mapn of { inputs : t list }
    [@@deriving sexp]

    let rec of_complete (complete : complete) =
      match complete.kind with
      | Constant -> Constant { id = complete.id }
      | Exception -> Exception
      | Incr -> Incr
      | Named -> Named { uid = complete.id }
      | Cutoff { t; added_by_let_syntax } ->
        Cutoff { t = of_complete t; added_by_let_syntax }
      | Mapn { inputs } -> Mapn { inputs = List.map inputs ~f:of_complete }
    ;;
  end

  let minimal_sexp_of_t t = Minimal.(of_complete t |> sexp_of_t)

  let inputs { kind; node_path = _; here = _; id = _ } =
    match kind with
    | Constant | Incr | Named | Exception -> []
    | Cutoff { t; added_by_let_syntax = _ } -> [ t ]
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
          | Exception _ -> Exception
          | Incr _ -> Incr
          | Named _ -> Named
          | Cutoff { t; equal = _; added_by_let_syntax } ->
            Cutoff
              { t =
                  helper
                    ~current_path:
                      (Node_path.choice_point current_path 1 |> Node_path.descend)
                    t
              ; added_by_let_syntax
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
    | Exception -> sprintf "exception_%s" (Id.to_string id)
    | Constant -> sprintf "constant_%s" (Id.to_string id)
    | Incr -> "incr"
    | Named -> sprintf "x%s" (Id.to_string id)
    | Cutoff { t; added_by_let_syntax = _ } -> sprintf "(cutoff %s)" (to_string_hum t)
    | Mapn { inputs } ->
      sprintf "(mapn %s)" (String.concat ~sep:" " (List.map inputs ~f:to_string_hum))
  ;;
end

module Computation0 = struct
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.Stable.V1.t option
    }

  and kind =
    | Return of { value : Value.t }
    | Leaf01 of { input : Value.t }
    | Leaf1 of { input : Value.t }
    | Leaf0
    | Leaf_incr of { input : Value.t }
    | Model_cutoff of { t : t }
    | Sub of
        { from : t
        ; via : int
        ; into : t
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
        ; cmp_id : int
        ; data_id : int
        ; by : t
        }
    | Assoc_on of
        { map : Value.t
        ; io_key_id : int
        ; model_key_id : int
        ; model_cmp_id : int
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
    | With_model_resetter of
        { reset_id : int
        ; inner : t
        }
    | Path
    | Lifecycle of { value : Value.t }
    | Identity of { t : t }
  [@@deriving sexp]

  let of_computation : 'result Computation.t -> t =
    fun computation ->
    let rec helper
      : type result. current_path:Node_path.builder -> result Computation.t -> t
      =
      fun ~current_path computation ->
      let choice_point choice =
        Node_path.choice_point current_path choice |> Node_path.descend
      in
      let node_path = finalize current_path in
      match computation with
      | Return value ->
        { node_path
        ; here = None
        ; kind =
            Return
              { value =
                  Value.of_value' ~initial_path:(Node_path.descend current_path) value
              }
        }
      | Leaf1 { input; _ } ->
        { node_path
        ; here = None
        ; kind =
            Leaf1
              { input =
                  Value.of_value' ~initial_path:(Node_path.descend current_path) input
              }
        }
      | Leaf0 _ -> { node_path; here = None; kind = Leaf0 }
      | Leaf_incr { input; _ } ->
        { node_path
        ; here = None
        ; kind =
            Leaf_incr
              { input =
                  Value.of_value' ~initial_path:(Node_path.descend current_path) input
              }
        }
      | Sub { from; via; into; here } ->
        let kind =
          Sub
            { from = helper ~current_path:(choice_point 1) from
            ; via = Id.of_type_id via
            ; into = helper ~current_path:(choice_point 2) into
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
      | Assoc { map; key_id; cmp_id; data_id; by; _ } ->
        let kind =
          Assoc
            { map = Value.of_value' ~initial_path:(choice_point 1) map
            ; key_id = Id.of_type_id key_id
            ; cmp_id = Id.of_type_id cmp_id
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
      | Assoc_on { map; io_key_id; model_key_id; model_cmp_id; data_id; by; _ } ->
        let kind =
          Assoc_on
            { map = Value.of_value' ~initial_path:(choice_point 1) map
            ; io_key_id = Id.of_type_id io_key_id
            ; model_key_id = Id.of_type_id model_key_id
            ; model_cmp_id = Id.of_type_id model_cmp_id
            ; data_id = Id.of_type_id data_id
            ; by = helper ~current_path:(choice_point 2) by
            }
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
                Map.fold arms ~init:[] ~f:(fun ~key:_ ~data:computation acc ->
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
            Lazy.force t |> helper ~current_path:(Node_path.descend current_path) |> Some
        in
        { node_path; here = None; kind = Lazy { t = potentially_evaluated } }
      | With_model_resetter { reset_id; inner } ->
        let kind =
          With_model_resetter
            { reset_id = Id.of_type_id reset_id
            ; inner = helper ~current_path:(Node_path.descend current_path) inner
            }
        in
        { node_path; here = None; kind }
      | Wrap { inner; wrapper_model; inject_id; _ } ->
        let kind =
          Wrap
            { model_id = Id.of_model_type_id wrapper_model.type_id
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
      | Leaf01 of { input : Value.Minimal.t }
      | Leaf1 of { input : Value.Minimal.t }
      | Leaf0
      | Leaf_incr of { input : Value.Minimal.t }
      | Model_cutoff of { t : t }
      | Sub of
          { from : t
          ; via : Id.t
          ; into : t
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
          ; cmp_id : Id.t
          ; data_id : Id.t
          ; by : t
          }
      | Assoc_on of
          { map : Value.Minimal.t
          ; io_key_id : Id.t
          ; model_key_id : Id.t
          ; model_cmp_id : Id.t
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
      | With_model_resetter of
          { inner : t
          ; reset_id : Id.t
          }
      | Path
      | Lifecycle of { value : Value.Minimal.t }
      | Identity of { t : t }
    [@@deriving sexp]

    let rec of_complete (complete : complete) =
      match complete.kind with
      | Return { value } -> Return { value = Value.Minimal.of_complete value }
      | Leaf01 { input } -> Leaf01 { input = Value.Minimal.of_complete input }
      | Leaf1 { input } -> Leaf1 { input = Value.Minimal.of_complete input }
      | Leaf0 -> Leaf0
      | Leaf_incr { input } -> Leaf_incr { input = Value.Minimal.of_complete input }
      | Model_cutoff { t } -> Model_cutoff { t = of_complete t }
      | Sub { from; via; into } ->
        Sub { from = of_complete from; via; into = of_complete into }
      | Store { id; value; inner } ->
        Store { id; value = Value.Minimal.of_complete value; inner = of_complete inner }
      | Fetch { id } -> Fetch { id }
      | Assoc { map; key_id; cmp_id; data_id; by } ->
        Assoc
          { map = Value.Minimal.of_complete map
          ; key_id
          ; cmp_id
          ; data_id
          ; by = of_complete by
          }
      | Assoc_simpl { map } -> Assoc_simpl { map = Value.Minimal.of_complete map }
      | Assoc_on { map; io_key_id; model_key_id; model_cmp_id; data_id; by } ->
        Assoc_on
          { map = Value.Minimal.of_complete map
          ; io_key_id
          ; model_key_id
          ; model_cmp_id
          ; data_id
          ; by = of_complete by
          }
      | Switch { match_; arms } ->
        Switch
          { match_ = Value.Minimal.of_complete match_
          ; arms = List.map arms ~f:of_complete
          }
      | Lazy { t = None } -> Lazy { t = None }
      | Lazy { t = Some t } -> Lazy { t = Some (of_complete t) }
      | Wrap { model_id; inject_id; inner } ->
        Wrap { model_id; inject_id; inner = of_complete inner }
      | With_model_resetter { inner; reset_id } ->
        With_model_resetter { inner = of_complete inner; reset_id }
      | Path -> Path
      | Lifecycle { value } -> Lifecycle { value = Value.Minimal.of_complete value }
      | Identity { t } -> Identity { t = of_complete t }
    ;;
  end

  let minimal_sexp_of_t t = Minimal.(of_complete t |> sexp_of_t)

  let inputs (t : t) =
    match t.kind with
    | Return { value } -> [ value ]
    | Leaf01 { input } -> [ input ]
    | Leaf1 { input } -> [ input ]
    | Leaf0 -> []
    | Leaf_incr { input } -> [ input ]
    | Model_cutoff _ -> []
    | Sub { from = _; via = _; into = _ } -> []
    | Store { value; id = _; inner = _ } -> [ value ]
    | Fetch { id = _ } -> []
    | Assoc { map; key_id = _; cmp_id = _; data_id = _; by = _ } -> [ map ]
    | Assoc_simpl { map } -> [ map ]
    | Assoc_on
        { map; io_key_id = _; model_key_id = _; model_cmp_id = _; data_id = _; by = _ } ->
      [ map ]
    | Switch { match_; arms = _ } -> [ match_ ]
    | Lazy _ -> []
    | Wrap { model_id = _; inject_id = _; inner = _ } -> []
    | With_model_resetter _ -> []
    | Path -> []
    | Lifecycle { value } -> [ value ]
    | Identity _ -> []
  ;;

  let children (t : t) =
    match t.kind with
    | Return _ -> []
    | Leaf01 { input = _ } -> []
    | Leaf1 { input = _ } -> []
    | Leaf0 -> []
    | Leaf_incr { input = _ } -> []
    | Model_cutoff { t } -> [ t ]
    | Sub { from; via = _; into } -> [ from; into ]
    | Store { id = _; value = _; inner : t } -> [ inner ]
    | Fetch { id = _ } -> []
    | Assoc { map = _; key_id = _; cmp_id = _; data_id = _; by } -> [ by ]
    | Assoc_simpl { map = _ } -> []
    | Assoc_on
        { map = _; io_key_id = _; model_key_id = _; model_cmp_id = _; data_id = _; by } ->
      [ by ]
    | Switch { match_ = _; arms = _ } -> []
    | Lazy { t = None } -> []
    | Lazy { t = Some t } -> [ t ]
    | Wrap { model_id = _; inject_id = _; inner } -> [ inner ]
    | With_model_resetter { inner; reset_id = _ } -> [ inner ]
    | Path -> []
    | Lifecycle _ -> []
    | Identity { t } -> [ t ]
  ;;
end

include struct
  [@@@warning
    "-30"
    (* disabling [duplicate-definitions] warning which is prompted here due to
       type sharing names for their fields which is a requisite for type equality
       with the original type defined within modules. *)]

  type node_path = Node_path.t
  type source_code_position = Source_code_position.Stable.V1.t
  type id = Id.t
  type 'a lazy_ = 'a Lazy.t

  type computation = Computation0.t =
    { node_path : node_path lazy_
    ; kind : computation_kind
    ; here : source_code_position option
    }

  and computation_kind = Computation0.kind =
    | Return of { value : value }
    | Leaf01 of { input : value }
    | Leaf1 of { input : value }
    | Leaf0
    | Leaf_incr of { input : value }
    | Model_cutoff of { t : computation }
    | Sub of
        { from : computation
        ; via : id
        ; into : computation
        }
    | Store of
        { id : id
        ; value : value
        ; inner : computation
        }
    | Fetch of { id : id }
    | Assoc of
        { map : value
        ; key_id : id
        ; cmp_id : id
        ; data_id : id
        ; by : computation
        }
    | Assoc_on of
        { map : value
        ; io_key_id : id
        ; model_key_id : id
        ; model_cmp_id : id
        ; data_id : id
        ; by : computation
        }
    | Assoc_simpl of { map : value }
    | Switch of
        { match_ : value
        ; arms : computation list
        }
    | Lazy of { t : computation option }
    | Wrap of
        { model_id : id
        ; inject_id : id
        ; inner : computation
        }
    | With_model_resetter of
        { reset_id : id
        ; inner : computation
        }
    | Path
    | Lifecycle of { value : value }
    | Identity of { t : computation }

  and value = Value.t =
    { node_path : node_path lazy_
    ; kind : value_kind
    ; here : source_code_position option
    ; id : id
    }

  and value_kind = Value.kind =
    | Constant
    | Exception
    | Incr
    | Named
    | Cutoff of
        { t : value
        ; added_by_let_syntax : bool
        }
    | Mapn of { inputs : value list }
  [@@deriving traverse_map, traverse_fold]
end

module Traverse = struct
  class ['acc] fold' =
    object
      inherit ['acc] fold
      method source_code_position _ = Fn.id
      method option f o acc = Option.fold ~init:acc ~f:(Fn.flip f) o
      method node_path _ = Fn.id
      method list f l acc = List.fold ~init:acc ~f:(Fn.flip f) l

      method lazy_ _ _ =
        (* NOTE: We do not force the lazy; thus the fold does not enter the lazy by
           default.*)
        Fn.id

      method id _ = Fn.id
      method bool _ = Fn.id
    end

  class ['acc] fold = ['acc] fold'

  class map' =
    object
      inherit map
      method source_code_position = Fn.id
      method option f x = Option.map ~f x
      method node_path = Fn.id
      method list f x = List.map ~f x
      method lazy_ f x = Lazy.map ~f x
      method id = Fn.id
      method bool = Fn.id
    end

  class map = map'
end

module Computation = struct
  include Computation0

  let sanitize_for_testing (t : t) =
    let min_uid =
      let find_minimum_ids =
        object
          inherit [int] Traverse.fold as super
          method! id id min_uid = super#id id (min min_uid id)
        end
      in
      find_minimum_ids#computation t Int.max_value
    in
    let min_uid = if min_uid = Int.max_value then 0 else min_uid in
    let replace_old_uids_with_sanitized_ones =
      object
        inherit Traverse.map as super
        method! id id = super#id (id - min_uid)
      end
    in
    replace_old_uids_with_sanitized_ones#computation t
  ;;
end
