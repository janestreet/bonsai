open! Core
open! Import

let finalize node_path_builder = lazy (node_path_builder |> Node_path.finalize)

module Bonsai_value = Value
module Bonsai_computation = Computation

module Id : sig
  type t [@@deriving compare, hash, sexp_of]

  val to_string : t -> string
  val of_type_id : _ Type_equal.Id.t -> t
  val of_model_type_id : _ Meta.Model.Type_id.t -> t
  val of_int_for_testing : int -> t
end = struct
  type t =
    | Type of Type_equal.Id.Uid.t
    | Test of int
  [@@deriving compare, hash, sexp_of]

  let to_string t =
    match t with
    | Type uid -> Type_equal.Id.Uid.sexp_of_t uid |> Sexp.to_string
    | Test int -> Int.to_string int
  ;;

  let of_type_id id = Type (Type_equal.Id.uid id)
  let of_model_type_id id = id |> Meta.Model.Type_id.to_type_id |> of_type_id
  let of_int_for_testing int = Test int
end

module Value = struct
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.Stable.V1.t
    }

  and kind =
    | Constant
    | Exception
    | Incr
    | Named of Id.t
    | Cutoff of
        { t : t
        ; added_by_let_syntax : bool
        }
    | Mapn of { inputs : t list }
  [@@deriving sexp_of]

  module Minimal = struct
    type nonrec complete = t

    type t =
      | Constant
      | Exception
      | Incr
      | Named of { uid : Id.t }
      | Cutoff of
          { t : t
          ; added_by_let_syntax : bool
          }
      | Mapn of { inputs : t list }
    [@@deriving sexp_of]

    let rec of_complete (complete : complete) =
      match complete.kind with
      | Constant -> Constant
      | Exception -> Exception
      | Incr -> Incr
      | Named id -> Named { uid = id }
      | Cutoff { t; added_by_let_syntax } ->
        Cutoff { t = of_complete t; added_by_let_syntax }
      | Mapn { inputs } -> Mapn { inputs = List.map inputs ~f:of_complete }
    ;;
  end

  let minimal_sexp_of_t t = Minimal.(of_complete t |> sexp_of_t)

  let inputs { kind; node_path = _; here = _ } =
    match kind with
    | Constant | Incr | Named _ | Exception -> []
    | Cutoff { t; added_by_let_syntax = _ } -> [ t ]
    | Mapn { inputs } -> inputs
  ;;

  let of_value' : initial_path:Node_path.builder -> 'a Value.t -> t Trampoline.t =
    fun ~initial_path value ->
    let rec helper : type a. current_path:Node_path.builder -> a Value.t -> t Trampoline.t
      =
      let module Packed_value = struct
        type t = T : 'a Value.t -> t
      end
      in
      let create_mapn_with_choices ~current_path (values : Packed_value.t list) =
        let%map.Trampoline inputs =
          List.mapi values ~f:(fun i (T value) ->
            helper
              ~current_path:
                (Node_path.choice_point current_path (i + 1) |> Node_path.descend)
              value)
          |> Trampoline.all
        in
        Mapn { inputs }
      in
      fun ~current_path { value; here } ->
        let open Trampoline.Let_syntax in
        let%bind kind =
          match value with
          | Constant _ -> return Constant
          | Exception _ -> return Exception
          | Incr _ -> return Incr
          | Named (_, id) -> return (Named (Id.of_type_id id))
          | Cutoff { t; equal = _; added_by_let_syntax } ->
            let%map t =
              helper
                ~current_path:(Node_path.choice_point current_path 1 |> Node_path.descend)
                t
            in
            Cutoff { t; added_by_let_syntax }
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
        return { node_path = finalize current_path; here; kind }
    in
    helper ~current_path:initial_path value
  ;;

  let of_value value =
    Trampoline.run (of_value' ~initial_path:(Node_path.descend Node_path.empty) value)
  ;;

  let rec to_string_hum { node_path = _; kind; here = _ } =
    match kind with
    | Exception -> sprintf "exception"
    | Constant -> sprintf "constant"
    | Incr -> "incr"
    | Named id -> sprintf "x%s" (Id.to_string id)
    | Cutoff { t; added_by_let_syntax = _ } -> sprintf "(cutoff %s)" (to_string_hum t)
    | Mapn { inputs } ->
      sprintf "(mapn %s)" (String.concat ~sep:" " (List.map inputs ~f:to_string_hum))
  ;;
end

module Computation0 = struct
  type t =
    { node_path : Node_path.t Lazy.t
    ; kind : kind
    ; here : Source_code_position.Stable.V1.t
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
        ; via : Id.t
        ; into : t
        ; (* [invert_lifecycles] is quite rare, and adds clutter to expect text outputs,
             so we don't include it in skeleton serializations by default. *)
          invert_lifecycles : bool [@sexp.default false] [@sexp_drop_default.equal]
        }
    | Store of
        { id : Id.t
        ; value : Value.t
        ; inner : t
        }
    | Fetch of { id : Id.t }
    | Assoc of
        { map : Value.t
        ; key_id : Id.t
        ; cmp_id : Id.t
        ; data_id : Id.t
        ; by : t
        }
    | Assoc_on of
        { map : Value.t
        ; io_key_id : Id.t
        ; model_key_id : Id.t
        ; model_cmp_id : Id.t
        ; data_id : Id.t
        ; by : t
        }
    | Assoc_simpl of { map : Value.t }
    | Switch of
        { match_ : Value.t
        ; arms : t list
        }
    | Lazy of { t : t option }
    | Fix_define of
        { result : t
        ; initial_input : Value.t
        ; fix_id : Id.t
        ; input_id : Id.t
        }
    | Fix_recurse of
        { input : Value.t
        ; input_id : Id.t
        ; fix_id : Id.t
        }
    | Wrap of
        { model_id : Id.t
        ; inject_id : Id.t
        ; inner : t
        }
    | With_model_resetter of
        { reset_id : Id.t
        ; inner : t
        }
    | Path
    | Lifecycle of { value : Value.t }
    | Identity of { t : t }
    | Computation_watcher of
        { inner : t
        ; free_vars : Id.t list
        }
  [@@deriving sexp_of]

  let of_computation : 'result Computation.t -> t =
    fun computation ->
    let rec helper
      : type result.
        current_path:Node_path.builder -> result Computation.t -> t Trampoline.t
      =
      let open Trampoline.Let_syntax in
      fun ~current_path computation ->
        let choice_point choice =
          Node_path.choice_point current_path choice |> Node_path.descend
        in
        let node_path = finalize current_path in
        match computation with
        | Return { value; here } ->
          let%map kind =
            let%map value =
              Value.of_value' ~initial_path:(Node_path.descend current_path) value
            in
            Return { value }
          in
          { node_path; here; kind }
        | Leaf1 { input; here; _ } ->
          let%map input =
            Value.of_value' ~initial_path:(Node_path.descend current_path) input
          in
          { node_path; here; kind = Leaf1 { input } }
        | Leaf0 { here; _ } -> return { node_path; here; kind = Leaf0 }
        | Leaf_incr { input; here; _ } ->
          let%map input =
            Value.of_value' ~initial_path:(Node_path.descend current_path) input
          in
          { node_path; here; kind = Leaf_incr { input } }
        | Sub { from; via; into; here; invert_lifecycles } ->
          let%map from = helper ~current_path:(choice_point 1) from
          and into = helper ~current_path:(choice_point 2) into in
          let kind = Sub { from; via = Id.of_type_id via; into; invert_lifecycles } in
          { node_path; here; kind }
        | Store { id; value; inner; here } ->
          let%map value = Value.of_value' ~initial_path:(choice_point 1) value
          and inner = helper ~current_path:(choice_point 2) inner in
          let kind = Store { id = Id.of_type_id id; value; inner } in
          { node_path; here; kind }
        | Fetch { id; here; _ } ->
          let kind = Fetch { id = Id.of_type_id id } in
          return { node_path; here; kind }
        | Assoc { map; key_id; cmp_id; data_id; by; here; _ } ->
          let%map map = Value.of_value' ~initial_path:(choice_point 1) map
          and by = helper ~current_path:(choice_point 2) by in
          let kind =
            Assoc
              { map
              ; key_id = Id.of_type_id key_id
              ; cmp_id = Id.of_type_id cmp_id
              ; data_id = Id.of_type_id data_id
              ; by
              }
          in
          { node_path; here; kind }
        | Assoc_simpl { map; here; _ } ->
          let%map map =
            Value.of_value' ~initial_path:(Node_path.descend current_path) map
          in
          let kind = Assoc_simpl { map } in
          { node_path; here; kind }
        | Assoc_on { map; io_key_id; model_key_id; model_cmp_id; data_id; by; here; _ } ->
          let%map map = Value.of_value' ~initial_path:(choice_point 1) map
          and by = helper ~current_path:(choice_point 2) by in
          let kind =
            Assoc_on
              { map
              ; io_key_id = Id.of_type_id io_key_id
              ; model_key_id = Id.of_type_id model_key_id
              ; model_cmp_id = Id.of_type_id model_cmp_id
              ; data_id = Id.of_type_id data_id
              ; by
              }
          in
          { node_path; here; kind }
        | Switch { match_; arms; here; _ } ->
          (* This form of node_path generation is necessary to achive the same traversal
             as the one in [transform.mli] so that both node paths are in sync. *)
          let index = ref 1 in
          let%map match_ = Value.of_value' ~initial_path:(choice_point 1) match_
          and arms =
            Map.fold arms ~init:[] ~f:(fun ~key:_ ~data:computation acc ->
              incr index;
              helper ~current_path:(choice_point !index) computation :: acc)
            |> List.rev
            |> Trampoline.all
          in
          let kind = Switch { match_; arms } in
          { node_path; here; kind }
        | Fix_define { result; initial_input; fix_id; input_id; here } ->
          let%map initial_input =
            Value.of_value' ~initial_path:(choice_point 1) initial_input
          and result = helper ~current_path:(choice_point 2) result in
          let kind =
            Fix_define
              { initial_input
              ; fix_id = Id.of_type_id (Fix_id.type_id fix_id)
              ; input_id = Id.of_type_id input_id
              ; result
              }
          in
          { node_path; here; kind }
        | Fix_recurse { fix_id; input_id; input; here } ->
          let%map input = Value.of_value' ~initial_path:(choice_point 1) input in
          let kind =
            Fix_recurse
              { input
              ; input_id = Id.of_type_id input_id
              ; fix_id = Id.of_type_id (Fix_id.type_id fix_id)
              }
          in
          { node_path; here; kind }
        | Lazy { t; here } ->
          let%map potentially_evaluated =
            (* If lazy has already been forced, then the forced value is stored. *)
            match Lazy.is_val t with
            | false -> return None
            | true ->
              Lazy.force t
              |> helper ~current_path:(Node_path.descend current_path)
              |> Trampoline.map ~f:Option.some
          in
          { node_path; here; kind = Lazy { t = potentially_evaluated } }
        | With_model_resetter { reset_id; inner; here } ->
          let%map inner = helper ~current_path:(Node_path.descend current_path) inner in
          let kind = With_model_resetter { reset_id = Id.of_type_id reset_id; inner } in
          { node_path; here; kind }
        | Wrap { inner; wrapper_model; inject_id; here; _ } ->
          let%map inner = helper ~current_path:(Node_path.descend current_path) inner in
          let kind =
            Wrap
              { model_id = Id.of_model_type_id wrapper_model.type_id
              ; inject_id = Id.of_type_id inject_id
              ; inner
              }
          in
          { node_path; here; kind }
        | Path { here } -> return { node_path; here; kind = Path }
        | Lifecycle { lifecycle = value; here } ->
          let%map value =
            Value.of_value' ~initial_path:(Node_path.descend current_path) value
          in
          let kind = Lifecycle { value } in
          { node_path; here; kind }
        | Computation_watcher
            { inner
            ; here
            ; free_vars
            ; config = _
            ; queue = _
            ; value_id_observation_definition_positions = _
            ; enable_watcher = _
            } ->
          let%map inner = helper ~current_path:(Node_path.descend current_path) inner in
          let kind =
            Computation_watcher
              { inner
              ; free_vars =
                  Computation_watcher.Type_id_location_map.map_to_list
                    free_vars
                    { f = (fun key _ -> Id.of_type_id key) }
              }
          in
          { node_path; here; kind }
    in
    Trampoline.run (helper ~current_path:(Node_path.descend Node_path.empty) computation)
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
              (* [invert_lifecycles] is quite rare, and adds clutter to expect text
                 outputs, so we don't include it in skeleton serializations by default. *)
          ; invert_lifecycles : bool [@sexp.default false] [@sexp_drop_default.equal]
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
      | Fix_define of
          { result : t
          ; initial_input : Value.Minimal.t
          ; fix_id : Id.t
          ; input_id : Id.t
          }
      | Fix_recurse of
          { input : Value.Minimal.t
          ; fix_id : Id.t
          ; input_id : Id.t
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
      | Computation_watcher of
          { inner : t
          ; free_vars : Id.t list
          }
    [@@deriving sexp_of]

    let rec of_complete (complete : complete) =
      match complete.kind with
      | Return { value } -> Return { value = Value.Minimal.of_complete value }
      | Leaf01 { input } -> Leaf01 { input = Value.Minimal.of_complete input }
      | Leaf1 { input } -> Leaf1 { input = Value.Minimal.of_complete input }
      | Leaf0 -> Leaf0
      | Leaf_incr { input } -> Leaf_incr { input = Value.Minimal.of_complete input }
      | Model_cutoff { t } -> Model_cutoff { t = of_complete t }
      | Sub { from; via; into; invert_lifecycles } ->
        Sub { from = of_complete from; via; into = of_complete into; invert_lifecycles }
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
      | Fix_define { result; initial_input; fix_id; input_id } ->
        Fix_define
          { result = of_complete result
          ; initial_input = Value.Minimal.of_complete initial_input
          ; fix_id
          ; input_id
          }
      | Fix_recurse { input; input_id; fix_id } ->
        Fix_recurse { input = Value.Minimal.of_complete input; fix_id; input_id }
      | Lazy { t = None } -> Lazy { t = None }
      | Lazy { t = Some t } -> Lazy { t = Some (of_complete t) }
      | Wrap { model_id; inject_id; inner } ->
        Wrap { model_id; inject_id; inner = of_complete inner }
      | With_model_resetter { inner; reset_id } ->
        With_model_resetter { inner = of_complete inner; reset_id }
      | Path -> Path
      | Lifecycle { value } -> Lifecycle { value = Value.Minimal.of_complete value }
      | Identity { t } -> Identity { t = of_complete t }
      | Computation_watcher { inner; free_vars } ->
        Computation_watcher { inner = of_complete inner; free_vars }
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
    | Sub { from = _; via = _; into = _; invert_lifecycles = _ } -> []
    | Store { value; id = _; inner = _ } -> [ value ]
    | Fetch { id = _ } -> []
    | Assoc { map; key_id = _; cmp_id = _; data_id = _; by = _ } -> [ map ]
    | Assoc_simpl { map } -> [ map ]
    | Assoc_on
        { map; io_key_id = _; model_key_id = _; model_cmp_id = _; data_id = _; by = _ } ->
      [ map ]
    | Switch { match_; arms = _ } -> [ match_ ]
    | Fix_define { initial_input; _ } -> [ initial_input ]
    | Fix_recurse { input; _ } -> [ input ]
    | Lazy _ -> []
    | Wrap { model_id = _; inject_id = _; inner = _ } -> []
    | With_model_resetter _ -> []
    | Path -> []
    | Lifecycle { value } -> [ value ]
    | Identity _ -> []
    | Computation_watcher _ -> []
  ;;

  let children (t : t) =
    match t.kind with
    | Return _ -> []
    | Leaf01 { input = _ } -> []
    | Leaf1 { input = _ } -> []
    | Leaf0 -> []
    | Leaf_incr { input = _ } -> []
    | Model_cutoff { t } -> [ t ]
    | Sub { from; via = _; into; invert_lifecycles = _ } -> [ from; into ]
    | Store { id = _; value = _; inner : t } -> [ inner ]
    | Fetch { id = _ } -> []
    | Assoc { map = _; key_id = _; cmp_id = _; data_id = _; by } -> [ by ]
    | Assoc_simpl { map = _ } -> []
    | Assoc_on
        { map = _; io_key_id = _; model_key_id = _; model_cmp_id = _; data_id = _; by } ->
      [ by ]
    | Switch { match_ = _; arms = _ } -> []
    | Fix_define { result; _ } -> [ result ]
    | Fix_recurse _ -> []
    | Lazy { t = None } -> []
    | Lazy { t = Some t } -> [ t ]
    | Wrap { model_id = _; inject_id = _; inner } -> [ inner ]
    | With_model_resetter { inner; reset_id = _ } -> [ inner ]
    | Path -> []
    | Lifecycle _ -> []
    | Identity { t } -> [ t ]
    | Computation_watcher { inner; _ } -> [ inner ]
  ;;
end

include struct
  [@@@warning
    "-30"
    (* disabling [duplicate-definitions] warning which is prompted here due to type
       sharing names for their fields which is a requisite for type equality with the
       original type defined within modules. *)]

  type node_path = Node_path.t
  type source_code_position = Source_code_position.Stable.V1.t
  type id = Id.t
  type 'a lazy_ = 'a Lazy.t

  type computation = Computation0.t =
    { node_path : node_path lazy_
    ; kind : computation_kind
    ; here : source_code_position
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
        ; invert_lifecycles : bool
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
    | Fix_define of
        { result : computation
        ; initial_input : value
        ; fix_id : id
        ; input_id : id
        }
    | Fix_recurse of
        { input : value
        ; input_id : id
        ; fix_id : id
        }
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
    | Computation_watcher of
        { inner : computation
        ; free_vars : id list
        }

  and value = Value.t =
    { node_path : node_path lazy_
    ; kind : value_kind
    ; here : source_code_position
    }

  and value_kind = Value.kind =
    | Constant
    | Exception
    | Incr
    | Named of id
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
           default. *)
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
    let table = Hashtbl.create (module Id) in
    let replace id =
      Hashtbl.find_or_add table id ~default:(fun () ->
        Id.of_int_for_testing (Hashtbl.length table))
    in
    let replace_old_uids_with_sanitized_ones =
      object
        inherit Traverse.map as super
        method! id id = super#id (replace id)
      end
    in
    replace_old_uids_with_sanitized_ones#computation t
  ;;
end

module Counts = struct
  module Computation = struct
    type t =
      { return : int
      ; leaf01 : int
      ; leaf1 : int
      ; leaf0 : int
      ; leaf_incr : int
      ; model_cutoff : int
      ; sub : int
      ; store : int
      ; fetch : int
      ; assoc : int
      ; assoc_on : int
      ; assoc_simpl : int
      ; switch : int
      ; fix_define : int
      ; fix_recurse : int
      ; wrap : int
      ; with_model_resetter : int
      ; path : int
      ; lifecycle : int
      ; identity : int
      ; computation_watcher : int
      }
    [@@deriving sexp_of]
  end

  module Value = struct
    type t =
      { constant : int
      ; exception_ : int
      ; incr : int
      ; named : int
      ; cutoff : int
      ; mapn : int
      }
    [@@deriving sexp_of]
  end

  type t =
    { mutable computation : Computation.t
    ; mutable value : Value.t
    }
  [@@deriving sexp_of, fields ~getters]

  let get' t =
    let count_uids =
      object
        inherit [t] Traverse.fold as super

        method! computation_kind t acc =
          let c = acc.computation in
          (match t with
           | Return _ -> acc.computation <- { c with return = c.return + 1 }
           | Leaf01 _ -> acc.computation <- { c with leaf01 = c.leaf01 + 1 }
           | Leaf1 _ -> acc.computation <- { c with leaf1 = c.leaf1 + 1 }
           | Leaf0 -> acc.computation <- { c with leaf0 = c.leaf0 + 1 }
           | Leaf_incr _ -> acc.computation <- { c with leaf_incr = c.leaf_incr + 1 }
           | Model_cutoff _ ->
             acc.computation <- { c with model_cutoff = c.model_cutoff + 1 }
           | Sub _ -> acc.computation <- { c with sub = c.sub + 1 }
           | Store _ -> acc.computation <- { c with store = c.store + 1 }
           | Fetch _ -> acc.computation <- { c with fetch = c.fetch + 1 }
           | Assoc _ -> acc.computation <- { c with assoc = c.assoc + 1 }
           | Assoc_on _ -> acc.computation <- { c with assoc_on = c.assoc_on + 1 }
           | Assoc_simpl _ ->
             acc.computation <- { c with assoc_simpl = c.assoc_simpl + 1 }
           | Switch _ -> acc.computation <- { c with switch = c.switch + 1 }
           | Fix_define _ -> acc.computation <- { c with fix_define = c.fix_define + 1 }
           | Fix_recurse _ ->
             acc.computation <- { c with fix_recurse = c.fix_recurse + 1 }
           | Wrap _ -> acc.computation <- { c with wrap = c.wrap + 1 }
           | With_model_resetter _ ->
             acc.computation <- { c with with_model_resetter = c.with_model_resetter + 1 }
           | Path -> acc.computation <- { c with path = c.path + 1 }
           | Lifecycle _ -> acc.computation <- { c with lifecycle = c.lifecycle + 1 }
           | Identity _ -> acc.computation <- { c with identity = c.identity + 1 }
           | Lazy _ -> ()
           | Computation_watcher _ ->
             acc.computation <- { c with computation_watcher = c.computation_watcher + 1 });
          super#computation_kind t acc

        method! value_kind t acc =
          let v = acc.value in
          (match t with
           | Constant -> acc.value <- { v with constant = v.constant + 1 }
           | Exception -> acc.value <- { v with exception_ = v.exception_ + 1 }
           | Incr -> acc.value <- { v with incr = v.incr + 1 }
           | Named _ -> acc.value <- { v with named = v.named + 1 }
           | Cutoff _ -> acc.value <- { v with cutoff = v.cutoff + 1 }
           | Mapn _ -> acc.value <- { v with mapn = v.mapn + 1 });
          super#value_kind t acc
      end
    in
    count_uids#computation
      t
      { computation =
          { return = 0
          ; leaf01 = 0
          ; leaf1 = 0
          ; leaf0 = 0
          ; leaf_incr = 0
          ; model_cutoff = 0
          ; sub = 0
          ; store = 0
          ; fetch = 0
          ; assoc = 0
          ; assoc_on = 0
          ; assoc_simpl = 0
          ; switch = 0
          ; fix_define = 0
          ; fix_recurse = 0
          ; wrap = 0
          ; with_model_resetter = 0
          ; path = 0
          ; lifecycle = 0
          ; identity = 0
          ; computation_watcher = 0
          }
      ; value =
          { constant = 0; exception_ = 0; incr = 0; named = 0; cutoff = 0; mapn = 0 }
      }
  ;;

  let get ?(pre_process = true) t =
    let computation = if pre_process then Pre_process.pre_process t else t in
    let skeleton_computation = Computation0.of_computation computation in
    get' skeleton_computation
  ;;
end
