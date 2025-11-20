open! Core
open! Import

module Id : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val is_named : t -> bool
end = struct
  include String

  let is_named = String.is_prefix ~prefix:"named_"
end

module State = struct
  type t =
    { mutable id : int
    ; type_id_to_name : (Skeleton.Id.t, Id.t) Hashtbl.t
    ; buffer : Buffer.t
    }
end

module Kind = struct
  type t =
    | Computation of string
    | Leaf
    | Value of
        { kind : string
        ; here : Source_code_position.t
        }
    | Subst of Source_code_position.t
    | Reset_id
    | Dyn
    | Fix_id

  let basic_shape ?(other = "") ?tooltip ~shape ~label ~color () =
    let tooltip =
      match tooltip with
      | Some tooltip -> [%string ", tooltip = \"%{tooltip}\""]
      | None -> ""
    in
    [%string
      {|[ style=filled%{tooltip}, shape = "%{shape}", label = "%{label}"; fillcolor = "%{color}"; %{other}]|}]
  ;;

  let to_style = function
    | Computation kind -> basic_shape ~shape:"Mrecord" ~label:kind ~color:"#86E3CE" ()
    | Leaf ->
      basic_shape
        ~shape:"Mrecord"
        ~tooltip:"leaf"
        ~label:"{state machine}"
        ~color:"#D0E6A5"
        ()
    | Value { kind; here } ->
      let color = "#FFDD94" in
      let tooltip = Source_code_position.to_string here in
      basic_shape ~tooltip ~shape:"oval" ~label:kind ~color ()
    | Subst here ->
      let tooltip = Source_code_position.to_string here in
      basic_shape
        ~tooltip
        ~shape:"oval"
        ~label:"subst"
        ~color:"#FFFFFF"
        ~other:"width=.1, height=.1"
        ()
    | Reset_id | Dyn | Fix_id ->
      basic_shape
        ~shape:"circle"
        ~label:""
        ~color:"#000000"
        ~other:"width=.1, height=.1"
        ()
  ;;
end

let register state kind name =
  assert (String.for_all name ~f:(fun c -> Char.is_alphanum c || Char.equal c '_'));
  let s = [%string "%{name}_%{state.State.id#Int}"] in
  Buffer.add_string state.State.buffer s;
  Buffer.add_char state.State.buffer ' ';
  Buffer.add_string state.State.buffer (Kind.to_style kind);
  Buffer.add_char state.State.buffer '\n';
  state.id <- state.id + 1;
  Id.of_string s
;;

(* Connects [from] to [to_] with an arrow. If [to_] is a "named" value (i.e. it is a
   Value.t produced by [sub]), the arrow-head on the arrow is removed. *)
let arrow state ~from ~to_ =
  let arrow_string = if Id.is_named to_ then " [dir=none];" else ";" in
  bprintf
    state.State.buffer
    "%s -> %s%s\n"
    (Id.to_string from)
    (Id.to_string to_)
    arrow_string
;;

(* [arrow_from_many state ~to [a;b;c;d]] is the same as
   {[
     arrow ~from:a ~to_;
     arrow ~from:b ~to_;
     arrow ~from:c ~to_;
     to_
   ]} *)
let arrow_from_many state ~to_ l =
  List.iter l ~f:(fun from -> arrow state ~from ~to_);
  to_
;;

let register_named state shape name =
  Hashtbl.find_or_add state.State.type_id_to_name name ~default:(fun () ->
    register state shape "named")
;;

let rec follow_skeleton_value state { Skeleton.Value.kind = value; here; node_path = _ } =
  let register s = register state (Kind.Value { kind = s; here }) s in
  match value with
  | Skeleton.Value.Constant -> register "const"
  | Exception -> register "exception"
  | Incr -> register "incr"
  | Named id -> register_named state (Kind.Subst here) id
  | Cutoff { t; added_by_let_syntax = _ } ->
    let me = register "cutoff" in
    let them = follow_skeleton_value state t in
    arrow state ~from:them ~to_:me;
    me
  | Mapn { inputs } ->
    arrow_from_many
      state
      (List.map inputs ~f:(fun value -> follow_skeleton_value state value))
      ~to_:(register "mapn")
;;

let follow_dynamic_skeleton_leaf state (input : Skeleton.Value.t) =
  let me = register state Kind.Leaf "leaf" in
  match input.kind with
  | Skeleton.Value.Constant -> me
  | Mapn { inputs } ->
    arrow_from_many
      state
      (List.map inputs ~f:(fun value -> follow_skeleton_value state value))
      ~to_:me
  | _ ->
    arrow state ~from:(follow_skeleton_value state input) ~to_:me;
    me
;;

let rec follow_skeleton_computation state (computation : Skeleton.Computation.t) =
  let register_computation kind = register state (Kind.Computation kind) kind in
  let here = computation.here in
  match computation.kind with
  | Skeleton.Computation.Return { value } ->
    let me = register_computation "read" in
    arrow state ~from:(follow_skeleton_value state value) ~to_:me;
    me
  | Fetch { id; _ } ->
    let me = register_computation "fetch" in
    arrow state ~from:(register_named state Kind.Dyn id) ~to_:me;
    me
  | Leaf0 -> register state Kind.Leaf "leaf0"
  | Leaf01 { input; _ } -> follow_dynamic_skeleton_leaf state input
  | Leaf1 { input; _ } -> follow_dynamic_skeleton_leaf state input
  | Leaf_incr _ -> register_computation "leaf_incr"
  | Path -> register_computation "path"
  | Lifecycle { value } ->
    let me = register_computation "life_cycle" in
    arrow state ~from:(follow_skeleton_value state value) ~to_:me;
    me
  | Model_cutoff { t } ->
    let me = register_computation "model_cutoff" in
    arrow state ~from:(follow_skeleton_computation state t) ~to_:me;
    me
  | Sub { from; via; into; invert_lifecycles = _ } ->
    arrow
      state
      ~from:(follow_skeleton_computation state from)
      ~to_:(register_named state (Kind.Subst computation.here) via);
    follow_skeleton_computation state into
  | Assoc { map; by; _ } ->
    let me = register_computation "assoc" in
    arrow state ~from:(follow_skeleton_computation state by) ~to_:me;
    arrow state ~from:(follow_skeleton_value state map) ~to_:me;
    me
  | Fix_define { result; input_id; initial_input; fix_id } ->
    let me = register_named state Kind.Fix_id fix_id in
    arrow
      state
      ~from:(follow_skeleton_value state initial_input)
      ~to_:(register_named state (Kind.Subst here) input_id);
    arrow state ~from:(follow_skeleton_computation state result) ~to_:me;
    me
  | Fix_recurse { input; input_id = _; fix_id } ->
    let me = register_computation "fix_recurse" in
    arrow
      state
      ~from:(follow_skeleton_value state input)
      ~to_:(register_named state Kind.Fix_id fix_id);
    me
  | Assoc_on { map; by; _ } ->
    let me = register_computation "assoc_on" in
    arrow state ~from:(follow_skeleton_computation state by) ~to_:me;
    arrow state ~from:(follow_skeleton_value state map) ~to_:me;
    me
  | Assoc_simpl { map; _ } ->
    let me = register_computation "assoc_simpl" in
    arrow state ~from:(follow_skeleton_value state map) ~to_:me;
    me
  | Switch { match_; arms; _ } ->
    let me = register_computation "switch" in
    arrow state ~from:(follow_skeleton_value state match_) ~to_:me;
    List.iter arms ~f:(fun t ->
      arrow state ~from:(follow_skeleton_computation state t) ~to_:me);
    me
  | Lazy { t = None } -> register_computation "lazy"
  | Lazy { t = Some t } ->
    let me = register_computation "forced_lazy" in
    arrow state ~from:(follow_skeleton_computation state t) ~to_:me;
    me
  | Wrap { inner; model_id = _; inject_id = _ } ->
    let me = register_computation "wrap" in
    arrow state ~from:(follow_skeleton_computation state inner) ~to_:me;
    me
  | With_model_resetter { reset_id; inner } ->
    let me = register_computation "with_model_resetter" in
    arrow state ~from:me ~to_:(register_named state Kind.Reset_id reset_id);
    arrow state ~from:(follow_skeleton_computation state inner) ~to_:me;
    me
  | Store { id; value; inner } ->
    let me = register_computation "dyn_set" in
    arrow state ~from:(follow_skeleton_value state value) ~to_:me;
    arrow state ~from:me ~to_:(register_named state Kind.Dyn id);
    follow_skeleton_computation state inner
  | Identity { t } ->
    let me = register_computation "identity" in
    arrow state ~from:(follow_skeleton_computation state t) ~to_:me;
    me
  | Computation_watcher { inner; _ } ->
    let me = register_computation "computation_watcher" in
    arrow state ~from:(follow_skeleton_computation state inner) ~to_:me;
    me
;;

let to_dot ?(pre_process = true) t =
  let state =
    { State.id = 0
    ; buffer = Buffer.create 2014
    ; type_id_to_name = Hashtbl.create (module Skeleton.Id)
    }
  in
  let computation = if pre_process then Pre_process.pre_process t else t in
  let skeleton_computation = Skeleton.Computation.of_computation computation in
  let _root : Id.t = follow_skeleton_computation state skeleton_computation in
  sprintf "digraph {\n%s}" (Buffer.contents state.buffer)
;;
