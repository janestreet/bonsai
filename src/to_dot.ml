open! Core_kernel
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
    ; type_id_to_name : (Type_equal.Id.Uid.t, Id.t) Hashtbl.t
    ; const_id_to_name : Id.t Value.Constant_id.Table.t
    ; buffer : Buffer.t
    }
end

module Kind = struct
  type t =
    | Computation of string
    | Leaf of
        { kind : string
        ; name : string
        }
    | Value of string
    | Subst

  let basic_shape ?(other = "") ~shape ~label ~color () =
    [%string
      {|[ style=filled, shape = "%{shape}", label = "%{label}"; fillcolor = "%{color}"; %{other}]|}]
  ;;

  let to_style = function
    | Computation kind -> basic_shape ~shape:"Mrecord" ~label:kind ~color:"#86E3CE" ()
    | Leaf { kind; name } ->
      basic_shape
        ~shape:"Mrecord"
        ~label:[%string "{%{kind}|%{name}}"]
        ~color:"#D0E6A5"
        ()
    | Value kind -> basic_shape ~shape:"oval" ~label:kind ~color:"#FFDD94" ()
    | Subst ->
      basic_shape
        ~shape:"circle"
        ~label:""
        ~color:"#FFFFFF"
        ~other:"width=.1, height=.1"
        ()
  ;;
end

let register state kind name =
  let s = [%string "%{name}_%{state.State.id#Int}"] in
  Buffer.add_string state.State.buffer s;
  Buffer.add_char state.State.buffer ' ';
  Buffer.add_string state.State.buffer (Kind.to_style kind);
  Buffer.add_char state.State.buffer '\n';
  state.id <- state.id + 1;
  Id.of_string s
;;

(* Connects [from] to [to_] with an arrow.  If [to_] is a "named" value (i.e.
   it is a Value.t produced by [sub]), the arrow-head on the arrow is removed. *)
let arrow state ~from ~to_ =
  let arrow_string = if Id.is_named to_ then " [dir=none];" else ";" in
  bprintf
    state.State.buffer
    "%s -> %s%s\n"
    (Id.to_string from)
    (Id.to_string to_)
    arrow_string
;;

(* [arrow_from_many state ~to [a;b;c;d]]  is the same as
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
  let name = Type_equal.Id.uid name in
  Hashtbl.find_or_add state.State.type_id_to_name name ~default:(fun () ->
    register state shape "named")
;;

let register_const state shape id =
  Hashtbl.find_or_add state.State.const_id_to_name id ~default:(fun () ->
    register state shape "const")
;;

let rec follow_value : type a. State.t -> a Value.t -> Id.t =
  fun state value ->
  let register s = register state (Kind.Value s) s in
  let register_named = register_named state Kind.Subst in
  let register_const = register_const state (Kind.Value "const") in
  match value with
  | Value.Constant (_, id) -> register_const id
  | Incr _ -> register "incr"
  | Named name -> register_named name
  | Cutoff { t; _ } ->
    let me = register "cutoff" in
    let them = follow_value state t in
    arrow state ~from:them ~to_:me;
    me
  | Map { t; _ } ->
    let me = register "map" in
    let them = follow_value state t in
    arrow state ~from:them ~to_:me;
    me
  | Map2 { t1; t2; _ } ->
    arrow_from_many
      state
      [ follow_value state t1; follow_value state t2 ]
      ~to_:(register "map2")
  | Map3 { t1; t2; t3; _ } ->
    arrow_from_many
      state
      [ follow_value state t1; follow_value state t2; follow_value state t3 ]
      ~to_:(register "map3")
  | Map4 { t1; t2; t3; t4; _ } ->
    arrow_from_many
      state
      [ follow_value state t1
      ; follow_value state t2
      ; follow_value state t3
      ; follow_value state t4
      ]
      ~to_:(register "map4")
  | Map5 { t1; t2; t3; t4; t5; _ } ->
    arrow_from_many
      state
      [ follow_value state t1
      ; follow_value state t2
      ; follow_value state t3
      ; follow_value state t4
      ; follow_value state t5
      ]
      ~to_:(register "map5")
  | Map6 { t1; t2; t3; t4; t5; t6; _ } ->
    arrow_from_many
      state
      [ follow_value state t1
      ; follow_value state t2
      ; follow_value state t3
      ; follow_value state t4
      ; follow_value state t5
      ; follow_value state t6
      ]
      ~to_:(register "map6")
  | Map7 { t1; t2; t3; t4; t5; t6; t7; _ } ->
    arrow_from_many
      state
      [ follow_value state t1
      ; follow_value state t2
      ; follow_value state t3
      ; follow_value state t4
      ; follow_value state t5
      ; follow_value state t6
      ; follow_value state t7
      ]
      ~to_:(register "map7")
  | Both (t1, Both (t2, Both (t3, Both (t4, Both (t5, Both (t6, t7)))))) ->
    arrow_from_many
      state
      [ follow_value state t1
      ; follow_value state t2
      ; follow_value state t3
      ; follow_value state t4
      ; follow_value state t5
      ; follow_value state t6
      ; follow_value state t7
      ]
      ~to_:(register "map7")
  | Both (t1, Both (t2, Both (t3, Both (t4, Both (t5, t6))))) ->
    arrow_from_many
      state
      [ follow_value state t1
      ; follow_value state t2
      ; follow_value state t3
      ; follow_value state t4
      ; follow_value state t5
      ; follow_value state t6
      ]
      ~to_:(register "map6")
  | Both (t1, Both (t2, Both (t3, Both (t4, t5)))) ->
    arrow_from_many
      state
      [ follow_value state t1
      ; follow_value state t2
      ; follow_value state t3
      ; follow_value state t4
      ; follow_value state t5
      ]
      ~to_:(register "map5")
  | Both (t1, Both (t2, Both (t3, t4))) ->
    arrow_from_many
      state
      [ follow_value state t1
      ; follow_value state t2
      ; follow_value state t3
      ; follow_value state t4
      ]
      ~to_:(register "map4")
  | Both (t1, Both (t2, t3)) ->
    arrow_from_many
      state
      [ follow_value state t1; follow_value state t2; follow_value state t3 ]
      ~to_:(register "map3")
  | Both (t1, t2) ->
    arrow_from_many
      state
      [ follow_value state t1; follow_value state t2 ]
      ~to_:(register "map2")
;;

let rec follow_computation
  : type model action result. State.t -> (model, action, result) Computation.t -> Id.t
  =
  fun state computation ->
  let register_computation kind = register state (Kind.Computation kind) kind in
  match computation with
  | Return value ->
    let me = register_computation "read" in
    arrow state ~from:(follow_value state value) ~to_:me;
    me
  | Leaf { input; kind; name; _ } ->
    let me = register state (Kind.Leaf { kind; name }) "leaf" in
    (match input with
     | Value.Constant _ -> me
     | Value.Map2 { t1; t2; f = _ } ->
       arrow_from_many state [ follow_value state t1; follow_value state t2 ] ~to_:me
     | Value.Map3 { t1; t2; t3; f = _ } ->
       arrow_from_many
         state
         [ follow_value state t1; follow_value state t2; follow_value state t3 ]
         ~to_:me
     | Value.Map4 { t1; t2; t3; t4; f = _ } ->
       arrow_from_many
         state
         [ follow_value state t1
         ; follow_value state t2
         ; follow_value state t3
         ; follow_value state t4
         ]
         ~to_:me
     | Value.Map5 { t1; t2; t3; t4; t5; f = _ } ->
       arrow_from_many
         state
         [ follow_value state t1
         ; follow_value state t2
         ; follow_value state t3
         ; follow_value state t4
         ; follow_value state t5
         ]
         ~to_:me
     | Value.Map6 { t1; t2; t3; t4; t5; t6; f = _ } ->
       arrow_from_many
         state
         [ follow_value state t1
         ; follow_value state t2
         ; follow_value state t3
         ; follow_value state t4
         ; follow_value state t5
         ; follow_value state t6
         ]
         ~to_:me
     | Value.Map7 { t1; t2; t3; t4; t5; t6; t7; f = _ } ->
       arrow_from_many
         state
         [ follow_value state t1
         ; follow_value state t2
         ; follow_value state t3
         ; follow_value state t4
         ; follow_value state t5
         ; follow_value state t6
         ; follow_value state t7
         ]
         ~to_:me
     | input ->
       arrow state ~from:(follow_value state input) ~to_:me;
       me)
  | Leaf_incr _ -> register_computation "leaf_incr"
  | Path -> register_computation "path"
  | Model_cutoff { t; model = _ } ->
    let me = register_computation "model-cutoff" in
    arrow state ~from:(follow_computation state t) ~to_:me;
    me
  | Subst { from = Return from; via; into } ->
    arrow state ~from:(follow_value state from) ~to_:(register_named state Kind.Subst via);
    follow_computation state into
  | Subst { from; via; into } ->
    arrow
      state
      ~from:(follow_computation state from)
      ~to_:(register_named state Kind.Subst via);
    follow_computation state into
  | Assoc { map; by; _ } ->
    let me = register_computation "assoc" in
    arrow state ~from:(follow_computation state by) ~to_:me;
    arrow state ~from:(follow_value state map) ~to_:me;
    me
  | Assoc_simpl { map; _ } ->
    let me = register_computation "assoc_simpl" in
    arrow state ~from:(follow_value state map) ~to_:me;
    me
  | Enum { which; out_of; _ } ->
    let me = register_computation "enum" in
    arrow state ~from:(follow_value state which) ~to_:me;
    Map.iter out_of ~f:(fun (Computation.T { t; _ }) ->
      arrow state ~from:(follow_computation state t) ~to_:me);
    me
  | Lazy _ -> register_computation "lazy"
  | Wrap { inner; model_id = _; inject_id = _; apply_action = _ } ->
    let me = register_computation "wrap" in
    arrow state ~from:(follow_computation state inner) ~to_:me;
    me
  | With_model_resetter { t; default_model = _ } ->
    let me = register_computation "with-model-resetter" in
    arrow state ~from:(follow_computation state t) ~to_:me;
    me
;;

let to_dot (Computation.T { t; _ }) =
  let state =
    { State.id = 0
    ; buffer = Buffer.create 2014
    ; type_id_to_name = Hashtbl.create (module Type_equal.Id.Uid)
    ; const_id_to_name = Value.Constant_id.Table.create ()
    }
  in
  let _root : Id.t = follow_computation state t in
  sprintf "digraph {\n%s}" (Buffer.contents state.buffer)
;;
