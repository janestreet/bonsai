open! Core
open! Import
module Effect = Ui_effect
module Time_source = Time_source
module Apply_action_context = Apply_action_context

module type Enum = Module_types.Enum

module For_bonsai_internal = struct
  let perform_on_exception = ref ignore
  let set_perform_on_exception perform = perform_on_exception := perform
end

module Cont_primitives : sig
  type graph

  (* Main primitives; see above for explanation. *)
  val perform
    :  here:Source_code_position.t
    -> local_ graph
    -> 'a Computation.t
    -> 'a Value.t

  val handle
    :  here:[%call_pos]
    -> f:(local_ graph -> 'a Value.t)
    -> local_ graph
    -> 'a Computation.t

  (* Special-use primitives for getting the global graph, and creating it in the top
     level. *)
  val isolated
    :  local_ graph
    -> here:Source_code_position.t
    -> f:local_ (unit -> 'a Value.t)
    -> 'a Computation.t

  val top_level_handle
    :  here:[%call_pos]
    -> (local_ graph -> 'a Value.t)
    -> 'a Computation.t

  val handle_for_lazy
    :  here:[%call_pos]
    -> (local_ graph -> 'a Value.t)
    -> 'a Computation.t

  val with_global_graph : f:local_ (local_ graph -> 'a) -> no_graph:(unit -> 'a) -> 'a
end = struct
  (* NOTE: We use [Trampoline.t] to avoid stack overflows when this function is applied. *)
  type graph =
    { mutable f : 'a. 'a Computation.t Trampoline.t -> 'a Computation.t Trampoline.t }

  let perform
    : type a. here:Source_code_position.t -> local_ graph -> a Computation.t -> a Value.t
    =
    fun ~here graph -> function
    | Return
        { value = { value = (Named _ | Constant _ | Exception _) as value; _ }; here } ->
      (* Introduce the optimization [let%sub a = return foo in use a] => [use foo] This
         only makes sense if the Value.t being returned is either a constant or an
         already-bound named value, otherwise you risk losing value sharing. *)
      { Value.value; here }
    | computation_to_perform ->
      (* Mint a fresh type-id to hold the result of performing this graph modification *)
      let via : a Type_equal.Id.t = Type_equal.Id.create ~name:"" [%sexp_of: opaque] in
      (* Keep hold of the previous graph-modification function *)
      let old_f : type b. b Computation.t Trampoline.t -> b Computation.t Trampoline.t =
        graph.f
      in
      let new_f computation =
        let new_f : type x. x Computation.t -> x Computation.t Trampoline.t = function
          | Return { value = { value = Named (_, id); _ }; here = _ }
            when Type_equal.Id.same via id ->
            (* introduce the optimization {[ let%sub a = foo bar in return a ]} => {[ foo bar ]} *)
            let T = Type_equal.Id.same_witness_exn via id in
            old_f (Trampoline.return computation_to_perform)
          | eventual_result ->
            (* old_f takes the eventual innermost result, and wraps it in 0+ layers of
               subs. We replace it with a new function that adds another layer to the
               inside. *)
            let computation =
              Computation.Sub
                { from = computation_to_perform
                ; via
                ; into = eventual_result
                ; (* We only invert lifecycles for explicit calls to
                     [Bonsai.with_inverted_lifecycle_ordering]. *)
                  invert_lifecycles = false
                ; here
                }
            in
            old_f (Trampoline.return computation)
        in
        let%bind.Trampoline computation in
        new_f computation
      in
      (* write the new hole into the graph, and return a new value referencing the type-id
         that will be populated when [new_f] is invoked. *)
      graph.f <- new_f;
      Value.named ~here (Sub here) via
  ;;

  (* [isolated] runs [f] on a fresh graph context. As an implementation detail, we
     actually mutate the same ['graph'], so that [the_one_and_only] is kept up to date.
     [isolated] also has an exception handler that returns any exceptions inside a
     Value.t. This restricts the return type of [isolated] to ['a Computation.t]. *)
  let isolated graph ~here ~(local_ f) =
    let backup_f = graph.f in
    graph.f <- Fn.id;
    try
      let r = f () in
      let r = graph.f (Trampoline.return (Proc.read ~here r)) in
      graph.f <- backup_f;
      Trampoline.run r
    with
    | exn ->
      !For_bonsai_internal.perform_on_exception exn;
      graph.f <- backup_f;
      Proc.read ~here (Value.return_exn ~here exn)
  ;;

  (* A global value which stores the current graph. This is so that functions like
     [Cont.map] can look up the current graph without being passed it explicitly. *)
  let the_one_and_only = { f = (fun _ -> failwith "outside of a Bonsai toplevel") }

  (* If [Value.map] is called within a [top_level_handle], we can use the global graph to
     deduplicate work. This counter keeps track of the number of nested [top_level_handle]
     calls we're currently within. In theory, this could be a bool, since
     [top_level_handle] calls shouldn't be nested, but this is a bit more defensive. *)
  let num_nested_top_level_handles = ref 0

  (* A small wrapper around isolated. All it does is ensure that you're using the same
     graph that you passed in. *)
  let handle ~(here : [%call_pos]) ~f (local_ graph) =
    isolated graph ~here ~f:(fun () -> f graph) [@nontail]
  ;;

  let handle_with_global_graph ~here inside_a_lazy f =
    (* nesting calls to this function is _fine_, but it should never happen, unless you're
       inside of a lazy_, where it's begrudgingly expected *)
    (match inside_a_lazy, !num_nested_top_level_handles > 0 with
     | `Not_inside_lazy, true ->
       eprintf
         "BUG: nested calls (%d) to top_level_handle. Please report to bonsai-devs.\n"
         !num_nested_top_level_handles
     | `Inside_lazy, (true | false) | `Not_inside_lazy, false -> ());
    incr num_nested_top_level_handles;
    Exn.protect
      ~f:(fun () ->
        let g = the_one_and_only in
        let backup_f = g.f in
        g.f <- Fn.id;
        let v = f g in
        let computation_context = g.f in
        g.f <- backup_f;
        (* You grit your teeth, plant your feet against the floor, and dredge a
           Computation.t from the void. *)
        Trampoline.run
          (computation_context (Trampoline.return (Proc_min.read ~here v))) [@nontail])
      ~finally:(fun () -> decr num_nested_top_level_handles) [@nontail]
  ;;

  let handle_for_lazy ~(here : [%call_pos]) f =
    (Timer.timer ()).time `Graph_application ~f:(fun () ->
      handle_with_global_graph ~here `Inside_lazy f)
  ;;

  (* Meant to be called at bonsai entrypoints only, [top_level_handle] uses the singleton
     graph and sets [nested_top_level_handles] acordingly. *)
  let top_level_handle ~(here : [%call_pos]) f =
    handle_with_global_graph ~here `Not_inside_lazy f
  ;;

  (* provides a way to get the current graph or provide a fallback if we aren't inside a
     call to top_level_handle. *)
  let with_global_graph ~f ~no_graph =
    if !num_nested_top_level_handles > 0 then f the_one_and_only else no_graph ()
  ;;
end

type 'a t = 'a Value.t
type graph = Cont_primitives.graph

open Cont_primitives

let return = Value.return
let return_lazy = Value.return_lazy

let arr1 ~(here : [%call_pos]) graph a ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map ~here a ~f))
;;

let arr2 ~(here : [%call_pos]) graph a b ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map2 ~here a b ~f))
;;

let arr3 ~(here : [%call_pos]) graph a b c ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map3 ~here a b c ~f))
;;

let arr4 ~(here : [%call_pos]) graph a b c d ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map4 ~here a b c d ~f))
;;

let arr5 ~(here : [%call_pos]) graph a b c d e ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map5 ~here a b c d e ~f))
;;

let arr6 ~(here : [%call_pos]) graph a b c d e g ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map6 ~here a b c d e g ~f))
;;

let arr7 ~(here : [%call_pos]) graph a b c d e g h ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map7 ~here a b c d e g h ~f))
;;

(* If we aren't inside of a [top_level_handle], then fall back to using [Value.map] *)
let map ~(here : [%call_pos]) a ~f =
  with_global_graph
    ~f:(fun graph -> arr1 ~here graph a ~f)
    ~no_graph:(fun () -> Value.map ~here a ~f)
;;

let map2 ~(here : [%call_pos]) a b ~f =
  with_global_graph
    ~f:(fun graph -> arr2 ~here graph a b ~f)
    ~no_graph:(fun () -> Value.map2 ~here a b ~f)
;;

include Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return = return
    let map2 = map2
    let map = `Custom map
  end)

let map3 ~(here : [%call_pos]) a b c ~f =
  with_global_graph
    ~f:(fun graph -> arr3 graph ~here a b c ~f)
    ~no_graph:(fun () -> Value.map3 ~here a b c ~f)
;;

let map4 ~(here : [%call_pos]) a b c d ~f =
  with_global_graph
    ~f:(fun graph -> arr4 ~here graph a b c d ~f)
    ~no_graph:(fun () -> Value.map4 ~here a b c d ~f)
;;

let map5 ~(here : [%call_pos]) a b c d e ~f =
  with_global_graph
    ~f:(fun graph -> arr5 ~here graph a b c d e ~f)
    ~no_graph:(fun () -> Value.map5 ~here a b c d e ~f)
;;

let map6 ~(here : [%call_pos]) a b c d e g ~f =
  with_global_graph
    ~f:(fun graph -> arr6 ~here graph a b c d e g ~f)
    ~no_graph:(fun () -> Value.map6 ~here a b c d e g ~f)
;;

let map7 ~(here : [%call_pos]) a b c d e g h ~f =
  with_global_graph
    ~f:(fun graph -> arr7 ~here graph a b c d e g h ~f)
    ~no_graph:(fun () -> Value.map7 ~here a b c d e g h ~f)
;;

let all ~(here : [%call_pos]) ts =
  with_global_graph
    ~f:(fun graph -> perform ~here graph (Proc.read ~here (Value.all ~here ts)))
    ~no_graph:(fun () -> Value.all ts)
;;

module Autopack = struct
  type 'a bonsai = 'a Value.t

  type ('packed, 'unpacked) t =
    | One : ('a, 'a bonsai) t
    | Two : ('a * 'b, 'a bonsai * 'b bonsai) t
    | Three : ('a * 'b * 'c, 'a bonsai * 'b bonsai * 'c bonsai) t
    | Four : ('a * 'b * 'c * 'd, 'a bonsai * 'b bonsai * 'c bonsai * 'd bonsai) t
    | Five :
        ( 'a * 'b * 'c * 'd * 'e
          , 'a bonsai * 'b bonsai * 'c bonsai * 'd bonsai * 'e bonsai )
          t
    | Six :
        ( 'a * 'b * 'c * 'd * 'e * 'f
          , 'a bonsai * 'b bonsai * 'c bonsai * 'd bonsai * 'e bonsai * 'f bonsai )
          t
    | Seven :
        ( 'a * 'b * 'c * 'd * 'e * 'f * 'g
          , 'a bonsai
            * 'b bonsai
            * 'c bonsai
            * 'd bonsai
            * 'e bonsai
            * 'f bonsai
            * 'g bonsai )
          t

  let pack
    : type packed unpacked.
      here:Source_code_position.t -> n:(packed, unpacked) t -> unpacked -> packed bonsai
    =
    fun ~here ~n unpacked ->
    match n with
    | One -> unpacked
    | Two ->
      let t1, t2 = unpacked in
      map2 ~here t1 t2 ~f:(fun v1 v2 -> v1, v2)
    | Three ->
      let t1, t2, t3 = unpacked in
      map3 ~here t1 t2 t3 ~f:(fun v1 v2 v3 -> v1, v2, v3)
    | Four ->
      let t1, t2, t3, t4 = unpacked in
      map4 ~here t1 t2 t3 t4 ~f:(fun v1 v2 v3 v4 -> v1, v2, v3, v4)
    | Five ->
      let t1, t2, t3, t4, t5 = unpacked in
      map5 ~here t1 t2 t3 t4 t5 ~f:(fun v1 v2 v3 v4 v5 -> v1, v2, v3, v4, v5)
    | Six ->
      let t1, t2, t3, t4, t5, t6 = unpacked in
      map6 ~here t1 t2 t3 t4 t5 t6 ~f:(fun v1 v2 v3 v4 v5 v6 -> v1, v2, v3, v4, v5, v6)
    | Seven ->
      let t1, t2, t3, t4, t5, t6, t7 = unpacked in
      map7 ~here t1 t2 t3 t4 t5 t6 t7 ~f:(fun v1 v2 v3 v4 v5 v6 v7 ->
        v1, v2, v3, v4, v5, v6, v7)
  ;;

  let unpack
    : type packed unpacked.
      here:Source_code_position.t -> n:(packed, unpacked) t -> packed bonsai -> unpacked
    =
    fun ~here ~n packed ->
    match n with
    | One -> packed
    | Two ->
      let t1 = map packed ~here ~f:(fun (v1, _) -> v1) in
      let t2 = map packed ~here ~f:(fun (_, v2) -> v2) in
      t1, t2
    | Three ->
      let t1 = map packed ~here ~f:(fun (v1, _, _) -> v1) in
      let t2 = map packed ~here ~f:(fun (_, v2, _) -> v2) in
      let t3 = map packed ~here ~f:(fun (_, _, v3) -> v3) in
      t1, t2, t3
    | Four ->
      let t1 = map packed ~here ~f:(fun (v1, _, _, _) -> v1) in
      let t2 = map packed ~here ~f:(fun (_, v2, _, _) -> v2) in
      let t3 = map packed ~here ~f:(fun (_, _, v3, _) -> v3) in
      let t4 = map packed ~here ~f:(fun (_, _, _, v4) -> v4) in
      t1, t2, t3, t4
    | Five ->
      let t1 = map packed ~here ~f:(fun (v1, _, _, _, _) -> v1) in
      let t2 = map packed ~here ~f:(fun (_, v2, _, _, _) -> v2) in
      let t3 = map packed ~here ~f:(fun (_, _, v3, _, _) -> v3) in
      let t4 = map packed ~here ~f:(fun (_, _, _, v4, _) -> v4) in
      let t5 = map packed ~here ~f:(fun (_, _, _, _, v5) -> v5) in
      t1, t2, t3, t4, t5
    | Six ->
      let t1 = map packed ~here ~f:(fun (v1, _, _, _, _, _) -> v1) in
      let t2 = map packed ~here ~f:(fun (_, v2, _, _, _, _) -> v2) in
      let t3 = map packed ~here ~f:(fun (_, _, v3, _, _, _) -> v3) in
      let t4 = map packed ~here ~f:(fun (_, _, _, v4, _, _) -> v4) in
      let t5 = map packed ~here ~f:(fun (_, _, _, _, v5, _) -> v5) in
      let t6 = map packed ~here ~f:(fun (_, _, _, _, _, v6) -> v6) in
      t1, t2, t3, t4, t5, t6
    | Seven ->
      let t1 = map packed ~here ~f:(fun (v1, _, _, _, _, _, _) -> v1) in
      let t2 = map packed ~here ~f:(fun (_, v2, _, _, _, _, _) -> v2) in
      let t3 = map packed ~here ~f:(fun (_, _, v3, _, _, _, _) -> v3) in
      let t4 = map packed ~here ~f:(fun (_, _, _, v4, _, _, _) -> v4) in
      let t5 = map packed ~here ~f:(fun (_, _, _, _, v5, _, _) -> v5) in
      let t6 = map packed ~here ~f:(fun (_, _, _, _, _, v6, _) -> v6) in
      let t7 = map packed ~here ~f:(fun (_, _, _, _, _, _, v7) -> v7) in
      t1, t2, t3, t4, t5, t6, t7
  ;;
end

let both ~(here : [%call_pos]) a b = map2 ~here a b ~f:Tuple2.create

let cutoff ~(here : [%call_pos]) v ~equal =
  Value.cutoff ~here v ~equal ~added_by_let_syntax:false
;;

let all_map ~(here : [%call_pos]) v (local_ graph) =
  perform
    ~here
    graph
    (Proc.Computation.all_map ~here (Core.Map.map v ~f:(fun f -> handle ~here ~f graph)))
;;

let transpose_opt ~(here : [%call_pos]) opt =
  Option.value_map opt ~default:(return ~here None) ~f:(map ~here ~f:Option.some)
;;

let path_id ~(here : [%call_pos]) graph = perform ~here graph (Proc.path_id ~here ())

let split ~here graph tuple =
  let a = arr1 ~here graph tuple ~f:Tuple2.get1 in
  let b = arr1 ~here graph tuple ~f:Tuple2.get2 in
  a, b
;;

let state__for_proc2
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?equal
  default_model
  (local_ graph)
  =
  perform ~here graph (Proc.state ~here ?reset ?sexp_of_model ?equal default_model)
;;

let state ~(here : [%call_pos]) ?reset ?sexp_of_model ?equal default_model (local_ graph) =
  state__for_proc2 ~here ?reset ?sexp_of_model ?equal default_model graph
  |> split ~here graph
;;

let state_opt__for_proc2
  ~(here : [%call_pos])
  ?reset
  ?default_model
  ?sexp_of_model
  ?equal
  ()
  (local_ graph)
  =
  perform
    ~here
    graph
    (Proc.state_opt ~here ?reset ?sexp_of_model ?equal ?default_model ())
;;

let state_opt
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?equal
  ?default_model
  (local_ graph)
  =
  state_opt__for_proc2 ~here ?reset ?sexp_of_model ?equal ?default_model () graph
  |> split ~here graph
;;

let state'__for_proc2
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?equal
  default_model
  (local_ graph)
  =
  perform ~here graph (Proc.state' ~here ?reset ?sexp_of_model ?equal default_model)
;;

let state' ~(here : [%call_pos]) ?reset ?sexp_of_model ?equal default_model (local_ graph)
  =
  state'__for_proc2 ~here ?reset ?sexp_of_model ?equal default_model graph
  |> split ~here graph
;;

let toggle__for_proc2 ~(here : [%call_pos]) ~default_model (local_ graph) =
  perform ~here graph (Proc.toggle ~here ~default_model ())
;;

let toggle ~(here : [%call_pos]) ~default_model (local_ graph) =
  toggle__for_proc2 ~here ~default_model graph |> split ~here graph
;;

module Toggle = struct
  type 'a v = 'a t

  type t =
    { state : bool v
    ; set_state : (bool -> unit Effect.t) v
    ; toggle : unit Effect.t v
    }
  [@@deriving fields ~getters]
end

let toggle' ~(here : [%call_pos]) ~default_model (local_ graph) =
  let all = perform ~here graph (Proc.toggle' ~here ~default_model ()) in
  let state = arr1 ~here graph all ~f:(fun { Proc.Toggle.state; _ } -> state) in
  let set_state =
    arr1 ~here graph all ~f:(fun { Proc.Toggle.set_state; _ } -> set_state)
  in
  let toggle = arr1 ~here graph all ~f:(fun { Proc.Toggle.toggle; _ } -> toggle) in
  { Toggle.state; set_state; toggle }
;;

module Path = Path

let path ~(here : [%call_pos]) graph = perform ~here graph (Proc.path ~here ())

let state_machine__for_proc2
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~apply_action
  ()
  (local_ graph)
  =
  Proc.state_machine
    ~here
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ()
    ~default_model
    ~apply_action
  |> perform ~here graph
;;

let state_machine
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~apply_action
  (local_ graph)
  =
  state_machine__for_proc2
    ~here
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~apply_action
    ()
    graph
  |> split ~here graph
;;

module Computation_status = Proc.Computation_status

module Actor (Action : T1) = struct
  module Base = Proc.Actor (Action)
  include Base

  let create_with_input
    ~(here : [%call_pos])
    ?sexp_of_action
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~recv
    input
    (local_ graph)
    =
    let model_and_inject =
      Base.create_with_input
        ~here
        ?sexp_of_action
        ?reset
        ?sexp_of_model
        ?equal
        ~default_model
        ~recv
        input
      |> perform ~here graph
    in
    map ~here model_and_inject ~f:Tuple2.get1, map ~here model_and_inject ~f:Tuple2.get2
  ;;

  let create
    ~(here : [%call_pos])
    ?sexp_of_action
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~recv
    (local_ graph)
    =
    let model_and_inject =
      Base.create
        ~here
        ?sexp_of_action
        ?reset
        ?sexp_of_model
        ?equal
        ~default_model
        ~recv
        ()
      |> perform ~here graph
    in
    map ~here model_and_inject ~f:Tuple2.get1, map ~here model_and_inject ~f:Tuple2.get2
  ;;
end

let state_machine_with_input__for_proc2
  ~(here : [%call_pos])
  ?sexp_of_action
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~apply_action
  input
  (local_ graph)
  =
  Proc.state_machine_with_input
    ~here
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~apply_action
    input
  |> perform ~here graph
;;

let state_machine_with_input
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~apply_action
  input
  (local_ graph)
  =
  state_machine_with_input__for_proc2
    ~here
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~apply_action
    input
    graph
  |> split ~here graph
;;

let actor__for_proc2
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~recv
  ()
  (local_ graph)
  =
  Proc.actor ~here ?reset ?sexp_of_model ?sexp_of_action ?equal ~default_model ~recv ()
  |> perform ~here graph
;;

let actor
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~recv
  (local_ graph)
  =
  actor__for_proc2
    ~here
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~recv
    ()
    graph
  |> split ~here graph
;;

let actor_with_input__for_proc2
  ~(here : [%call_pos])
  ?sexp_of_action
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~recv
  input
  (local_ graph)
  =
  Proc.actor_with_input
    ~here
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~recv
    input
  |> perform ~here graph
;;

let actor_with_input
  ~(here : [%call_pos])
  ?sexp_of_action
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~recv
  input
  (local_ graph)
  =
  actor_with_input__for_proc2
    ~here
    ?sexp_of_action
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~recv
    input
    graph
  |> split ~here graph
;;

let delay ~(here : [%call_pos]) ~f (local_ graph) =
  Proc.lazy_ ~here (lazy (handle_for_lazy ~here f)) |> perform ~here graph
;;

module Expert = struct
  let thunk ~(here : [%call_pos]) ~f graph = perform ~here graph (Proc.thunk ~here f)

  let assoc_on ~(here : [%call_pos]) io_cmp model_cmp map ~get_model_key ~f graph =
    Proc.assoc_on ~here io_cmp model_cmp map ~get_model_key ~f:(fun k v ->
      handle ~here graph ~f:(fun graph -> f k v graph) [@nontail])
    |> perform ~here graph
  ;;

  module Var = Var
  module For_bonsai_internal = For_bonsai_internal
end

let freeze ~(here : [%call_pos]) ?sexp_of_model ?equal v (local_ graph) =
  perform ~here graph (Proc.freeze ~here ?sexp_of_model ?equal v)
;;

let fix ~(here : [%call_pos]) v ~f graph =
  Proc_min.fix v ~here ~f:(fun ~recurse value ->
    let recurse v graph = perform ~here graph (recurse v) in
    isolated graph ~here ~f:(fun () -> f ~recurse value graph) [@nontail])
  |> perform ~here graph
;;

let fix2 ~(here : [%call_pos]) a b ~f graph =
  fix ~here (both ~here a b) graph ~f:(fun ~recurse a_and_b graph ->
    let a, b = split ~here graph a_and_b in
    let recurse a b graph = recurse (both ~here a b) graph in
    f ~recurse a b graph)
;;

let scope_model ~(here : [%call_pos]) comparator ~on ~for_ (local_ graph) =
  Proc.scope_model ~here comparator ~on (handle ~here graph ~f:(fun graph -> for_ graph))
  |> perform ~here graph
;;

let scope_model_n
  : type unpacked.
    here:[%call_pos]
    -> _
    -> n:(_, unpacked) Autopack.t
    -> on:_
    -> for_:(local_ graph -> unpacked)
    -> local_ graph
    -> unpacked
  =
  fun ~(here : [%call_pos]) comparator ~n ~on ~for_ (local_ graph) ->
  let result =
    scope_model ~here comparator ~on graph ~for_:(fun (local_ graph) ->
      Autopack.pack ~here ~n (for_ graph))
  in
  Autopack.unpack ~here ~n result
;;

let most_recent_some ~(here : [%call_pos]) ?sexp_of_model ~equal value ~f (local_ graph) =
  Proc.most_recent_some ~here ?sexp_of_model ~equal value ~f |> perform ~here graph
;;

let most_recent_value_satisfying
  ~(here : [%call_pos])
  ?sexp_of_model
  ~equal
  value
  ~condition
  (local_ graph)
  =
  Proc.most_recent_value_satisfying ~here ?sexp_of_model ~equal value ~condition
  |> perform ~here graph
;;

let previous_value ~(here : [%call_pos]) ?sexp_of_model ~equal value (local_ graph) =
  Proc.previous_value ~here ?sexp_of_model ~equal value |> perform ~here graph
;;

let wrap__for_proc2
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~apply_action
  ~f
  ()
  (local_ graph)
  =
  Proc_min.wrap
    ~here
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~apply_action
    ()
    ~f:(fun model inject ->
      handle ~here graph ~f:(fun graph -> f model inject graph) [@nontail])
  |> perform ~here graph
;;

let wrap
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~apply_action
  ~f
  (local_ graph)
  =
  wrap__for_proc2
    ~here
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~apply_action
    ~f
    ()
    graph
;;

let wrap_n
  : type packed unpacked.
    here:[%call_pos]
    -> ?reset:_
    -> ?sexp_of_model:_
    -> ?equal:_
    -> default_model:_
    -> apply_action:(_ -> packed Computation_status.t -> _)
    -> f:(_ -> _ t -> local_ graph -> unpacked)
    -> n:(packed, unpacked) Autopack.t
    -> local_ graph
    -> unpacked
  =
  fun ~(here : [%call_pos])
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~apply_action
    ~f
    ~n
    (local_ graph) ->
  let packed =
    wrap
      ~here
      ?reset
      ?sexp_of_model
      ?equal
      ~default_model
      ~apply_action
      graph
      ~f:(fun model inject (local_ graph) ->
        Autopack.pack ~here ~n (f model inject graph))
  in
  Autopack.unpack ~here ~n packed
;;

let enum ~(here : [%call_pos]) m ~match_ ~with_ (local_ graph) =
  let with_ : 'k -> 'd Computation.t =
    fun k -> handle ~f:(fun (local_ graph) -> with_ k graph) graph [@nontail]
  in
  perform ~here graph (Proc.enum ~here m ~match_ ~with_)
;;

let with_inverted_lifecycle_ordering ~(here : [%call_pos]) ~compute_dep ~f (local_ graph) =
  Proc_min.with_inverted_lifecycle_ordering
    ~here
    ~compute_dep:(handle ~here graph ~f:(fun graph -> compute_dep graph) [@nontail])
    (fun value -> handle ~here graph ~f:(fun graph -> f value graph) [@nontail])
  |> perform ~here graph
;;

let with_model_resetter__for_proc2 ~(here : [%call_pos]) ~f graph =
  perform
    ~here
    graph
    (Proc.with_model_resetter ~here (handle graph ~here ~f:(fun graph -> f graph)))
;;

let with_model_resetter ~(here : [%call_pos]) ~f graph =
  with_model_resetter__for_proc2 ~here ~f graph |> split ~here graph
;;

let with_model_resetter_n
  : type packed unpacked.
    here:[%call_pos]
    -> f:(local_ graph -> unpacked)
    -> n:(packed, unpacked) Autopack.t
    -> local_ graph
    -> unpacked * _
  =
  fun ~(here : [%call_pos]) ~f ~n (local_ graph) ->
  let packed, effect =
    with_model_resetter ~here graph ~f:(fun graph -> Autopack.pack ~here ~n (f graph))
  in
  Autopack.unpack ~here ~n packed, effect
;;

let with_model_resetter' ~(here : [%call_pos]) ~f (local_ graph) =
  Proc_min.with_model_resetter ~here (fun ~reset ->
    handle ~here graph ~f:(fun graph -> f ~reset graph) [@nontail])
  |> perform ~here graph
;;

let with_model_resetter_n'
  : type packed unpacked.
    here:[%call_pos]
    -> f:(reset:_ -> local_ graph -> unpacked)
    -> n:(packed, unpacked) Autopack.t
    -> local_ graph
    -> unpacked
  =
  fun ~(here : [%call_pos]) ~f ~n (local_ graph) ->
  let packed =
    with_model_resetter' ~here graph ~f:(fun ~reset graph ->
      Autopack.pack ~here ~n (f ~reset graph))
  in
  Autopack.unpack ~here ~n packed
;;

let peek ~(here : [%call_pos]) value graph = perform ~here graph (Proc.yoink ~here value)
let ignore_t (_ : unit t) = ()

module Clock = struct
  let approx_now ~(here : [%call_pos]) ~tick_every graph =
    perform ~here graph (Proc.Clock.approx_now ~here ~tick_every ())
  ;;

  module Before_or_after = struct
    type t = Ui_incr.Before_or_after.t =
      | Before
      | After
    [@@deriving sexp, equal]
  end

  let at ~(here : [%call_pos]) time graph = perform ~here graph (Proc.Clock.at ~here time)

  let every
    ~(here : [%call_pos])
    ~when_to_start_next_effect
    ?trigger_on_activate
    span
    callback
    graph
    =
    Proc.Clock.every ~here ~when_to_start_next_effect ?trigger_on_activate span callback
    |> perform ~here graph
    |> ignore_t
  ;;

  let get_current_time ~(here : [%call_pos]) graph =
    perform ~here graph (Proc.Clock.get_current_time ~here ())
  ;;

  let sleep ~(here : [%call_pos]) graph = perform ~here graph (Proc.Clock.sleep ~here ())
  let until ~(here : [%call_pos]) graph = perform ~here graph (Proc.Clock.until ~here ())

  module Expert = struct
    let now ~(here : [%call_pos]) graph = perform ~here graph (Proc.Clock.now ~here ())
  end
end

module Edge = struct
  let on_change__for_proc2
    ~(here : [%call_pos])
    ?sexp_of_model
    ~trigger
    ~equal
    value
    ~callback
    graph
    =
    perform
      ~here
      graph
      (Proc.Edge.on_change ~here ?sexp_of_model ~trigger ~equal value ~callback)
  ;;

  let on_change ~(here : [%call_pos]) ?sexp_of_model ~trigger ~equal value ~callback graph
    =
    ignore_t
      (on_change__for_proc2 ~here ?sexp_of_model ~trigger ~equal value ~callback graph)
  ;;

  let on_change'__for_proc2
    ~(here : [%call_pos])
    ?sexp_of_model
    ~trigger
    ~equal
    value
    ~callback
    graph
    =
    perform
      ~here
      graph
      (Proc.Edge.on_change' ~here ?sexp_of_model ~trigger ~equal value ~callback)
  ;;

  let on_change'
    ~(here : [%call_pos])
    ?sexp_of_model
    ~trigger
    ~equal
    value
    ~callback
    graph
    =
    ignore_t
      (on_change'__for_proc2 ~here ?sexp_of_model ~trigger ~equal value ~callback graph)
  ;;

  let lifecycle__for_proc2
    ~(here : [%call_pos])
    ?on_activate
    ?on_deactivate
    ?before_display
    ?after_display
    ()
    graph
    =
    perform
      ~here
      graph
      (Proc.Edge.lifecycle
         ~here
         ?on_activate
         ?on_deactivate
         ?before_display
         ?after_display
         ())
  ;;

  let lifecycle
    ~(here : [%call_pos])
    ?on_activate
    ?on_deactivate
    ?before_display
    ?after_display
    graph
    =
    ignore_t
      (lifecycle__for_proc2
         ~here
         ?on_activate
         ?on_deactivate
         ?before_display
         ?after_display
         ()
         graph)
  ;;

  let lifecycle'__for_proc2
    ~(here : [%call_pos])
    ?on_activate
    ?on_deactivate
    ?before_display
    ?after_display
    ()
    graph
    =
    perform
      ~here
      graph
      (Proc.Edge.lifecycle'
         ~here
         ?on_activate
         ?on_deactivate
         ?before_display
         ?after_display
         ())
  ;;

  let lifecycle'
    ~(here : [%call_pos])
    ?on_activate
    ?on_deactivate
    ?before_display
    ?after_display
    graph
    =
    ignore_t
      (lifecycle'__for_proc2
         ~here
         ?on_activate
         ?on_deactivate
         ?before_display
         ?after_display
         ()
         graph)
  ;;

  let before_display__for_proc2 ~(here : [%call_pos]) callback graph =
    perform ~here graph (Proc.Edge.before_display ~here callback)
  ;;

  let before_display ~(here : [%call_pos]) callback (local_ graph) =
    ignore_t (before_display__for_proc2 ~here callback graph)
  ;;

  let before_display'__for_proc2 ~(here : [%call_pos]) callback graph =
    perform ~here graph (Proc.Edge.before_display' ~here callback)
  ;;

  let before_display' ~(here : [%call_pos]) callback graph =
    ignore_t (before_display'__for_proc2 ~here callback graph)
  ;;

  (**)

  let after_display__for_proc2 ~(here : [%call_pos]) callback graph =
    perform ~here graph (Proc.Edge.after_display ~here callback)
  ;;

  let after_display ~(here : [%call_pos]) callback graph =
    ignore_t (after_display__for_proc2 ~here callback graph)
  ;;

  let after_display'__for_proc2 ~(here : [%call_pos]) callback graph =
    perform ~here graph (Proc.Edge.after_display' ~here callback)
  ;;

  let after_display' ~(here : [%call_pos]) callback graph =
    ignore_t (after_display'__for_proc2 ~here callback graph)
  ;;

  let wait_before_display ~(here : [%call_pos]) graph =
    perform ~here graph (Proc.Edge.wait_before_display ~here ())
  ;;

  let wait_after_display ~(here : [%call_pos]) graph =
    perform ~here graph (Proc.Edge.wait_after_display ~here ())
  ;;

  module Poll = struct
    module Starting = Proc.Edge.Poll.Starting

    let effect_on_change
      ~(here : [%call_pos])
      ?sexp_of_input
      ?sexp_of_result
      ~equal_input
      ?equal_result
      starting
      value
      ~effect
      graph
      =
      Proc.Edge.Poll.effect_on_change
        ~here
        ?sexp_of_input
        ?sexp_of_result
        ~equal_input
        ?equal_result
        starting
        value
        ~effect
      |> perform ~here graph
    ;;

    let manual_refresh__for_proc2
      ~(here : [%call_pos])
      ?sexp_of_model
      ?equal
      starting
      ~effect
      graph
      =
      perform
        ~here
        graph
        (Proc.Edge.Poll.manual_refresh ~here ?sexp_of_model ?equal starting ~effect)
    ;;

    let manual_refresh ~(here : [%call_pos]) ?sexp_of_model ?equal starting ~effect graph =
      manual_refresh__for_proc2 ~here ?sexp_of_model ?equal starting ~effect graph
      |> split ~here graph
    ;;
  end
end

module Memo = struct
  type ('input, 'result) t = ('input, 'result) Proc.Memo.t

  let create ~(here : [%call_pos]) cmp ~f graph =
    Proc.Memo.create ~here cmp ~f:(fun v ->
      handle ~here graph ~f:(fun graph -> f v graph) [@nontail])
    |> perform ~here graph
  ;;

  let lookup ~(here : [%call_pos]) t input graph =
    perform ~here graph (Proc.Memo.lookup ~here t input)
  ;;

  type ('query, 'response) responses = ('query, 'response) Proc.Memo.responses =
    | T : ('query, 'response, 'cmp) Map.t -> ('query, 'response) responses

  let responses (type i r) (t : (i, r) t) = Proc.Memo.responses t
end

module Effect_throttling = struct
  module Poll_result = Proc.Effect_throttling.Poll_result

  let poll ~(here : [%call_pos]) callback graph =
    perform ~here graph (Proc.Effect_throttling.poll ~here callback)
  ;;
end

module Dynamic_scope = struct
  type 'a bonsai_t = 'a t
  type 'a t = 'a Proc.Dynamic_scope.t

  type revert =
    { revert : 'a. (local_ graph -> 'a bonsai_t) -> (local_ graph -> 'a bonsai_t) }

  let create = Proc.Dynamic_scope.create
  let derived = Proc.Dynamic_scope.derived

  let lookup ~(here : [%call_pos]) var graph =
    perform ~here graph (Proc.Dynamic_scope.lookup ~here var)
  ;;

  let modify ~(here : [%call_pos]) var ~change ~f (local_ graph) =
    let current = lookup ~here var graph in
    let revert c (local_ graph) =
      perform
        ~here
        graph
        (Proc.Dynamic_scope.store ~here var current (handle ~here ~f:c graph))
    in
    let value = change current in
    perform
      ~here
      graph
      (Proc.Dynamic_scope.store
         ~here
         var
         value
         (handle ~here graph ~f:(fun (local_ graph) -> f { revert } graph)))
  ;;

  let set ~(here : [%call_pos]) var value ~inside (local_ graph) =
    let inside = handle ~here graph ~f:(fun graph -> inside graph) in
    perform ~here graph (Proc.Dynamic_scope.set ~here var value ~inside)
  ;;

  let set' ~(here : [%call_pos]) var value ~f (local_ graph) =
    modify ~here var ~change:(fun _ -> value) ~f graph
  ;;
end

module Incr = struct
  let value_cutoff ~(here : [%call_pos]) t ~equal (local_ graph) =
    perform ~here graph (Proc.Incr.value_cutoff ~here t ~equal)
  ;;

  let compute ~(here : [%call_pos]) t ~f (local_ graph) =
    perform ~here graph (Proc.Incr.compute ~here t ~f)
  ;;

  let to_value ~(here : [%call_pos]) incr = Proc.Incr.to_value ~here incr

  let with_clock ~(here : [%call_pos]) ~f (local_ graph) =
    perform ~here graph (Proc.Incr.with_clock ~here f)
  ;;
end

let assoc ~(here : [%call_pos]) comparator map ~f (local_ graph) =
  (Proc.assoc ~here comparator map ~f:(fun k v ->
     handle ~here graph ~f:(fun graph -> f k v graph) [@nontail])
  [@nontail])
  |> perform ~here graph
;;

let assoc_n
  : type packed unpacked.
    here:[%call_pos]
    -> _
    -> _
    -> f:(_ -> _ -> local_ graph -> unpacked)
    -> n:(packed, unpacked) Autopack.t
    -> local_ graph
    -> (_, packed, _) Map.t t
  =
  fun ~(here : [%call_pos]) comparator map ~f ~n (local_ graph) ->
  assoc ~here comparator map graph ~f:(fun key data (local_ graph) ->
    Autopack.pack ~here ~n (f key data graph))
;;

let assoc_set ~(here : [%call_pos]) comparator set ~f (local_ graph) =
  Proc.assoc_set ~here comparator set ~f:(fun k ->
    handle ~here graph ~f:(fun graph -> f k graph) [@nontail])
  |> perform ~here graph
;;

let assoc_set_n
  : type packed unpacked.
    here:[%call_pos]
    -> _
    -> _
    -> f:(_ -> local_ graph -> unpacked)
    -> n:(packed, unpacked) Autopack.t
    -> local_ graph
    -> (_, packed, _) Map.t t
  =
  fun ~(here : [%call_pos]) comparator map ~f ~n (local_ graph) ->
  assoc_set ~here comparator map graph ~f:(fun key (local_ graph) ->
    Autopack.pack ~here ~n (f key graph))
;;

let assoc_list ~(here : [%call_pos]) comparator list ~get_key ~f (local_ graph) =
  Proc.assoc_list ~here comparator list ~get_key ~f:(fun k v ->
    handle ~here graph ~f:(fun graph -> f k v graph) [@nontail])
  |> perform ~here graph
;;

let assoc_list_n
  : type packed unpacked.
    here:[%call_pos]
    -> _
    -> _
    -> get_key:_
    -> f:(_ -> _ -> local_ graph -> unpacked)
    -> n:(packed, unpacked) Autopack.t
    -> local_ graph
    -> [ `Duplicate_key of 'key | `Ok of packed list ] t
  =
  fun ~(here : [%call_pos]) comparator list ~get_key ~f ~n (local_ graph) ->
  assoc_list ~here comparator list ~get_key graph ~f:(fun key data (local_ graph) ->
    Autopack.pack ~here ~n (f key data graph))
;;

module Debug = struct
  let on_change ~(here : [%call_pos]) v ~f graph =
    let f =
      arr1 ~here graph v ~f:(fun v ->
        f v;
        Effect.Ignore)
    in
    (* Use [on_deactivate] because the incremental node is always considered to be in use. *)
    Edge.lifecycle ~here ~on_deactivate:f graph
  ;;

  let on_change_print_s ~(here : [%call_pos]) v sexp_of =
    on_change ~here v ~f:(fun a -> print_s (sexp_of a))
  ;;

  let to_dot ?pre_process c = To_dot.to_dot ?pre_process (top_level_handle c)

  let bonsai_node_counts ?pre_process c =
    Skeleton.Counts.get ?pre_process (top_level_handle c)
  ;;

  let enable_incremental_annotations = Annotate_incr.enable
  let disable_incremental_annotations = Annotate_incr.disable

  let watch_computation
    ~(here : [%call_pos])
    ?(log_model_before = false)
    ?(log_model_after = false)
    ?(log_action = false)
    ?(log_incr_info = true)
    ?(log_watcher_positions = true)
    ?(log_dependency_definition_position = true)
    ?label
    ~f
    (local_ graph)
    =
    perform
      graph
      (Proc.watch_computation
         ~here
         ~log_model_before
         ~log_model_after
         ~log_action
         ~log_incr_info
         ~log_watcher_positions
         ~log_dependency_definition_position
         ~label
         (handle graph ~f:(fun graph -> f graph)))
      ~here
  ;;

  let memo_subscribers (T { subscribers; _ } : _ Memo.t) = subscribers
end

let switch__for_proc2
  ~(here : [%call_pos])
  ~match_
  ~branches
  ~(local_ with_)
  (local_ graph)
  =
  let arms =
    let arms = ref [] in
    for i = 0 to branches - 1 do
      let computation = isolated graph ~here ~f:(fun () -> with_ i graph) in
      arms := (i, computation) :: !arms
    done;
    !arms
  in
  Computation.Switch { match_; arms = Map.of_alist_exn (module Int) arms; here }
  |> perform ~here graph
;;

module Let_syntax = struct
  let return = return
  let ( >>| ) ~(here : [%call_pos]) t f = map ~here t ~f

  module Let_syntax = struct
    let return = Fn.id
    let map ~(here : [%call_pos]) a ~f = map ~here a ~f
    let map2 = map2
    let map3 = map3
    let map4 = map4
    let map5 = map5
    let map6 = map6
    let map7 = map7
    let arr = map
    let arr2 = map2
    let arr3 = map3
    let arr4 = map4
    let arr5 = map5
    let arr6 = map6
    let arr7 = map7
    let both = both

    let cutoff ~(here : [%call_pos]) v ~equal =
      Value.cutoff ~here v ~equal ~added_by_let_syntax:true
    ;;

    let switch ~(here : [%call_pos]) ~match_ ~branches ~with_ graph =
      let with_ i _graph = with_ i in
      switch__for_proc2 ~here ~match_ ~branches ~with_ graph [@nontail]
    ;;

    let switch ~here ~match_ ~branches ~(local_ with_) =
      with_global_graph
        ~f:(fun graph -> switch ~here ~match_ ~branches ~with_ graph)
        ~no_graph:(fun () ->
          raise_s
            [%message
              "match%sub called outside of the context of a graph"
                (here : Source_code_position.t)]) [@nontail]
    ;;

    let sub ~here:(_ : [%call_pos]) a ~f = f a
    let delay = delay
  end
end

module For_proc = struct
  module type Map_and_set0_output = Map_and_set0_intf.Output

  let arr1 = arr1
  let arr2 = arr2
  let arr3 = arr3
  let arr4 = arr4
  let arr5 = arr5
  let arr6 = arr6
  let arr7 = arr7
  let value_cutoff v ~equal = Value.cutoff v ~equal ~added_by_let_syntax:false
  let value_map ~(here : [%call_pos]) t ~f = { (Value.map t ~f) with here }
  let conceal_value v = v
  let state = state__for_proc2
  let state' = state'__for_proc2
  let state_opt = state_opt__for_proc2
  let toggle = toggle__for_proc2

  module Toggle = Proc.Toggle

  let toggle' ~(here : [%call_pos]) ~default_model graph =
    perform ~here graph (Proc.toggle' ~here ~default_model ())
  ;;

  let state_machine = state_machine__for_proc2
  let state_machine_with_input = state_machine_with_input__for_proc2
  let actor = actor__for_proc2
  let actor_with_input = actor_with_input__for_proc2
  let wrap = wrap__for_proc2

  let with_model_resetter ~(here : [%call_pos]) f graph =
    with_model_resetter__for_proc2 ~here ~f graph
  ;;

  let with_model_resetter' ~(here : [%call_pos]) f graph =
    with_model_resetter' ~here ~f graph
  ;;

  let lazy_ ~(here : [%call_pos]) f graph =
    delay ~here ~f:(fun graph -> Lazy.force f graph) graph
  ;;

  let switch ~(here : [%call_pos]) ~match_ ~branches ~with_ graph =
    switch__for_proc2 ~here ~match_ ~branches ~with_ graph
  ;;

  let on_change = Edge.on_change__for_proc2
  let on_change' = Edge.on_change'__for_proc2
  let lifecycle = Edge.lifecycle__for_proc2
  let lifecycle' = Edge.lifecycle'__for_proc2
  let before_display = Edge.before_display__for_proc2
  let before_display' = Edge.before_display'__for_proc2
  let after_display = Edge.after_display__for_proc2
  let after_display' = Edge.after_display'__for_proc2
  let manual_refresh = Edge.Poll.manual_refresh__for_proc2

  let debug_on_change ~(here : [%call_pos]) v ~f graph =
    let f =
      arr1 ~here graph v ~f:(fun v ->
        f v;
        Effect.Ignore)
    in
    Edge.after_display__for_proc2 ~here f graph
  ;;

  let debug_on_change_print_s ~(here : [%call_pos]) v sexp_of =
    debug_on_change ~here v ~f:(fun a -> print_s (sexp_of a))
  ;;

  let narrow ~(here : [%call_pos]) state_and_inject ~get ~set graph =
    let open Let_syntax in
    let state, inject = state_and_inject |> split ~here graph in
    let inject =
      let peek_state = peek ~here state graph in
      let%arr peek_state and inject in
      fun a ->
        match%bind.Effect peek_state with
        | Inactive -> Effect.Ignore
        | Active state -> inject (set state a)
    in
    let state =
      let%arr state in
      get state
    in
    let%arr state and inject in
    state, inject
  ;;

  let narrow_via_field ~(here : [%call_pos]) state_and_inject field =
    narrow state_and_inject ~here ~get:(Field.get field) ~set:(Field.fset field)
  ;;
end

module Conv = struct
  let handle = handle
  let top_level_handle = top_level_handle
  let perform ~(here : [%call_pos]) (local_ graph) c = perform ~here graph c
  let reveal_value = Fn.id
  let conceal_value = Fn.id
  let isolated = isolated
end

include Map_and_set0.Make (struct
    module Value = struct
      type nonrec 'a t = 'a t

      let both = both
    end

    module Computation = struct
      type nonrec 'a t = local_ graph -> 'a t
    end

    module Incr = struct
      let compute = Incr.compute
    end
  end)
