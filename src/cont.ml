open! Core
open! Import
module Effect = Ui_effect
module Time_source = Time_source
module Apply_action_context = Apply_action_context

module type Enum = Module_types.Enum
module type Comparator = Module_types.Comparator

type ('k, 'cmp) comparator = ('k, 'cmp) Module_types.comparator

module For_bonsai_internal = struct
  let perform_on_exception = ref ignore
  let set_perform_on_exception perform = perform_on_exception := perform
end

module Cont_primitives : sig
  type graph

  (* Main primitives; see above for explanation. *)
  val perform : here:Source_code_position.t -> graph -> 'a Computation.t -> 'a Value.t

  val handle
    :  ?here:Stdlib.Lexing.position
    -> f:(graph -> 'a Value.t)
    -> graph
    -> 'a Computation.t

  (* Special-use primitives for getting the global graph, and creating it in the top level. *)
  val isolated
    :  graph
    -> here:Source_code_position.t
    -> f:(unit -> 'a Value.t)
    -> 'a Computation.t

  val top_level_handle
    :  ?here:Stdlib.Lexing.position
    -> (graph -> 'a Value.t)
    -> 'a Computation.t

  val handle_for_lazy
    :  ?here:Stdlib.Lexing.position
    -> (graph -> 'a Value.t)
    -> 'a Computation.t

  val with_global_graph : f:(graph -> 'a) -> no_graph:(unit -> 'a) -> 'a
end = struct
  type graph = { mutable f : 'a. 'a Computation.t -> 'a Computation.t }

  let perform
    : type a. here:Source_code_position.t -> graph -> a Computation.t -> a Value.t
    =
    fun ~here graph -> function
    | Return
        { value = { value = (Named _ | Constant _ | Exception _) as value; id; _ }; here }
      ->
      (* Introduce the optimization [let%sub a = return foo in use a] => [use foo]
           This only makes sense if the Value.t being returned is either a constant or an
           already-bound named value, otherwise you risk losing value sharing. *)
      { Value.value; id; here }
    | computation_to_perform ->
      (* Mint a fresh type-id to hold the result of performing this graph modification  *)
      let via : a Type_equal.Id.t = Type_equal.Id.create ~name:"" [%sexp_of: opaque] in
      (* Keep hold of the previous graph-modification function *)
      let old_f : type b. b Computation.t -> b Computation.t = graph.f in
      let new_f : type x. x Computation.t -> x Computation.t = function
        | Return { value = { value = Named _; id; _ }; here = _ }
          when Type_equal.Id.same via id ->
          (* introduce the optimization {[ let%sub a = foo bar in return a ]} => {[ foo bar ]} *)
          let T = Type_equal.Id.same_witness_exn via id in
          old_f computation_to_perform
        | eventual_result ->
          (* old_f takes the eventual innermost result, and wraps it in 0+ layers of subs.
               We replace it with a new function that adds another layer to the inside. *)
          old_f (Sub { from = computation_to_perform; via; into = eventual_result; here })
      in
      (* write the new hole into the graph, and return a new value referencing the
           type-id that will be populated when [new_f] is invoked. *)
      graph.f <- new_f;
      Value.named ~here (Sub here) via
  ;;

  (* [isolated] runs [f] on a fresh graph context. As an implementation detail, we actually
     mutate the same ['graph'], so that [the_one_and_only] is kept up to date.
     [isolated] also has an exception handler that returns any exceptions inside a Value.t.
     This restricts the return type of [isolated] to ['a Computation.t]. *)
  let isolated graph ~here ~f =
    let backup_f = graph.f in
    graph.f <- Fn.id;
    try
      let r = f () in
      let r = graph.f (Proc.read ~here r) in
      graph.f <- backup_f;
      r
    with
    | exn ->
      !For_bonsai_internal.perform_on_exception exn;
      graph.f <- backup_f;
      Proc.read ~here (Value.return_exn ~here exn)
  ;;

  (* A global value which stores the current graph.  This is so that functions like
     [Cont.map] can look up the current graph without being passed it explicitly. *)
  let the_one_and_only = { f = (fun _ -> failwith "outside of a Bonsai toplevel") }

  (* If [Value.map] is called within a [top_level_handle], we can use the global graph to
     deduplicate work. This counter keeps track of the number of nested [top_level_handle]
     calls we're currently within. In theory, this could be a bool, since
     [top_level_handle] calls shouldn't be nested, but this is a bit more defensive. *)
  let num_nested_top_level_handles = ref 0

  (* A small wrapper around isolated.  All it does is ensure that you're using
     the same graph that you passed in. *)
  let handle ?(here = Stdlib.Lexing.dummy_pos) ~f graph =
    isolated graph ~here ~f:(fun () -> f graph) [@nontail]
  ;;

  let handle_with_global_graph ~here inside_a_lazy f =
    (* nesting calls to this function is _fine_, but it should never happen,
       unless you're inside of a lazy_, where it's begrudgingly expected *)
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
        computation_context (Proc_min.read ~here v) [@nontail])
      ~finally:(fun () -> decr num_nested_top_level_handles) [@nontail]
  ;;

  let handle_for_lazy ?(here = Stdlib.Lexing.dummy_pos) f =
    handle_with_global_graph ~here `Inside_lazy f
  ;;

  (* Meant to be called at bonsai entrypoints only, [top_level_handle] uses the
     singleton graph and sets [nested_top_level_handles] acordingly. *)
  let top_level_handle ?(here = Stdlib.Lexing.dummy_pos) f =
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

let arr1 ?(here = Stdlib.Lexing.dummy_pos) graph a ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map ~here a ~f))
;;

let arr2 ?(here = Stdlib.Lexing.dummy_pos) graph a b ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map2 ~here a b ~f))
;;

let arr3 ?(here = Stdlib.Lexing.dummy_pos) graph a b c ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map3 ~here a b c ~f))
;;

let arr4 ?(here = Stdlib.Lexing.dummy_pos) graph a b c d ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map4 ~here a b c d ~f))
;;

let arr5 ?(here = Stdlib.Lexing.dummy_pos) graph a b c d e ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map5 ~here a b c d e ~f))
;;

let arr6 ?(here = Stdlib.Lexing.dummy_pos) graph a b c d e g ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map6 ~here a b c d e g ~f))
;;

let arr7 ?(here = Stdlib.Lexing.dummy_pos) graph a b c d e g h ~f =
  perform ~here graph (Proc.read ~here (Proc.Value.map7 ~here a b c d e g h ~f))
;;

(* If we aren't inside of a [top_level_handle], then fall back to using [Value.map] *)
let map ?(here = Stdlib.Lexing.dummy_pos) a ~f =
  with_global_graph
    ~f:(fun graph -> arr1 ~here graph a ~f)
    ~no_graph:(fun () -> Value.map ~here a ~f)
;;

let map2 ?(here = Stdlib.Lexing.dummy_pos) a b ~f =
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

let map3 ?(here = Stdlib.Lexing.dummy_pos) a b c ~f =
  with_global_graph
    ~f:(fun graph -> arr3 graph ~here a b c ~f)
    ~no_graph:(fun () -> Value.map3 ~here a b c ~f)
;;

let map4 ?(here = Stdlib.Lexing.dummy_pos) a b c d ~f =
  with_global_graph
    ~f:(fun graph -> arr4 ~here graph a b c d ~f)
    ~no_graph:(fun () -> Value.map4 ~here a b c d ~f)
;;

let map5 ?(here = Stdlib.Lexing.dummy_pos) a b c d e ~f =
  with_global_graph
    ~f:(fun graph -> arr5 ~here graph a b c d e ~f)
    ~no_graph:(fun () -> Value.map5 ~here a b c d e ~f)
;;

let map6 ?(here = Stdlib.Lexing.dummy_pos) a b c d e g ~f =
  with_global_graph
    ~f:(fun graph -> arr6 ~here graph a b c d e g ~f)
    ~no_graph:(fun () -> Value.map6 ~here a b c d e g ~f)
;;

let map7 ?(here = Stdlib.Lexing.dummy_pos) a b c d e g h ~f =
  with_global_graph
    ~f:(fun graph -> arr7 ~here graph a b c d e g h ~f)
    ~no_graph:(fun () -> Value.map7 ~here a b c d e g h ~f)
;;

let both ?(here = Stdlib.Lexing.dummy_pos) a b = map2 ~here a b ~f:Tuple2.create

let cutoff ?(here = Stdlib.Lexing.dummy_pos) v ~equal =
  Value.cutoff ~here v ~equal ~added_by_let_syntax:false
;;

let all_map ?(here = Stdlib.Lexing.dummy_pos) v graph =
  perform
    ~here
    graph
    (Proc.Computation.all_map (Core.Map.map v ~f:(fun f -> handle ~f graph)))
;;

let transpose_opt opt =
  Option.value_map opt ~default:(return None) ~f:(map ~f:Option.some)
;;

let path_id ?(here = Stdlib.Lexing.dummy_pos) graph =
  perform ~here graph (Proc.path_id ~here ())
;;

let split ~here graph tuple =
  let a = arr1 ~here graph tuple ~f:Tuple2.get1 in
  let b = arr1 ~here graph tuple ~f:Tuple2.get2 in
  a, b
;;

let state__for_proc2
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?equal
  default_model
  graph
  =
  perform ~here graph (Proc.state ~here ?reset ?sexp_of_model ?equal default_model)
;;

let state
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?equal
  default_model
  graph
  =
  state__for_proc2 ~here ?reset ?sexp_of_model ?equal default_model graph
  |> split ~here graph
;;

let state_opt__for_proc2
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?default_model
  ?sexp_of_model
  ?equal
  ()
  graph
  =
  perform
    ~here
    graph
    (Proc.state_opt ~here ?reset ?sexp_of_model ?equal ?default_model ())
;;

let state_opt
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?equal
  ?default_model
  graph
  =
  state_opt__for_proc2 ~here ?reset ?sexp_of_model ?equal ?default_model () graph
  |> split ~here graph
;;

let toggle__for_proc2 ?(here = Stdlib.Lexing.dummy_pos) ~default_model graph =
  perform ~here graph (Proc.toggle ~here ~default_model ())
;;

let toggle ?(here = Stdlib.Lexing.dummy_pos) ~default_model graph =
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

let toggle' ?(here = Stdlib.Lexing.dummy_pos) ~default_model graph =
  let all = perform ~here graph (Proc.toggle' ~here ~default_model ()) in
  let state = arr1 graph all ~f:(fun { Proc.Toggle.state; _ } -> state) in
  let set_state = arr1 graph all ~f:(fun { Proc.Toggle.set_state; _ } -> set_state) in
  let toggle = arr1 graph all ~f:(fun { Proc.Toggle.toggle; _ } -> toggle) in
  { Toggle.state; set_state; toggle }
;;

module Path = Path

let path ?(here = Stdlib.Lexing.dummy_pos) graph =
  perform ~here graph (Proc.path ~here ())
;;

let state_machine0__for_proc2
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~apply_action
  ()
  graph
  =
  Proc.state_machine0
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

let state_machine0
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~apply_action
  graph
  =
  state_machine0__for_proc2
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

let state_machine1__for_proc2
  ?(here = Stdlib.Lexing.dummy_pos)
  ?sexp_of_action
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~apply_action
  input
  graph
  =
  Proc.state_machine1
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

let state_machine1
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~apply_action
  input
  graph
  =
  state_machine1__for_proc2
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

let actor0__for_proc2
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~recv
  ()
  graph
  =
  Proc.actor0 ~here ?reset ?sexp_of_model ?sexp_of_action ?equal ~default_model ~recv ()
  |> perform ~here graph
;;

let actor0
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~recv
  graph
  =
  actor0__for_proc2
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

let actor1__for_proc2
  ?(here = Stdlib.Lexing.dummy_pos)
  ?sexp_of_action
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~recv
  input
  graph
  =
  Proc.actor1
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

let actor1
  ?(here = Stdlib.Lexing.dummy_pos)
  ?sexp_of_action
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~recv
  input
  graph
  =
  actor1__for_proc2
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

let delay ?(here = Stdlib.Lexing.dummy_pos) ~f graph =
  Proc.lazy_ ~here (lazy (handle_for_lazy ~here f)) |> perform ~here graph
;;

module Expert = struct
  let thunk ?(here = Stdlib.Lexing.dummy_pos) ~f graph =
    perform ~here graph (Proc.thunk ~here f)
  ;;

  let assoc_on
    ?(here = Stdlib.Lexing.dummy_pos)
    io_cmp
    model_cmp
    map
    ~get_model_key
    ~f
    graph
    =
    Proc.assoc_on ~here io_cmp model_cmp map ~get_model_key ~f:(fun k v ->
      handle ~here graph ~f:(fun graph -> f k v graph) [@nontail])
    |> perform ~here graph
  ;;

  let delay = delay

  module Var = Var
  module For_bonsai_internal = For_bonsai_internal
end

let freeze ?(here = Stdlib.Lexing.dummy_pos) ?sexp_of_model ?equal v graph =
  perform ~here graph (Proc.freeze ~here ?sexp_of_model ?equal v)
;;

let fix ?(here = Stdlib.Lexing.dummy_pos) v ~f graph =
  Proc_min.fix v ~here ~f:(fun ~recurse value ->
    let recurse v graph = perform ~here graph (recurse v) in
    isolated graph ~here ~f:(fun () -> f ~recurse value graph) [@nontail])
  |> perform ~here graph
;;

let fix2 ?(here = Stdlib.Lexing.dummy_pos) a b ~f graph =
  fix ~here (both ~here a b) graph ~f:(fun ~recurse a_and_b graph ->
    let a, b = split ~here graph a_and_b in
    let recurse a b graph = recurse (both ~here a b) graph in
    f ~recurse a b graph)
;;

let scope_model ?(here = Stdlib.Lexing.dummy_pos) comparator ~on ~for_ graph =
  Proc.scope_model ~here comparator ~on (handle ~here graph ~f:(fun graph -> for_ graph))
  |> perform ~here graph
;;

let most_recent_some
  ?(here = Stdlib.Lexing.dummy_pos)
  ?sexp_of_model
  ~equal
  value
  ~f
  graph
  =
  Proc.most_recent_some ~here ?sexp_of_model ~equal value ~f |> perform ~here graph
;;

let most_recent_value_satisfying
  ?(here = Stdlib.Lexing.dummy_pos)
  ?sexp_of_model
  ~equal
  value
  ~condition
  graph
  =
  Proc.most_recent_value_satisfying ~here ?sexp_of_model ~equal value ~condition
  |> perform ~here graph
;;

let previous_value ?(here = Stdlib.Lexing.dummy_pos) ?sexp_of_model ~equal value graph =
  Proc.previous_value ~here ?sexp_of_model ~equal value |> perform ~here graph
;;

let wrap__for_proc2
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~apply_action
  ~f
  ()
  graph
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
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~apply_action
  ~f
  graph
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

let enum ?(here = Stdlib.Lexing.dummy_pos) m ~match_ ~with_ graph =
  let with_ : 'k -> 'd Computation.t =
    fun k -> handle ~f:(fun graph -> with_ k graph) graph [@nontail]
  in
  perform ~here graph (Proc.enum m ~match_ ~with_)
;;

let with_model_resetter__for_proc2 ?(here = Stdlib.Lexing.dummy_pos) ~f graph =
  perform
    ~here
    graph
    (Proc.with_model_resetter ~here (handle graph ~here ~f:(fun graph -> f graph)))
;;

let with_model_resetter ?(here = Stdlib.Lexing.dummy_pos) ~f graph =
  with_model_resetter__for_proc2 ~here ~f graph |> split ~here graph
;;

let with_model_resetter' ?(here = Stdlib.Lexing.dummy_pos) ~f graph =
  Proc_min.with_model_resetter ~here (fun ~reset ->
    handle ~here graph ~f:(fun graph -> f ~reset graph) [@nontail])
  |> perform ~here graph
;;

let peek ?(here = Stdlib.Lexing.dummy_pos) value graph =
  perform ~here graph (Proc.yoink ~here value)
;;

let ignore_t (_ : unit t) = ()

module Clock = struct
  let approx_now ?(here = Stdlib.Lexing.dummy_pos) ~tick_every graph =
    perform ~here graph (Proc.Clock.approx_now ~here ~tick_every ())
  ;;

  let now ?(here = Stdlib.Lexing.dummy_pos) graph =
    perform ~here graph (Proc.Clock.now ~here ())
  ;;

  module Before_or_after = struct
    type t = Ui_incr.Before_or_after.t =
      | Before
      | After
    [@@deriving sexp, equal]
  end

  let at ?(here = Stdlib.Lexing.dummy_pos) time graph =
    perform ~here graph (Proc.Clock.at ~here time)
  ;;

  let every
    ?(here = Stdlib.Lexing.dummy_pos)
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

  let get_current_time ?(here = Stdlib.Lexing.dummy_pos) graph =
    perform ~here graph (Proc.Clock.get_current_time ~here ())
  ;;

  let sleep ?(here = Stdlib.Lexing.dummy_pos) graph =
    perform ~here graph (Proc.Clock.sleep ~here ())
  ;;

  let until ?(here = Stdlib.Lexing.dummy_pos) graph =
    perform ~here graph (Proc.Clock.until ~here ())
  ;;
end

module Edge = struct
  let on_change__for_proc2
    ?(here = Stdlib.Lexing.dummy_pos)
    ?sexp_of_model
    ~equal
    value
    ~callback
    graph
    =
    perform ~here graph (Proc.Edge.on_change ~here ?sexp_of_model ~equal value ~callback)
  ;;

  let on_change
    ?(here = Stdlib.Lexing.dummy_pos)
    ?sexp_of_model
    ~equal
    value
    ~callback
    graph
    =
    ignore_t (on_change__for_proc2 ~here ?sexp_of_model ~equal value ~callback graph)
  ;;

  let on_change'__for_proc2
    ?(here = Stdlib.Lexing.dummy_pos)
    ?sexp_of_model
    ~equal
    value
    ~callback
    graph
    =
    perform ~here graph (Proc.Edge.on_change' ~here ?sexp_of_model ~equal value ~callback)
  ;;

  let on_change'
    ?(here = Stdlib.Lexing.dummy_pos)
    ?sexp_of_model
    ~equal
    value
    ~callback
    graph
    =
    ignore_t (on_change'__for_proc2 ~here ?sexp_of_model ~equal value ~callback graph)
  ;;

  let lifecycle__for_proc2
    ?(here = Stdlib.Lexing.dummy_pos)
    ?on_activate
    ?on_deactivate
    ?after_display
    ()
    graph
    =
    perform
      ~here
      graph
      (Proc.Edge.lifecycle ~here ?on_activate ?on_deactivate ?after_display ())
  ;;

  let lifecycle
    ?(here = Stdlib.Lexing.dummy_pos)
    ?on_activate
    ?on_deactivate
    ?after_display
    graph
    =
    ignore_t
      (lifecycle__for_proc2 ~here ?on_activate ?on_deactivate ?after_display () graph)
  ;;

  let lifecycle'__for_proc2
    ?(here = Stdlib.Lexing.dummy_pos)
    ?on_activate
    ?on_deactivate
    ?after_display
    ()
    graph
    =
    perform
      ~here
      graph
      (Proc.Edge.lifecycle' ?on_activate ?on_deactivate ?after_display ())
  ;;

  let lifecycle'
    ?(here = Stdlib.Lexing.dummy_pos)
    ?on_activate
    ?on_deactivate
    ?after_display
    graph
    =
    ignore_t
      (lifecycle'__for_proc2 ~here ?on_activate ?on_deactivate ?after_display () graph)
  ;;

  let after_display__for_proc2 ?(here = Stdlib.Lexing.dummy_pos) callback graph =
    perform ~here graph (Proc.Edge.after_display ~here callback)
  ;;

  let after_display ?(here = Stdlib.Lexing.dummy_pos) callback graph =
    ignore_t (after_display__for_proc2 ~here callback graph)
  ;;

  let after_display'__for_proc2 ?(here = Stdlib.Lexing.dummy_pos) callback graph =
    perform ~here graph (Proc.Edge.after_display' ~here callback)
  ;;

  let after_display' ?(here = Stdlib.Lexing.dummy_pos) callback graph =
    ignore_t (after_display'__for_proc2 ~here callback graph)
  ;;

  let wait_after_display ?(here = Stdlib.Lexing.dummy_pos) graph =
    perform ~here graph (Proc.Edge.wait_after_display ~here ())
  ;;

  module Poll = struct
    module Starting = Proc.Edge.Poll.Starting

    let effect_on_change
      ?(here = Stdlib.Lexing.dummy_pos)
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
      ?(here = Stdlib.Lexing.dummy_pos)
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

    let manual_refresh
      ?(here = Stdlib.Lexing.dummy_pos)
      ?sexp_of_model
      ?equal
      starting
      ~effect
      graph
      =
      manual_refresh__for_proc2 ~here ?sexp_of_model ?equal starting ~effect graph
      |> split ~here graph
    ;;
  end
end

module Memo = struct
  type ('input, 'result) t = ('input, 'result) Proc.Memo.t

  let create ?(here = Stdlib.Lexing.dummy_pos) cmp ~f graph =
    Proc.Memo.create ~here cmp ~f:(fun v ->
      handle ~here graph ~f:(fun graph -> f v graph) [@nontail])
    |> perform ~here graph
  ;;

  let lookup ?(here = Stdlib.Lexing.dummy_pos) ?sexp_of_model ~equal t input graph =
    perform ~here graph (Proc.Memo.lookup ~here ?sexp_of_model ~equal t input)
  ;;
end

module Effect_throttling = struct
  module Poll_result = Proc.Effect_throttling.Poll_result

  let poll ?(here = Stdlib.Lexing.dummy_pos) callback graph =
    perform ~here graph (Proc.Effect_throttling.poll ~here callback)
  ;;
end

module Dynamic_scope = struct
  type 'a bonsai_t = 'a t
  type 'a t = 'a Proc.Dynamic_scope.t
  type revert = { revert : 'a. (graph -> 'a bonsai_t) -> graph -> 'a bonsai_t }

  let create = Proc.Dynamic_scope.create
  let derived = Proc.Dynamic_scope.derived

  let set ?(here = Stdlib.Lexing.dummy_pos) var value ~inside graph =
    let inside = handle ~here graph ~f:(fun graph -> inside graph) in
    perform ~here graph (Proc.Dynamic_scope.set ~here var value ~inside)
  ;;

  let f_with_resetter ~here ~f graph (resetter : Proc.Dynamic_scope.revert) =
    let resetter : revert =
      { revert =
          (fun c graph ->
            perform
              ~here
              graph
              (resetter.revert (handle ~here graph ~f:(fun graph -> c graph))))
      }
    in
    handle ~here graph ~f:(fun graph -> f resetter graph)
  ;;

  let set' ?(here = Stdlib.Lexing.dummy_pos) var value ~f graph =
    let f = f_with_resetter ~here ~f graph in
    perform ~here graph (Proc.Dynamic_scope.set' ~here var value ~f)
  ;;

  let lookup ?(here = Stdlib.Lexing.dummy_pos) var graph =
    perform ~here graph (Proc.Dynamic_scope.lookup ~here var)
  ;;

  let modify ?(here = Stdlib.Lexing.dummy_pos) var ~change ~f graph =
    let f = f_with_resetter ~here ~f graph in
    perform ~here graph (Proc.Dynamic_scope.modify ~here var ~change ~f)
  ;;
end

module Incr = struct
  let value_cutoff ?(here = Stdlib.Lexing.dummy_pos) t ~equal graph =
    perform ~here graph (Proc.Incr.value_cutoff ~here t ~equal)
  ;;

  let compute ?(here = Stdlib.Lexing.dummy_pos) t ~f graph =
    perform ~here graph (Proc.Incr.compute ~here t ~f)
  ;;

  let to_value ?(here = Stdlib.Lexing.dummy_pos) incr = Proc.Incr.to_value ~here incr

  let with_clock ?(here = Stdlib.Lexing.dummy_pos) ~f graph =
    perform ~here graph (Proc.Incr.with_clock ~here f)
  ;;
end

let assoc ?(here = Stdlib.Lexing.dummy_pos) comparator map ~f graph =
  (Proc.assoc ~here comparator map ~f:(fun k v ->
     handle ~here graph ~f:(fun graph -> f k v graph) [@nontail]) [@nontail])
  |> perform ~here graph
;;

let assoc_set ?(here = Stdlib.Lexing.dummy_pos) comparator set ~f graph =
  Proc.assoc_set ~here comparator set ~f:(fun k ->
    handle ~here graph ~f:(fun graph -> f k graph) [@nontail])
  |> perform ~here graph
;;

let assoc_list ?(here = Stdlib.Lexing.dummy_pos) comparator list ~get_key ~f graph =
  Proc.assoc_list ~here comparator list ~get_key ~f:(fun k v ->
    handle ~here graph ~f:(fun graph -> f k v graph) [@nontail])
  |> perform ~here graph
;;

module Debug = struct
  let on_change ?(here = Stdlib.Lexing.dummy_pos) v ~f graph =
    (* Use [after_display] because the incremental node is always considered to be in use.*)
    let f =
      arr1 ~here graph v ~f:(fun v ->
        f v;
        Effect.Ignore)
    in
    Edge.after_display ~here f graph
  ;;

  let on_change_print_s ?(here = Stdlib.Lexing.dummy_pos) v sexp_of =
    on_change ~here v ~f:(fun a -> print_s (sexp_of a))
  ;;

  let to_dot ?pre_process c = To_dot.to_dot ?pre_process (top_level_handle c)

  let bonsai_node_counts ?pre_process c =
    Skeleton.Counts.get ?pre_process (top_level_handle c)
  ;;

  let enable_incremental_annotations = Annotate_incr.enable
  let disable_incremental_annotations = Annotate_incr.disable

  let instrument_computation
    ?(here = Stdlib.Lexing.dummy_pos)
    c
    ~start_timer
    ~stop_timer
    graph
    =
    Instrumentation.instrument_computation (handle graph ~f:c) ~start_timer ~stop_timer
    |> perform ~here graph
  ;;

  let monitor_free_variables ?(here = Stdlib.Lexing.dummy_pos) ~f graph =
    perform
      graph
      ~here
      (Proc.monitor_free_variables ~here (handle graph ~f:(fun graph -> f graph)))
  ;;
end

let switch__for_proc2 ?(here = Stdlib.Lexing.dummy_pos) ~match_ ~branches ~with_ graph =
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
  let ( >>| ) ?(here = Stdlib.Lexing.dummy_pos) t f = map ~here t ~f

  module Let_syntax = struct
    let return = Fn.id
    let map ?(here = Stdlib.Lexing.dummy_pos) a ~f = map ~here a ~f
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

    let cutoff ?(here = Stdlib.Lexing.dummy_pos) v ~equal =
      Value.cutoff ~here v ~equal ~added_by_let_syntax:true
    ;;

    let switch ?(here = Stdlib.Lexing.dummy_pos) ~match_ ~branches ~with_ graph =
      let with_ i _graph = with_ i in
      switch__for_proc2 ~here ~match_ ~branches ~with_ graph [@nontail]
    ;;

    let switch ~here ~match_ ~branches ~with_ =
      with_global_graph
        ~f:(fun graph -> switch ~here ~match_ ~branches ~with_ graph)
        ~no_graph:(fun () ->
          raise_s
            [%message
              "match%sub called outside of the context of a graph"
                (here : Source_code_position.t)]) [@nontail]
    ;;

    let sub ?here:(_ = Stdlib.Lexing.dummy_pos) a ~f = f a
  end
end

(* These functions are here to provide the basis for the [proc_layer2.ml] which
   wants versions of these functions that don't have calls to [split ~here] in them *)
module For_proc2 = struct
  let arr1 = arr1
  let arr2 = arr2
  let arr3 = arr3
  let arr4 = arr4
  let arr5 = arr5
  let arr6 = arr6
  let arr7 = arr7
  let value_cutoff v ~equal = Value.cutoff v ~equal ~added_by_let_syntax:false
  let conceal_value v = v
  let state = state__for_proc2
  let state_opt = state_opt__for_proc2
  let toggle = toggle__for_proc2

  module Toggle = Proc.Toggle

  let toggle' ?(here = Stdlib.Lexing.dummy_pos) ~default_model graph =
    perform ~here graph (Proc.toggle' ~here ~default_model ())
  ;;

  let state_machine0 = state_machine0__for_proc2
  let state_machine1 = state_machine1__for_proc2
  let actor0 = actor0__for_proc2
  let actor1 = actor1__for_proc2
  let wrap = wrap__for_proc2

  let with_model_resetter ?(here = Stdlib.Lexing.dummy_pos) f graph =
    with_model_resetter__for_proc2 ~here ~f graph
  ;;

  let with_model_resetter' ?(here = Stdlib.Lexing.dummy_pos) f graph =
    with_model_resetter' ~here ~f graph
  ;;

  let lazy_ ?(here = Stdlib.Lexing.dummy_pos) f graph =
    delay ~here ~f:(fun graph -> Lazy.force f graph) graph
  ;;

  let switch ?(here = Stdlib.Lexing.dummy_pos) ~match_ ~branches ~with_ graph =
    switch__for_proc2 ~here ~match_ ~branches ~with_ graph
  ;;

  let on_change = Edge.on_change__for_proc2
  let on_change' = Edge.on_change'__for_proc2
  let lifecycle = Edge.lifecycle__for_proc2
  let lifecycle' = Edge.lifecycle'__for_proc2
  let after_display = Edge.after_display__for_proc2
  let after_display' = Edge.after_display'__for_proc2
  let manual_refresh = Edge.Poll.manual_refresh__for_proc2

  let debug_on_change ?(here = Stdlib.Lexing.dummy_pos) v ~f graph =
    let f =
      arr1 ~here graph v ~f:(fun v ->
        f v;
        Effect.Ignore)
    in
    Edge.after_display__for_proc2 ~here f graph
  ;;

  let debug_on_change_print_s ?(here = Stdlib.Lexing.dummy_pos) v sexp_of =
    debug_on_change ~here v ~f:(fun a -> print_s (sexp_of a))
  ;;

  let narrow ?(here = Stdlib.Lexing.dummy_pos) state_and_inject ~get ~set graph =
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

  let narrow_via_field ?(here = Stdlib.Lexing.dummy_pos) state_and_inject field =
    narrow state_and_inject ~here ~get:(Field.get field) ~set:(Field.fset field)
  ;;
end

module Conv = struct
  let handle = handle
  let top_level_handle = top_level_handle
  let perform ?(here = Stdlib.Lexing.dummy_pos) graph c = perform ~here graph c
  let reveal_value = Fn.id
  let conceal_value = Fn.id
  let isolated = isolated
end

module Map = Map0.Make (struct
    module Value = struct
      type nonrec 'a t = 'a t

      let both = both
    end

    module Computation = struct
      type nonrec 'a t = graph -> 'a t
    end

    module Incr = struct
      let compute = Incr.compute
    end
  end)
