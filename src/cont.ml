open! Core
open! Import
module Time_source = Time_source
module Apply_action_context = Apply_action_context

module Cont_primitives : sig
  (** Bonsai's [Cont] implementation is based on continuation-passing style.

      Previously, in [Proc], calls to [sub] and [arr] directly added the corresponding
      evaluation/lookup nodes to the computation graph, which was available through
      the [Computation.t]'s underlying type representation.

      As before, [Computation.t] is the static structure of the computation graph,
      and [Bonsai.t = Value.t] is the incrementally-computed "runtime" value.
      We now use that representation as an intermediate representation,
      but end-users don't construct the computation graph directly.

      Instead, we pass around a ['graph'] value. When components are combined or
      composed, Bonsai's internals add the new computation into the computation
      graph.

      The primitive API (not exposed to end-users) tries to simulate algebraic effects.
      - [perform] takes a [local_ graph -> Computation.t] component,
        evaluates it, and adds it into the computation graph.
        It returns a [Value.t] memoized alias to this new computation.
      - [handle] places a [Value.t] in the context of a fresh computation graph.
        It then wraps the result in a [Computation.t].
        This is generally used to construct a portion of the computation graph
        in isolation. The subgraph is then added to the main graph via [perform].

      At the top level, [top_level_handle] is similar to [handle], but only takes [f].
      In the context of a fresh graph, it runs [f graph] to obtain the total composed
      [Value.t] for a Bonsai app. Then, it raises that to a [Computation.t],
      and passes {i that i} through the [graph.f] that we've been assembling
      all this time.

      But what's going on under the surface? And what is [graph]?
      And why can the new Bonsai API use [let%map]?

      ---

      If you look at computation.ml, you'll see that [Computation.t] is a nested variant
      of nodes that store things. These include state, assoc/switch, store/fetch for
      Dynamic_scope, and most commonly used, sub. This is the memoization-like
      pattern that powers work-sharing for incrementality: we can store a reusable
      value with a unique name (via Type_equal.id), which downstream incremental [Value.t]s
      can reference (and reuse!) instead of repeating work. [Sub] has 3 fields:

      - [from] is the computation being memoized
      - [via] is the [Type_equal.id] identifier
      - [into] is the downstream computation that uses the value of [from].

      So if we want to depend on several [sub]ed computations, our graph will look as follows:

      {v
      Sub { from=comp1; via=<name1>; into =
        Sub { from=comp2; via=<name2>; into =
        ... into = thing_we_want_to_compute ...
        }
      }
      v}

      ---

      The [graph] we pass around is a ref to a function that will eventually compute this
      [Computation.t]. We can't construct it eagerly as we go, since that would require
      making the fields of [Computation.t] mutable, so we need to do some functional
      trickery.

      [graph] starts as [Fn.id]. At each invocation of [perform],
      we overwrite it to still return its input, but nested in a new layer of [Sub].
      Let's look at how this works in practice:

      {v
      1.let my_component graph =
      2.  let%sub model1, _inject = Bonsai.state 5 in
      3.  let%sub model2, _inject = Bonsai.state 6 in
      4.  model2

      1. graph = { f = fun r -> r }
      2. graph = { f = fun r -> Sub { from=state 5; via=<model1>; into=r } }
      3. graph = { f = fun r -> Sub { from=state 5; via=<model1>; into=Sub { from=state 6; via=<model2>; into=r } } }
      4. graph.f (<model2>) ==> Sub { from=state 5; via=<model1>; into=Sub { from=state 6; via=<model2>; into=<model2> } }
      v}

      Instead of [state 5/6], we'd actually have something more like [Leaf0 { ... }],
      but that's not really relevant. We also do some optimizations if [sub]ing isn't
      actually necessary.

      Handle is much simpler: we run our [Value.t] construction function ([f]) on a fresh
      [graph], which becomes constructed via calls to [perform] presumably inside [f].
      Then, we call [graph.f] on that result, so that it is inside the nested [Sub]s,
      giving it access to those memoized aliases. Finally, we wrap this in a [Computation.t],
      which can be [perform]ed into other computations.

      ---

      But why can we now use [let%map.Bonsai] instead of [let%sub] and [let%arr] combos?
      After all, we're not passing [graph] to [let%map]...

      In short, we cheat and make [graph] global. We still require users to pass it
      when possible, so that creating costly stateful nodes is an explicit operation.
      However, we don't want people to use these "magic" [let%map] and [match%sub] calls
      unless they are in the dynamic scope of a Bonsai top level.
      We enforce this by maintaining a global boolean ref for whether we're in a top level,
      and erroring on [perform] if we are not.
  *)

  type graph

  (* Main primitives; see above for explanation. *)
  val perform : ?here:Source_code_position.t -> graph -> 'a Computation.t -> 'a Value.t
  val handle : f:(graph -> 'a Value.t) -> graph -> 'a Computation.t

  (* Special-use primitives for getting the global graph, and creating it in the top level. *)
  val isolated : graph -> f:(unit -> 'a Value.t) -> 'a Computation.t
  val top_level_handle : (graph -> 'a Value.t) -> 'a Computation.t
  val handle_for_lazy : (graph -> 'a Value.t) -> 'a Computation.t
  val with_global_graph : f:(graph -> 'a) -> no_graph:(unit -> 'a) -> 'a
end = struct
  type graph = { mutable f : 'a. 'a Computation.t -> 'a Computation.t }

  let perform
    : type a. ?here:Source_code_position.t -> graph -> a Computation.t -> a Value.t
    =
    fun ?here graph -> function
    | Return { value = (Named _ | Constant _ | Exception _) as value; id; _ } ->
      (* Introduce the optimization [let%sub a = return foo in use a] => [use foo]
           This only makes sense if the Value.t being returned is either a constant or an
           already-bound named value, otherwise you risk losing value sharing. *)
      { Value.value; id; here = None }
    | computation_to_perform ->
      (* Mint a fresh type-id to hold the result of performing this graph modification  *)
      let via : a Type_equal.Id.t = Type_equal.Id.create ~name:"" [%sexp_of: opaque] in
      (* Keep hold of the previous graph-modification function *)
      let old_f : type b. b Computation.t -> b Computation.t = graph.f in
      let new_f : type x. x Computation.t -> x Computation.t = function
        | Return { value = Named _; id; _ } when Type_equal.Id.same via id ->
          (* introduce the optimization {[ let%sub a = foo bar in return a ]} => {[ foo bar ]} *)
          let T = Type_equal.Id.same_witness_exn via id in
          old_f computation_to_perform
        | eventual_result ->
          (* old_f takes the eventual innermost result, and wraps it in 0+ layers of subs.
               We replace it with a new function that adds another layer to the inside. *)
          old_f
            (Sub
               { from = computation_to_perform; via; into = eventual_result; here = None })
      in
      (* write the new hole into the graph, and return a new value referencing the
           type-id that will be populated when [new_f] is invoked. *)
      graph.f <- new_f;
      Value.named (Sub here) via
  ;;

  (* [isolated] runs [f] on a fresh graph context. As an implementation detail, we actually
     mutate the same ['graph'], so that [the_one_and_only] is kept up to date.
     [isolated] also has an exception handler that returns any exceptions inside a Value.t.
     This restricts the return type of [isolated] to ['a Computation.t]. *)
  let isolated graph ~f =
    let backup_f = graph.f in
    graph.f <- Fn.id;
    try
      let r = f () in
      let r = graph.f (Proc.read r) in
      graph.f <- backup_f;
      r
    with
    | exn ->
      graph.f <- backup_f;
      Proc.read (Value.return_exn exn)
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
  let handle ~f graph = isolated graph ~f:(fun () -> f graph) [@nontail]

  let handle_with_global_graph inside_a_lazy f =
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
        computation_context (Proc_min.read v) [@nontail])
      ~finally:(fun () -> decr num_nested_top_level_handles) [@nontail]
  ;;

  let handle_for_lazy f = handle_with_global_graph `Inside_lazy f

  (* Meant to be called at bonsai entrypoints only, [top_level_handle] uses the
     singleton graph and sets [nested_top_level_handles] acordingly. *)
  let top_level_handle f = handle_with_global_graph `Not_inside_lazy f

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
let arr1 graph a ~f = perform graph (Proc.read (Proc.Value.map a ~f))
let arr2 graph a b ~f = perform graph (Proc.read (Proc.Value.map2 a b ~f))
let arr3 graph a b c ~f = perform graph (Proc.read (Proc.Value.map3 a b c ~f))
let arr4 graph a b c d ~f = perform graph (Proc.read (Proc.Value.map4 a b c d ~f))
let arr5 graph a b c d e ~f = perform graph (Proc.read (Proc.Value.map5 a b c d e ~f))
let arr6 graph a b c d e g ~f = perform graph (Proc.read (Proc.Value.map6 a b c d e g ~f))

let arr7 graph a b c d e g h ~f =
  perform graph (Proc.read (Proc.Value.map7 a b c d e g h ~f))
;;

(* If we aren't inside of a [top_level_handle], then fall back to using [Value.map] *)
let map a ~f =
  with_global_graph ~f:(fun graph -> arr1 graph a ~f) ~no_graph:(fun () -> Value.map a ~f)
;;

let map2 a b ~f =
  with_global_graph
    ~f:(fun graph -> arr2 graph a b ~f)
    ~no_graph:(fun () -> Value.map2 a b ~f)
;;

include Applicative.Make_using_map2 (struct
  type nonrec 'a t = 'a t

  let return = return
  let map2 = map2
  let map = `Custom map
end)

let map3 a b c ~f =
  with_global_graph
    ~f:(fun graph -> arr3 graph a b c ~f)
    ~no_graph:(fun () -> Value.map3 a b c ~f)
;;

let map4 a b c d ~f =
  with_global_graph
    ~f:(fun graph -> arr4 graph a b c d ~f)
    ~no_graph:(fun () -> Value.map4 a b c d ~f)
;;

let map5 a b c d e ~f =
  with_global_graph
    ~f:(fun graph -> arr5 graph a b c d e ~f)
    ~no_graph:(fun () -> Value.map5 a b c d e ~f)
;;

let map6 a b c d e g ~f =
  with_global_graph
    ~f:(fun graph -> arr6 graph a b c d e g ~f)
    ~no_graph:(fun () -> Value.map6 a b c d e g ~f)
;;

let map7 a b c d e g h ~f =
  with_global_graph
    ~f:(fun graph -> arr7 graph a b c d e g h ~f)
    ~no_graph:(fun () -> Value.map7 a b c d e g h ~f)
;;

let both a b = map2 a b ~f:Tuple2.create

let transpose_opt opt =
  Option.value_map opt ~default:(return None) ~f:(map ~f:Option.some)
;;

let path_id graph = perform graph Proc.path_id

let split graph tuple =
  let a = arr1 graph tuple ~f:Tuple2.get1 in
  let b = arr1 graph tuple ~f:Tuple2.get2 in
  a, b
;;

let state__for_proc2 ?reset ?sexp_of_model ?equal default_model graph =
  perform graph (Proc.state ?reset ?sexp_of_model ?equal default_model)
;;

let state ?reset ?sexp_of_model ?equal default_model graph =
  state__for_proc2 ?reset ?sexp_of_model ?equal default_model graph |> split graph
;;

let state_opt__for_proc2 ?reset ?default_model ?sexp_of_model ?equal () graph =
  perform graph (Proc.state_opt ?reset ?sexp_of_model ?equal ?default_model ())
;;

let state_opt ?reset ?sexp_of_model ?equal ?default_model graph =
  state_opt__for_proc2 ?reset ?sexp_of_model ?equal ?default_model () graph |> split graph
;;

let toggle__for_proc2 ~default_model graph = perform graph (Proc.toggle ~default_model)
let toggle ~default_model graph = toggle__for_proc2 ~default_model graph |> split graph

module Toggle = struct
  type 'a v = 'a t

  type t =
    { state : bool v
    ; set_state : (bool -> unit Effect.t) v
    ; toggle : unit Effect.t v
    }
  [@@deriving fields ~getters]
end

let toggle' ~default_model graph =
  let all = perform graph (Proc.toggle' ~default_model) in
  let state = arr1 graph all ~f:(fun { Proc.Toggle.state; _ } -> state) in
  let set_state = arr1 graph all ~f:(fun { Proc.Toggle.set_state; _ } -> set_state) in
  let toggle = arr1 graph all ~f:(fun { Proc.Toggle.toggle; _ } -> toggle) in
  { Toggle.state; set_state; toggle }
;;

module Path = Path

let path graph = perform graph Proc.path

let state_machine0__for_proc2
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
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ()
    ~default_model
    ~apply_action
  |> perform graph
;;

let state_machine0
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~apply_action
  graph
  =
  state_machine0__for_proc2
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~apply_action
    ()
    graph
  |> split graph
;;

module Computation_status = Proc.Computation_status

let state_machine1__for_proc2
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
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~apply_action
    input
  |> perform graph
;;

let state_machine1
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
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~apply_action
    input
    graph
  |> split graph
;;

let actor0__for_proc2
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~recv
  ()
  graph
  =
  Proc.actor0 ?reset ?sexp_of_model ?sexp_of_action ?equal ~default_model ~recv ()
  |> perform graph
;;

let actor0 ?reset ?sexp_of_model ?sexp_of_action ?equal ~default_model ~recv graph =
  actor0__for_proc2
    ?reset
    ?sexp_of_model
    ?sexp_of_action
    ?equal
    ~default_model
    ~recv
    ()
    graph
  |> split graph
;;

let actor1__for_proc2
  ?sexp_of_action
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~recv
  input
  graph
  =
  Proc.actor1 ?reset ?sexp_of_model ?sexp_of_action ?equal ~default_model ~recv input
  |> perform graph
;;

let actor1 ?sexp_of_action ?reset ?sexp_of_model ?equal ~default_model ~recv input graph =
  actor1__for_proc2
    ?sexp_of_action
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~recv
    input
    graph
  |> split graph
;;

let delay ~f graph = Proc.lazy_ (lazy (handle_for_lazy f)) |> perform graph

module Expert = struct
  let thunk ~f graph = perform graph (Proc.thunk f)

  let assoc_on io_cmp model_cmp map ~get_model_key ~f graph =
    Proc.assoc_on io_cmp model_cmp map ~get_model_key ~f:(fun k v ->
      handle graph ~f:(fun graph -> f k v graph) [@nontail])
    |> perform graph
  ;;

  let delay = delay
end

let freeze ?sexp_of_model ?equal v graph =
  perform graph (Proc.freeze ?sexp_of_model ?equal v)
;;

let fix v ~f graph =
  let rec recurse i2 graph = delay graph ~f:(fun graph -> f ~recurse i2 graph) in
  f ~recurse v graph
;;

let fix2 a b ~f graph =
  let rec recurse a b graph = delay graph ~f:(fun graph -> f ~recurse a b graph) in
  f ~recurse a b graph
;;

let scope_model comparator ~on ~for_ graph =
  Proc.scope_model comparator ~on (handle graph ~f:(fun graph -> for_ graph))
  |> perform graph
;;

let most_recent_some ?sexp_of_model ~equal value ~f graph =
  Proc.most_recent_some ?sexp_of_model ~equal value ~f |> perform graph
;;

let most_recent_value_satisfying ?sexp_of_model ~equal value ~condition graph =
  Proc.most_recent_value_satisfying ?sexp_of_model ~equal value ~condition
  |> perform graph
;;

let previous_value ?sexp_of_model ~equal value graph =
  Proc.previous_value ?sexp_of_model ~equal value |> perform graph
;;

let wrap__for_proc2 ?reset ?sexp_of_model ?equal ~default_model ~apply_action ~f () graph =
  Proc_min.wrap
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~apply_action
    ()
    ~f:(fun model inject ->
    handle graph ~f:(fun graph -> f model inject graph) [@nontail])
  |> perform graph
;;

let wrap ?reset ?sexp_of_model ?equal ~default_model ~apply_action ~f graph =
  wrap__for_proc2 ?reset ?sexp_of_model ?equal ~default_model ~apply_action ~f () graph
;;

let with_model_resetter__for_proc2 ~f graph =
  perform graph (Proc.with_model_resetter (handle graph ~f:(fun graph -> f graph)))
;;

let with_model_resetter ~f graph = with_model_resetter__for_proc2 ~f graph |> split graph

let with_model_resetter' ~f graph =
  Proc_min.with_model_resetter (fun ~reset ->
    handle graph ~f:(fun graph -> f ~reset graph) [@nontail])
  |> perform graph
;;

let peek value graph = perform graph (Proc.yoink value)
let ignore_t (_ : unit t) = ()

module Clock = struct
  let approx_now ~tick_every graph = perform graph (Proc.Clock.approx_now ~tick_every)
  let now graph = perform graph Proc.Clock.now

  module Before_or_after = struct
    type t = Ui_incr.Before_or_after.t =
      | Before
      | After
    [@@deriving sexp, equal]
  end

  let at time graph = perform graph (Proc.Clock.at time)

  let every ~when_to_start_next_effect ?trigger_on_activate span callback graph =
    Proc.Clock.every ~when_to_start_next_effect ?trigger_on_activate span callback
    |> perform graph
    |> ignore_t
  ;;

  let get_current_time graph = perform graph Proc.Clock.get_current_time
  let sleep graph = perform graph Proc.Clock.sleep
  let until graph = perform graph Proc.Clock.until
end

module Edge = struct
  let on_change__for_proc2 ?sexp_of_model ~equal value ~callback graph =
    perform graph (Proc.Edge.on_change ?sexp_of_model ~equal value ~callback)
  ;;

  let on_change ?sexp_of_model ~equal value ~callback graph =
    ignore_t (on_change__for_proc2 ?sexp_of_model ~equal value ~callback graph)
  ;;

  let on_change'__for_proc2 ?sexp_of_model ~equal value ~callback graph =
    perform graph (Proc.Edge.on_change' ?sexp_of_model ~equal value ~callback)
  ;;

  let on_change' ?sexp_of_model ~equal value ~callback graph =
    ignore_t (on_change'__for_proc2 ?sexp_of_model ~equal value ~callback graph)
  ;;

  let lifecycle__for_proc2 ?on_activate ?on_deactivate ?after_display () graph =
    perform graph (Proc.Edge.lifecycle ?on_activate ?on_deactivate ?after_display ())
  ;;

  let lifecycle ?on_activate ?on_deactivate ?after_display graph =
    ignore_t (lifecycle__for_proc2 ?on_activate ?on_deactivate ?after_display () graph)
  ;;

  let lifecycle'__for_proc2 ?on_activate ?on_deactivate ?after_display () graph =
    perform graph (Proc.Edge.lifecycle' ?on_activate ?on_deactivate ?after_display ())
  ;;

  let lifecycle' ?on_activate ?on_deactivate ?after_display graph =
    ignore_t (lifecycle'__for_proc2 ?on_activate ?on_deactivate ?after_display () graph)
  ;;

  let after_display__for_proc2 callback graph =
    perform graph (Proc.Edge.after_display callback)
  ;;

  let after_display callback graph = ignore_t (after_display__for_proc2 callback graph)

  let after_display'__for_proc2 callback graph =
    perform graph (Proc.Edge.after_display' callback)
  ;;

  let after_display' callback graph = ignore_t (after_display'__for_proc2 callback graph)
  let wait_after_display graph = perform graph Proc.Edge.wait_after_display

  module Poll = struct
    module Starting = Proc.Edge.Poll.Starting

    let effect_on_change
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
        ?sexp_of_input
        ?sexp_of_result
        ~equal_input
        ?equal_result
        starting
        value
        ~effect
      |> perform graph
    ;;

    let manual_refresh__for_proc2 ?sexp_of_model ?equal starting ~effect graph =
      perform graph (Proc.Edge.Poll.manual_refresh ?sexp_of_model ?equal starting ~effect)
    ;;

    let manual_refresh ?sexp_of_model ?equal starting ~effect graph =
      manual_refresh__for_proc2 ?sexp_of_model ?equal starting ~effect graph
      |> split graph
    ;;
  end
end

module Memo = struct
  type ('input, 'result) t = ('input, 'result) Proc.Memo.t

  let create cmp ~f graph =
    Proc.Memo.create cmp ~f:(fun v -> handle graph ~f:(fun graph -> f v graph) [@nontail])
    |> perform graph
  ;;

  let lookup ?sexp_of_model ~equal t input graph =
    perform graph (Proc.Memo.lookup ?sexp_of_model ~equal t input)
  ;;
end

module Effect_throttling = struct
  module Poll_result = Proc.Effect_throttling.Poll_result

  let poll callback graph = perform graph (Proc.Effect_throttling.poll callback)
end

module Dynamic_scope = struct
  type 'a bonsai_t = 'a t
  type 'a t = 'a Proc.Dynamic_scope.t
  type revert = { revert : 'a. (graph -> 'a bonsai_t) -> graph -> 'a bonsai_t }

  let create = Proc.Dynamic_scope.create
  let derived = Proc.Dynamic_scope.derived

  let set var value ~inside graph =
    let inside = handle graph ~f:(fun graph -> inside graph) in
    perform graph (Proc.Dynamic_scope.set var value ~inside)
  ;;

  let f_with_resetter ~f graph (resetter : Proc.Dynamic_scope.revert) =
    let resetter : revert =
      { revert =
          (fun c graph ->
            perform graph (resetter.revert (handle graph ~f:(fun graph -> c graph))))
      }
    in
    handle graph ~f:(fun graph -> f resetter graph)
  ;;

  let set' var value ~f graph =
    let f = f_with_resetter ~f graph in
    perform graph (Proc.Dynamic_scope.set' var value ~f)
  ;;

  let lookup var graph = perform graph (Proc.Dynamic_scope.lookup var)

  let modify var ~change ~f graph =
    let f = f_with_resetter ~f graph in
    perform graph (Proc.Dynamic_scope.modify var ~change ~f)
  ;;
end

module Incr = struct
  let value_cutoff t ~equal graph = perform graph (Proc.Incr.value_cutoff t ~equal)
  let compute t ~f graph = perform graph (Proc.Incr.compute t ~f)
  let to_value incr = Proc.Incr.to_value incr
  let with_clock ~f graph = perform graph (Proc.Incr.with_clock f)
end

let assoc comparator map ~f graph =
  (Proc.assoc comparator map ~f:(fun k v ->
     handle graph ~f:(fun graph -> f k v graph) [@nontail]) [@nontail])
  |> perform graph
;;

let assoc_set comparator set ~f graph =
  Proc.assoc_set comparator set ~f:(fun k ->
    handle graph ~f:(fun graph -> f k graph) [@nontail])
  |> perform graph
;;

let assoc_list comparator list ~get_key ~f graph =
  Proc.assoc_list comparator list ~get_key ~f:(fun k v ->
    handle graph ~f:(fun graph -> f k v graph) [@nontail])
  |> perform graph
;;

module Debug = struct
  let on_change v ~f graph =
    (* Use [after_display] because the incremental node is always considered to be in use.*)
    let f =
      arr1 graph v ~f:(fun v ->
        f v;
        Effect.Ignore)
    in
    Edge.after_display f graph
  ;;

  let on_change_print_s v sexp_of = on_change v ~f:(fun a -> print_s (sexp_of a))
  let to_dot ?pre_process c = To_dot.to_dot ?pre_process (top_level_handle c)
  let enable_incremental_annotations = Annotate_incr.enable
  let disable_incremental_annotations = Annotate_incr.disable

  let instrument_computation c ~start_timer ~stop_timer graph =
    Instrumentation.instrument_computation (handle graph ~f:c) ~start_timer ~stop_timer
    |> perform graph
  ;;
end

let switch__for_proc2 ~match_ ~branches ~with_ graph =
  let arms =
    let arms = ref [] in
    for i = 0 to branches - 1 do
      let computation = isolated graph ~f:(fun () -> with_ i graph) in
      arms := (i, computation) :: !arms
    done;
    !arms
  in
  Computation.Switch { match_; arms = Map.of_alist_exn (module Int) arms; here = [%here] }
  |> perform graph
;;

module Let_syntax = struct
  let return = return
  let ( >>| ) t f = map t ~f

  module Let_syntax = struct
    let return = Fn.id
    let map ?here:_ a ~f = map a ~f
    let arr ?here:_ a ~f = map a ~f
    let map2 = map2
    let map3 = map3
    let map4 = map4
    let map5 = map5
    let map6 = map6
    let map7 = map7
    let both = both
    let cutoff v ~equal = Value.cutoff v ~equal ~added_by_let_syntax:true

    let switch ~here:_ ~match_ ~branches ~with_ graph =
      let with_ i _graph = with_ i in
      switch__for_proc2 ~match_ ~branches ~with_ graph [@nontail]
    ;;

    let switch ~here ~match_ ~branches ~with_ =
      with_global_graph
        ~f:(fun graph -> switch ~here ~match_ ~branches ~with_ graph)
        ~no_graph:(fun () ->
          failwith "match%sub called outside of the context of a graph") [@nontail]
    ;;

    let sub ?here:_ a ~f = f a
  end
end

(* These functions are here to provide the basis for the [proc_layer2.ml] which
   wants versions of these functions that don't have calls to [split] in them *)
module For_proc2 = struct
  let arr1_with_location ?here graph a ~f =
    perform graph (Proc.read (Proc.Let_syntax.Let_syntax.map ?here a ~f))
  ;;

  let value_cutoff v ~equal = Value.cutoff v ~equal ~added_by_let_syntax:false
  let conceal_value v = v
  let state = state__for_proc2
  let state_opt = state_opt__for_proc2
  let toggle = toggle__for_proc2

  module Toggle = Proc.Toggle

  let toggle' ~default_model graph = perform graph (Proc.toggle' ~default_model)
  let state_machine0 = state_machine0__for_proc2
  let state_machine1 = state_machine1__for_proc2
  let actor0 = actor0__for_proc2
  let actor1 = actor1__for_proc2
  let wrap = wrap__for_proc2
  let with_model_resetter f graph = with_model_resetter__for_proc2 ~f graph
  let with_model_resetter' f graph = with_model_resetter' ~f graph
  let lazy_ f graph = delay ~f:(fun graph -> Lazy.force f graph) graph

  let switch ~match_ ~branches ~with_ graph =
    switch__for_proc2 ~match_ ~branches ~with_ graph
  ;;

  let on_change = Edge.on_change__for_proc2
  let on_change' = Edge.on_change'__for_proc2
  let lifecycle = Edge.lifecycle__for_proc2
  let lifecycle' = Edge.lifecycle'__for_proc2
  let after_display = Edge.after_display__for_proc2
  let after_display' = Edge.after_display'__for_proc2
  let manual_refresh = Edge.Poll.manual_refresh__for_proc2

  let debug_on_change v ~f graph =
    let f =
      arr1 graph v ~f:(fun v ->
        f v;
        Effect.Ignore)
    in
    Edge.after_display__for_proc2 f graph
  ;;

  let debug_on_change_print_s v sexp_of =
    debug_on_change v ~f:(fun a -> print_s (sexp_of a))
  ;;

  let narrow state_and_inject ~get ~set graph =
    let open Let_syntax in
    let state, inject = state_and_inject |> split graph in
    let inject =
      let peek_state = peek state graph in
      let%map peek_state = peek_state
      and inject = inject in
      fun a ->
        match%bind.Effect peek_state with
        | Inactive -> Effect.Ignore
        | Active state -> inject (set state a)
    in
    let state =
      let%map state = state in
      get state
    in
    let%map state = state
    and inject = inject in
    state, inject
  ;;

  let narrow_via_field state_and_inject field =
    narrow state_and_inject ~get:(Field.get field) ~set:(Field.fset field)
  ;;
end

module Conv = struct
  let handle = handle
  let top_level_handle = top_level_handle
  let perform = perform
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
