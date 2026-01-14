open! Core
open! Import

(* A collection of stats about the stabilization tracker. These are exposed in the
   [For_testing] module, and are used for testing the internals only. *)
module Stats = struct
  type t =
    { mutable num_stabilize : int
    ; mutable num_don't_stabilize : int
    ; mutable num_stabilize_caused_by_vars : int
    ; mutable num_prunes_run : int
    ; mutable num_branches_pruned : int
    }
  [@@deriving fields ~getters]

  let create () =
    { num_stabilize = 0
    ; num_don't_stabilize = 0
    ; num_stabilize_caused_by_vars = 0
    ; num_prunes_run = 0
    ; num_branches_pruned = 0
    }
  ;;

  let incr_stabilize t = t.num_stabilize <- t.num_stabilize + 1
  let incr_don't_stabilize t = t.num_don't_stabilize <- t.num_don't_stabilize + 1

  let incr_stabilize_caused_by_vars t =
    t.num_stabilize_caused_by_vars <- t.num_stabilize_caused_by_vars + 1
  ;;

  let incr_prunes_run t = t.num_prunes_run <- t.num_prunes_run + 1
  let incr_branches_pruned t = t.num_branches_pruned <- t.num_branches_pruned + 1

  let display t =
    print_s
      [%sexp
        { stabilizations_before_actions : int = t.num_stabilize
        ; stabilizations_caused_by_var_changes : int = t.num_stabilize_caused_by_vars
        ; stabilizations_skipped : int = t.num_don't_stabilize
        ; prunes_run : int = t.num_prunes_run
        ; branches_pruned : int = t.num_branches_pruned
        }]
  ;;
end

(* [dirty_incremental_vars] tracks whether any [Incr.Var.t]s have been set since the last
   stabilization. This is global because there is only one Incremental universe for all
   Bonsai apps. If we ever decide to parameterize Bonsai apps over an Incremental
   universe, this state should get pulled into the trackers themselves. *)
let dirty_incremental_vars = ref false
let mark_incremental_dirty () = dirty_incremental_vars := true
let mark_incremental_clean () = dirty_incremental_vars := false

module Generation = struct
  include Int

  let initial = 0
  let next = Int.succ
end

module Action_trie = struct
  let max_seconds_between_prunes = 15

  (* If an assoc or switch hasn't received an action in the last
     [num_generations_to_stale] generations, then we'll assume the branch is no longer
     active and remove it from our map. Note that generations increase whenever we need to
     stabilize for an action. This number is mostly arbitrary and should be tweaked as
     necessary.

     The number below is: (min stabilization / frame) * (frames / sec) * sec between
     prunes
  *)
  let num_generations_to_stale = 3 * 60 * max_seconds_between_prunes

  type 'inner with_generation =
    { mutable generation : Generation.t
    ; mutable inner : 'inner
    }

  let with_empty_generation inner = { generation = -1; inner }

  type 'inner node =
    | Unexplored : _ node
    | Terminal : 'inner Action.leaf node
    | Sub :
        { from : 'from node with_generation
        ; into : 'into node with_generation
        }
        -> ('from, 'into) Action.sub node
    | Wrap :
        { outer : unit with_generation
        ; inner : 'inner node with_generation
        }
        -> ('inner, 'outer) Action.wrap node
    | Model_reset :
        { outer : unit with_generation
        ; inner : 'inner node with_generation
        }
        -> 'inner Action.model_resetter node
    | Lazy : packed -> Action.lazy_ node
    | Assoc : 'inner assoc -> (_, 'inner) Action.assoc node
    | Assoc_on : 'inner assoc_on -> (_, _, 'inner) Action.assoc_on node
    | Switch : switch -> Action.switch node

  and 'inner assoc = { mutable by_key : 'inner node with_generation Keyed.Map.t }
  and 'inner assoc_on = { mutable by_io_key : 'inner node with_generation Keyed.Map.t }
  and switch = { mutable by_branch : packed Int.Map.t }

  and packed =
    | T :
        { inner : 'inner node with_generation
        ; type_id : 'inner Action.id
        }
        -> packed

  type 'inner t = 'inner node with_generation

  let empty () = with_empty_generation Unexplored

  let node_of_action current_generation action =
    let with_current_generation inner = { generation = current_generation; inner } in
    let rec node_of_action : type stripped. stripped Action.t -> stripped node = function
      | Leaf_static _ -> Terminal
      | Leaf_dynamic _ -> Terminal
      | Sub_from from ->
        let from = with_current_generation (node_of_action from) in
        Sub { from; into = empty () }
      | Sub_into into ->
        let into = with_current_generation (node_of_action into) in
        Sub { from = empty (); into }
      | Wrap_inner inner ->
        let inner = with_current_generation (node_of_action inner) in
        Wrap { outer = with_empty_generation (); inner }
      | Wrap_outer _ -> Wrap { outer = with_current_generation (); inner = empty () }
      | Model_reset_inner inner ->
        let inner = with_current_generation (node_of_action inner) in
        Model_reset { outer = with_empty_generation (); inner }
      | Model_reset_outer ->
        Model_reset { outer = with_current_generation (); inner = empty () }
      | Switch { branch; type_id; action } ->
        let inner = with_current_generation (node_of_action action) in
        let inner = T { inner; type_id } in
        Switch { by_branch = Int.Map.singleton branch inner }
      | Lazy { type_id; action } ->
        let inner = with_current_generation (node_of_action action) in
        Lazy (T { inner; type_id })
      | Assoc { key; id; compare; action } ->
        let inner = with_current_generation (node_of_action action) in
        Assoc { by_key = Keyed.Map.singleton (Keyed.create ~key ~id ~compare) inner }
      | Assoc_on { io_key; model_key = _; io_id; io_compare; action } ->
        let inner = with_current_generation (node_of_action action) in
        Assoc_on
          { by_io_key =
              Keyed.Map.singleton
                (Keyed.create ~key:io_key ~id:io_id ~compare:io_compare)
                inner
          }
    in
    node_of_action action
  ;;

  type 'state traverser =
    { unexplored :
        'dynamic 'static 'stripped. 'state -> 'stripped t -> 'stripped Action.t -> 'state
    ; dynamic_leaf : 'state -> 'state
    ; static_leaf : 'state -> 'state
    ; sub :
        'dynamic 'static 'from 'into.
        'state
        -> from:'from t
        -> into:'into t
        -> ('from, 'into) Action.sub Action.t
        -> 'state
    ; wrap :
        'dynamic 'static 'inner 'outer.
        'state
        -> inner:'inner t
        -> outer:unit with_generation
        -> ('inner, 'outer) Action.wrap Action.t
        -> 'state
    ; model_reset :
        'dynamic 'static 'inner.
        'state
        -> inner:'inner t
        -> outer:unit with_generation
        -> 'inner Action.model_resetter Action.t
        -> 'state
    ; lazy_ :
        'dynamic 'static 'stripped. 'state -> 'stripped t -> 'stripped Action.t -> 'state
    ; assoc :
        'dynamic 'static 'inner 'key.
        'state
        -> assoc:'inner assoc
        -> key:Keyed.t
        -> ('key, 'inner) Action.assoc Action.t
        -> 'state
    ; assoc_on :
        'dynamic 'static 'inner 'io_key 'model_key.
        'state
        -> assoc_on:'inner assoc_on
        -> key:Keyed.t
        -> ('io_key, 'model_key, 'inner) Action.assoc_on Action.t
        -> 'state
    ; switch :
        'dynamic 'static.
        'state -> switch:switch -> branch:int -> Action.switch Action.t -> 'state
    }

  let traverse (type state) ~(initial_state : state) (traverser : state traverser) =
    let rec loop : type stripped. state -> stripped Action.t -> stripped t -> state =
      fun state action t ->
      match t.inner with
      | Unexplored -> traverser.unexplored state t action
      | Terminal ->
        (match action with
         | Leaf_dynamic _ -> traverser.dynamic_leaf state
         | Leaf_static _ -> traverser.static_leaf state)
      | Sub { from; into } ->
        let state = traverser.sub state ~from ~into action in
        (match action with
         | Sub_from from_action -> loop state from_action from
         | Sub_into into_action -> loop state into_action into)
      | Wrap { inner; outer } ->
        let state = traverser.wrap state ~inner ~outer action in
        (match action with
         | Wrap_inner inner_action -> loop state inner_action inner
         | Wrap_outer _ -> traverser.dynamic_leaf state)
      | Model_reset { inner; outer } ->
        let state = traverser.model_reset state ~inner ~outer action in
        (match action with
         | Model_reset_inner inner_action -> loop state inner_action inner
         | Model_reset_outer -> traverser.static_leaf state)
      | Lazy (T { type_id; inner }) ->
        let (Lazy { type_id = action_type_id; action }) = action in
        let T = Action.Type_id.same_witness_exn type_id action_type_id in
        let state = traverser.lazy_ state inner action in
        loop state action inner
      | Assoc assoc ->
        let (Assoc { key; id; compare; action = inner_action }) = action in
        let keyed = Keyed.create ~key ~id ~compare in
        let state = traverser.assoc state ~assoc ~key:keyed action in
        (match Map.find assoc.by_key keyed with
         | None ->
           let empty = empty () in
           assoc.by_key <- Map.set assoc.by_key ~key:keyed ~data:empty;
           traverser.unexplored state empty inner_action
         | Some inner -> loop state inner_action inner)
      | Assoc_on assoc_on ->
        let (Assoc_on { io_key; model_key = _; io_compare; io_id; action = inner_action })
          =
          action
        in
        let keyed = Keyed.create ~key:io_key ~id:io_id ~compare:io_compare in
        let state = traverser.assoc_on state ~assoc_on ~key:keyed action in
        (match Map.find assoc_on.by_io_key keyed with
         | None ->
           let empty = empty () in
           assoc_on.by_io_key <- Map.set assoc_on.by_io_key ~key:keyed ~data:empty;
           traverser.unexplored state empty inner_action
         | Some inner -> loop state inner_action inner)
      | Switch switch ->
        let (Switch { branch; type_id = action_type_id; action = inner_action }) =
          action
        in
        let state = traverser.switch state ~switch ~branch action in
        (match Map.find switch.by_branch branch with
         | None ->
           let empty = empty () in
           switch.by_branch
           <- Map.set
                switch.by_branch
                ~key:branch
                ~data:(T { inner = empty; type_id = action_type_id });
           traverser.unexplored state empty inner_action
         | Some (T { inner; type_id }) ->
           let T = Action.Type_id.same_witness_exn type_id action_type_id in
           loop state inner_action inner)
    in
    loop initial_state
  ;;

  let rec is_dynamic_action : type a. a Action.t -> bool = function
    | Leaf_dynamic _ -> true
    | Wrap_outer _ -> true
    | Model_reset_outer -> false
    | Leaf_static _ -> false
    | Sub_from action -> is_dynamic_action action
    | Sub_into action -> is_dynamic_action action
    | Wrap_inner action -> is_dynamic_action action
    | Model_reset_inner action -> is_dynamic_action action
    | Switch { action; type_id = _; branch = _ } -> is_dynamic_action action
    | Lazy { action; type_id = _ } -> is_dynamic_action action
    | Assoc { action; key = _; id = _; compare = _ } -> is_dynamic_action action
    | Assoc_on { action; io_key = _; io_id = _; io_compare = _; model_key = _ } ->
      is_dynamic_action action
  ;;

  let should_stabilize_dynamic_action stats found_conflict =
    (* Update internal bookkeeping for testing. *)
    if !dirty_incremental_vars then Stats.incr_stabilize_caused_by_vars stats;
    let should_stabilize = !dirty_incremental_vars || found_conflict in
    if should_stabilize
    then Stats.incr_stabilize stats
    else Stats.incr_don't_stabilize stats;
    should_stabilize
  ;;

  (* [requires_stabilization] determines whether we need to stabilize the incremental
     graph before applying a bonsai action. The current conditions for stabilization are:
     {v
       * A dynamic action destined for the into branch of a sub requires stabilization if
         the from branch has already received an action this generation
       * A dynamic action destined for a wrap requires a stabilization if:
           1. The action is destined for the inner computation and an outer action has
           already been applied this generation
           2. The action is destined for the outer apply_action and either an inner or
           outer action has already been applied this generation
       * A dynamic action destined inside a model resetter requires a stabilization if
         that component has been reset this generation
       * A dynamic action destined anywhere after an [Incr.Var.t] has been set
     v}
  *)
  let requires_stabilization current_generation stats =
    let sub_requires_stabilization state ~from ~into:_ : _ Action.sub Action.t -> bool
      = function
      | Sub_from _ -> state
      | Sub_into _ -> state || from.generation = current_generation
    in
    let wrap_requires_stabilization state ~inner ~outer : _ Action.wrap Action.t -> bool
      = function
      | Wrap_outer _ ->
        state
        || outer.generation = current_generation
        || inner.generation = current_generation
      (* The [Wrap_inner] case intentionally does not check that the [inner] computation's
         generation. See above. *)
      | Wrap_inner _ -> state || outer.generation = current_generation
    in
    let model_reset_requires_stabilization state ~inner:_ ~outer
      : _ Action.model_resetter Action.t -> bool
      = function
      | Model_reset_inner _ -> state || outer.generation = current_generation
      | Model_reset_outer -> state
    in
    let don't_stabilize () =
      Stats.incr_don't_stabilize stats;
      false
    in
    let traverser =
      { unexplored =
          (fun state _empty action ->
            if is_dynamic_action action
            then should_stabilize_dynamic_action stats state
            else don't_stabilize ())
      ; dynamic_leaf = should_stabilize_dynamic_action stats
      ; static_leaf = (fun _ -> don't_stabilize ())
      ; sub = sub_requires_stabilization
      ; wrap = wrap_requires_stabilization
      ; model_reset = model_reset_requires_stabilization
      ; assoc = (fun state ~assoc:_ ~key:_ _ -> state)
      ; assoc_on = (fun state ~assoc_on:_ ~key:_ _ -> state)
      ; switch = (fun state ~switch:_ ~branch:_ _ -> state)
      ; lazy_ = (fun state _ _ -> state)
      }
    in
    traverse ~initial_state:false traverser
  ;;

  let insert current_generation action t =
    let insert_unexplored
      (type stripped)
      ()
      (empty : stripped t)
      (action : stripped Action.t)
      =
      empty.inner <- node_of_action current_generation action
    in
    let insert_sub () ~from ~into : _ Action.sub Action.t -> unit = function
      | Sub_from _ -> from.generation <- current_generation
      | Sub_into _ -> into.generation <- current_generation
    in
    let insert_wrap () ~inner ~outer : _ Action.wrap Action.t -> unit = function
      | Wrap_inner _ -> inner.generation <- current_generation
      | Wrap_outer _ -> outer.generation <- current_generation
    in
    let insert_model_reset () ~inner ~outer : _ Action.model_resetter Action.t -> unit
      = function
      | Model_reset_inner _ -> inner.generation <- current_generation
      | Model_reset_outer -> outer.generation <- current_generation
    in
    let iter_data m ~key ~f = Option.iter (Map.find m key) ~f in
    let traverser =
      { unexplored = insert_unexplored
      ; dynamic_leaf = (fun _ -> ())
      ; static_leaf = (fun _ -> ())
      ; sub = insert_sub
      ; wrap = insert_wrap
      ; model_reset = insert_model_reset
      ; lazy_ = (fun () inner _ -> inner.generation <- current_generation)
      ; assoc =
          (fun () ~assoc ~key _action ->
            iter_data assoc.by_key ~key ~f:(fun inner ->
              inner.generation <- current_generation))
      ; assoc_on =
          (fun () ~assoc_on ~key _action ->
            iter_data assoc_on.by_io_key ~key ~f:(fun inner ->
              inner.generation <- current_generation))
      ; switch =
          (fun () ~switch ~branch _action ->
            iter_data switch.by_branch ~key:branch ~f:(fun (T { inner; type_id = _ }) ->
              inner.generation <- current_generation))
      }
    in
    traverse ~initial_state:() traverser action t
  ;;

  type packed_filter = T : 'a t -> packed_filter [@@unboxed]

  let prune_stale_branches current_generation t ~on_prune =
    let rec loop : type a. a t -> unit =
      fun t ->
      let should_prune : type a. a t -> bool =
        fun { generation; _ } ->
        let should_prune = generation <= current_generation - num_generations_to_stale in
        if should_prune then on_prune ();
        should_prune
      in
      let filter_and_loop_via_fold m ~f =
        Map.fold m ~init:m ~f:(fun ~key ~data map ->
          let (T inner) = f data in
          match should_prune inner with
          | false ->
            loop inner;
            map
          | true -> Map.remove map key)
      in
      match t.inner with
      | Unexplored -> ()
      | Terminal -> ()
      | Sub { from; into } ->
        loop from;
        loop into
      | Wrap { inner; outer = _ } -> loop inner
      | Model_reset { inner; outer = _ } -> loop inner
      | Lazy (T { type_id = _; inner }) -> loop inner
      | Assoc_on assoc_on ->
        assoc_on.by_io_key
        <- filter_and_loop_via_fold assoc_on.by_io_key ~f:(fun inner -> T inner)
      | Assoc assoc ->
        assoc.by_key <- filter_and_loop_via_fold assoc.by_key ~f:(fun inner -> T inner)
      | Switch switch ->
        switch.by_branch
        <- filter_and_loop_via_fold switch.by_branch ~f:(fun (T { inner; type_id = _ }) ->
             T inner)
    in
    loop t
  ;;
end

module Incremental_stats = struct
  (* [num_var_sets] is a field in [Incremental.State.t] that informs us a side-effect took
     place requiring a stabilization. Technically, we could also require a stabilization
     from the use of [Incr.Expert.Node.create] and [Incr.mark_stale], however, proper
     usage shouldn't trigger a stabilization on its own. *)
  type t = { last_num_var_sets : int } [@@deriving equal]

  let current () =
    let state = Incr.State.t in
    { last_num_var_sets = Incr.State.num_var_sets state }
  ;;
end

type 'a t =
  { trie : 'a Action_trie.t
  ; stats : Stats.t
  ; mutable current_generation : Generation.t
  ; mutable last_incremental_stats : Incremental_stats.t
  ; mutable last_generation_pruned : Generation.t
  ; mutable am_debugging_test : bool
  }

let empty () =
  { trie = Action_trie.empty ()
  ; stats = Stats.create ()
  ; current_generation = Generation.initial
  ; last_incremental_stats = Incremental_stats.current ()
  ; last_generation_pruned = Generation.initial
  ; am_debugging_test = false
  }
;;

let mark_stabilization t =
  mark_incremental_clean ();
  t.current_generation <- Generation.next t.current_generation;
  t.last_incremental_stats <- Incremental_stats.current ()
;;

(* [prune_trie] ensures that we don't leak inactive parts of the Bonsai graph. We discard
   any switch and assoc branches that haven't receieved an action in a long time, since we
   assume them to be inactive. *)
let prune_trie t =
  if t.current_generation
     >= t.last_generation_pruned + Action_trie.num_generations_to_stale
  then (
    Stats.incr_prunes_run t.stats;
    Action_trie.prune_stale_branches t.current_generation t.trie ~on_prune:(fun () ->
      Stats.incr_branches_pruned t.stats);
    t.last_generation_pruned <- t.current_generation)
;;

let check_incremental_stats_and_mark_dirty t =
  if not (Incremental_stats.equal (Incremental_stats.current ()) t.last_incremental_stats)
  then mark_incremental_dirty ()
;;

let requires_stabilization t action =
  check_incremental_stats_and_mark_dirty t;
  let requires_stabilization =
    Action_trie.requires_stabilization t.current_generation t.stats action t.trie
  in
  if t.am_debugging_test
  then
    if requires_stabilization
    then print_endline "stabilized"
    else print_endline "skipped stabilization";
  requires_stabilization
;;

let insert t action =
  Action_trie.insert t.current_generation action t.trie;
  prune_trie t
;;

module For_testing = struct
  module Stats = Stats

  let start_debugging t = t.am_debugging_test <- true
  let num_generations_for_pruning = Action_trie.num_generations_to_stale
  let display_stats t = Stats.display t.stats
end
