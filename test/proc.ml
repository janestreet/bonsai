open! Core
open Bonsai.For_open
open! Import

module Result_spec = struct
  module type S = sig
    type t
    type incoming

    val view : t -> string
    val incoming : t -> incoming -> unit Effect.t
  end

  type ('result, 'incoming) t =
    (module S with type t = 'result and type incoming = 'incoming)

  module No_incoming = struct
    type incoming = Nothing.t

    let incoming _t incoming = Nothing.unreachable_code incoming
  end

  module type Sexpable = sig
    type t [@@deriving sexp_of]
  end

  module type Stringable = sig
    type t

    val to_string : t -> string
  end

  let invisible (type a) : (a, Nothing.t) t =
    (module struct
      type t = a

      include No_incoming

      let view _ = ""
    end)
  ;;

  let sexp (type a) (module S : Sexpable with type t = a) =
    (module struct
      type t = a

      include No_incoming

      let view s = s |> S.sexp_of_t |> Sexp.to_string_hum
    end : S
      with type t = a
       and type incoming = Nothing.t)
  ;;

  let string (type a) (module S : Stringable with type t = a) =
    (module struct
      type t = a

      include No_incoming

      let view s = s |> S.to_string
    end : S
      with type t = a
       and type incoming = Nothing.t)
  ;;
end

module Handle = struct
  type ('result, 'incoming) t =
    (unit, 'result * string * ('incoming -> unit Effect.t)) Driver.t

  let create
    (type result incoming)
    ?(start_time = Time_ns.epoch)
    ~optimize
    (result_spec : (result, incoming) Result_spec.t)
    computation
    =
    let (module R) = result_spec in
    let component (_ : unit Value.t) =
      let open Bonsai.Let_syntax in
      let%sub result = computation in
      return
        (let%map result = result in
         result, R.view result, R.incoming result)
    in
    let clock = Bonsai.Time_source.create ~start:start_time in
    Driver.create ~optimize ~initial_input:() ~clock component
  ;;

  let node_paths_from_skeleton
    : type a. a Bonsai.Private.Computation.t -> Bonsai.Private.Node_path.Set.t
    =
    fun t ->
    let find_node_paths =
      object
        inherit
          [Bonsai.Private.Node_path.Set.t] Bonsai.Private.Skeleton.Traverse.fold as super

        method! value value acc =
          let acc = Set.add acc (Lazy.force value.node_path) in
          super#value value acc

        method! computation computation acc =
          let acc = Set.add acc (Lazy.force computation.node_path) in
          super#computation computation acc
      end
    in
    find_node_paths#computation
      (Bonsai.Private.Skeleton.Computation.of_computation t)
      Bonsai.Private.Node_path.Set.empty
  ;;

  let node_paths_from_transform
    : type a. a Bonsai.Private.Computation.t -> Bonsai.Private.Node_path.Set.t
    =
    fun t ->
    let node_paths = ref Bonsai.Private.Node_path.Set.empty in
    let computation_map
      (type result)
      (context : _ Bonsai.Private.Transform.For_computation.context)
      state
      (computation : result Bonsai.Private.Computation.t)
      =
      node_paths := Set.add !node_paths (Lazy.force context.current_path);
      let out = context.recurse state computation in
      out
    in
    let value_map
      (type a)
      (context : _ Bonsai.Private.Transform.For_value.context)
      state
      (wrapped_value : a Bonsai.Private.Value.t)
      =
      node_paths := Set.add !node_paths (Lazy.force context.current_path);
      context.recurse state wrapped_value
    in
    let (_ : _ Bonsai.Private.Computation.t) =
      Bonsai.Private.Transform.map
        ~init:()
        ~computation_mapper:{ f = computation_map }
        ~value_mapper:{ f = value_map }
        t
    in
    !node_paths
  ;;

  let assert_node_paths_identical_between_transform_and_skeleton_nodepaths
    : type a. a Bonsai.Private.Computation.t -> unit
    =
    fun computation ->
    let from_transform = node_paths_from_transform computation in
    let from_skeleton = node_paths_from_skeleton computation in
    if not ([%equal: Set.M(Bonsai.Private.Node_path).t] from_transform from_skeleton)
    then (
      Expect_test_helpers_core.print_cr
        [%here]
        (Sexp.Atom "BUG IN BONSAI! Node Path Mismatch");
      Expect_test_patdiff.print_patdiff_s
        ([%sexp_of: Bonsai.Private.Node_path.Set.t] from_transform)
        ([%sexp_of: Bonsai.Private.Node_path.Set.t] from_skeleton))
  ;;

  let create
    (type result incoming)
    ?start_time
    ?(optimize = true)
    (result_spec : (result, incoming) Result_spec.t)
    computation
    =
    (* [assert_node_paths_identical_between_transform_and_skeleton_nodepaths] is a useful 
       function to verify that the skeleton code correctly generates node_path identifiers.
       It's nice to run on every test, but was taking up ~20% of the run time for most 
       tests, so we disable it here, keeping the code around in case there's any major 
       changes to the skeleton code and want to easily run it on every test in the tree *)
    if false
    then
      assert_node_paths_identical_between_transform_and_skeleton_nodepaths
        (Bonsai.Private.reveal_computation computation);
    create ?start_time ~optimize result_spec computation
  ;;

  let last_result handle =
    let result, _, _ = Driver.result handle in
    result
  ;;

  let clock = Driver.clock
  let advance_clock_by t = Bonsai.Time_source.advance_clock_by (Driver.clock t)
  let advance_clock ~to_ t = Bonsai.Time_source.advance_clock ~to_ (Driver.clock t)

  let do_actions handle actions =
    let _, _, inject_action = Driver.result handle in
    let event = actions |> List.map ~f:inject_action |> Effect.sequence in
    Driver.schedule_event handle event
  ;;

  let recompute_view (handle : (_, 'r) Driver.t) =
    Driver.flush handle;
    let (_ : 'r) = Driver.result handle in
    Driver.trigger_lifecycles handle
  ;;

  let recompute_view_until_stable ?(max_computes = 100) handle =
    with_return (fun { return } ->
      for _ = 1 to max_computes do
        recompute_view handle;
        if not (Driver.has_after_display_events handle) then return ()
      done;
      failwithf "view not stable after %d recomputations" max_computes ())
  ;;

  let generic_show handle ~before ~f =
    let before = before handle in
    Driver.flush handle;
    let _, view, _ = Driver.result handle in
    Driver.store_view handle view;
    f before view;
    Driver.trigger_lifecycles handle
  ;;

  let show handle =
    generic_show handle ~before:(Fn.const ()) ~f:(fun () view -> print_endline view)
  ;;

  let show_diff
    ?(location_style = Patdiff_kernel.Format.Location_style.None)
    ?(diff_context = 16)
    handle
    =
    generic_show
      handle
      ~before:Driver.last_view
      ~f:(Expect_test_patdiff.print_patdiff ~location_style ~context:diff_context)
  ;;

  let store_view handle = generic_show handle ~before:(Fn.const ()) ~f:(fun () _ -> ())

  let show_model handle =
    Driver.flush handle;
    Driver.sexp_of_model handle |> print_s
  ;;

  let result_incr handle =
    let%pattern_bind.Incr result, _view, _inject = Driver.result_incr handle in
    result
  ;;

  let action_input_incr = Driver.action_input_incr
  let lifecycle_incr = Driver.lifecycle_incr
  let has_after_display_events = Driver.has_after_display_events
end
