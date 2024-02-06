open! Core
open! Import
open Bonsai.For_open
open Bonsai.Let_syntax
open Bonsai_test

let no_op_sm0 =
  Bonsai.state_machine0 ~apply_action:(fun _context () () -> ()) ~default_model:() ()
;;

let no_op_sm1 =
  Bonsai.state_machine1
    (opaque_const_value ())
    ~apply_action:(fun _context _input () () -> ())
    ~default_model:()
;;

let copying_sm1 input =
  Bonsai.state_machine1
    ~default_model:0
    ~apply_action:(fun _context input _ () ->
      match input with
      | Bonsai.Computation_status.Inactive -> failwith "impossible"
      | Bonsai.Computation_status.Active input -> input)
    input
;;

let%expect_test "Sub/Leaf1/Leaf0" =
  let component =
    let%sub _, inject1 = no_op_sm0 in
    let%sub _, inject2 = no_op_sm1 in
    let%sub _, inject3 = no_op_sm0 in
    let%sub _, inject4 = no_op_sm1 in
    let%arr inject1 = inject1
    and inject2 = inject2
    and inject3 = inject3
    and inject4 = inject4 in
    inject1 (), inject2 (), inject3 (), inject4 ()
  in
  let module Action = struct
    type t =
      | One
      | Two
      | Three
      | Four
    [@@deriving sexp_of]
  end
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t * unit Effect.t * unit Effect.t * unit Effect.t
        type incoming = Action.t

        let view _ = ""

        let incoming (i1, i2, i3, i4) = function
          | Action.One -> i1
          | Two -> i2
          | Three -> i3
          | Four -> i4
        ;;
      end)
      component
  in
  Handle.print_actions handle;
  Handle.print_stabilizations handle;
  Handle.do_actions handle [ One; Two; Three; Four ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action" (action (Sub_from (Leaf_static <opaque>))))
    stabilized
    ("Processed action" (action (Sub_into (Sub_from (Leaf_dynamic <opaque>)))))
    skipped stabilization
    ("Processed action"
     (action (Sub_into (Sub_into (Sub_from (Leaf_static <opaque>))))))
    stabilized
    ("Processed action"
     (action (Sub_into (Sub_into (Sub_into (Leaf_dynamic <opaque>)))))) |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
    ((stabilizations_before_actions 2) (stabilizations_caused_by_var_changes 0)
     (stabilizations_skipped 2) (prunes_run 0) (branches_pruned 0)) |}]
;;

let%expect_test "Wrap/Model_resetter" =
  let component =
    Bonsai.wrap
      ~default_model:()
      ~apply_action:(fun _context _ () () -> ())
      ~f:(fun (_ : unit Value.t) inject_outer ->
        let%sub (_, inject), inject_reset = Bonsai.with_model_resetter no_op_sm1 in
        let%arr inject_outer = inject_outer
        and inject_reset = inject_reset
        and inject = inject in
        inject_outer (), inject_reset, inject ())
      ()
  in
  let module Action = struct
    type t =
      | Wrap_outer
      | Model_reset
      | Inject_dynamic
    [@@deriving sexp_of]
  end
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t * unit Effect.t * unit Effect.t
        type incoming = Action.t

        let view _ = ""

        let incoming (wrap_outer, model_reset, inject) = function
          | Action.Wrap_outer -> wrap_outer
          | Model_reset -> model_reset
          | Inject_dynamic -> inject
        ;;
      end)
      component
  in
  Handle.print_actions handle;
  Handle.print_stabilizations handle;
  (* wrap outer *)
  Handle.do_actions handle [ Wrap_outer ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action" (action (Wrap_outer <opaque>))) |}];
  (* model reset *)
  Handle.do_actions handle [ Model_reset ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action" (action (Wrap_inner Model_reset_outer))) |}];
  (* inner dynamic *)
  Handle.do_actions handle [ Inject_dynamic ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action"
     (action (Wrap_inner (Model_reset_inner (Leaf_dynamic <opaque>))))) |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
    ((stabilizations_before_actions 0) (stabilizations_caused_by_var_changes 0)
     (stabilizations_skipped 3) (prunes_run 0) (branches_pruned 0)) |}];
  (* inner then outer *)
  Handle.do_actions handle [ Inject_dynamic; Wrap_outer ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action"
     (action (Wrap_inner (Model_reset_inner (Leaf_dynamic <opaque>)))))
    stabilized
    ("Processed action" (action (Wrap_outer <opaque>))) |}];
  (* reset then outer *)
  Handle.do_actions handle [ Model_reset; Wrap_outer ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action" (action (Wrap_inner Model_reset_outer)))
    stabilized
    ("Processed action" (action (Wrap_outer <opaque>))) |}]
;;

let%expect_test "Switch/Lazy" =
  let lazy_branch_var = Bonsai.Var.create false in
  let lazy_branch = Bonsai.Var.value lazy_branch_var in
  let component =
    match%sub lazy_branch with
    | false ->
      let%sub _, inject = no_op_sm0 in
      let%arr inject = inject in
      inject ()
    | true ->
      (Bonsai.lazy_ [@alert "-deprecated"])
        (lazy
          (let%sub _, inject = no_op_sm0 in
           let%arr inject = inject in
           inject ()))
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t
        type incoming = unit

        let view _ = ""
        let incoming inject () = inject
      end)
      component
  in
  Handle.print_actions handle;
  Handle.print_stabilizations handle;
  (* In this case, we should go through the first switch branch and not hit the lazy
     case *)
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action" (action (Switch 0 (Leaf_static <opaque>)))) |}];
  Bonsai.Var.set lazy_branch_var true;
  Handle.recompute_view_until_stable handle;
  (* And alternatively, in this case, we should go through the second branch and hit
     the lazy case *)
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action" (action (Switch 1 (Lazy (Leaf_static <opaque>))))) |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
    ((stabilizations_before_actions 0) (stabilizations_caused_by_var_changes 0)
     (stabilizations_skipped 2) (prunes_run 0) (branches_pruned 0)) |}]
;;

let%expect_test "Assoc" =
  let input = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
  let component =
    Bonsai.assoc
      (module Int)
      (Bonsai.Var.value input)
      ~f:(fun _ _ ->
        let%sub _, inject = no_op_sm0 in
        let%arr inject = inject in
        inject ())
  in
  let module Action = struct
    type t = Entry of int [@@deriving sexp_of]
  end
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t Int.Map.t
        type incoming = Action.t

        let view _ = ""
        let incoming m (Action.Entry i) = Map.find_exn m i
      end)
      component
  in
  Handle.print_actions handle;
  Handle.print_stabilizations handle;
  Handle.do_actions handle [ Entry 1; Entry 2 ];
  Handle.show handle;
  [%expect
    {|
    skipped stabilization
    ("Processed action" (action (Assoc 1 (Leaf_static <opaque>))))
    skipped stabilization
    ("Processed action" (action (Assoc 2 (Leaf_static <opaque>)))) |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
    ((stabilizations_before_actions 0) (stabilizations_caused_by_var_changes 0)
     (stabilizations_skipped 2) (prunes_run 0) (branches_pruned 0)) |}]
;;

let%expect_test "Assoc_on" =
  let input = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
  let component =
    Bonsai.Expert.assoc_on
      (module Int)
      (module Unit)
      ~get_model_key:(fun _ _ -> ())
      ~f:(fun _ _ -> Bonsai.state 0)
      (Bonsai.Var.value input)
  in
  let module Action = struct
    type t =
      | Entry of
          { key : int
          ; set_to : int
          }
    [@@deriving sexp_of]
  end
  in
  let handle =
    Handle.create
      (module struct
        type t = (int * (int -> unit Effect.t)) Int.Map.t
        type incoming = Action.t

        let view m =
          Map.to_alist m
          |> List.map ~f:(fun (i, (state, _inject)) -> i, state)
          |> [%sexp_of: (int * int) list]
          |> Sexp.to_string_hum
        ;;

        let incoming m (Action.Entry { key; set_to }) =
          let _state, inject = Map.find_exn m key in
          inject set_to
        ;;
      end)
      component
  in
  Handle.print_actions handle;
  Handle.do_actions handle [ Entry { key = 1; set_to = 1 } ];
  Handle.show handle;
  [%expect
    {|
        ("Processed action" (action (Assoc_on 1 () (Leaf_static <opaque>))))
        ((1 1) (2 1)) |}];
  Handle.do_actions handle [ Entry { key = 2; set_to = 2 } ];
  Handle.show handle;
  [%expect
    {|
        ("Processed action" (action (Assoc_on 2 () (Leaf_static <opaque>))))
        ((1 2) (2 2)) |}]
;;

let%expect_test "Dynamic actions applied across frames don't need extra stabilizations \
                 due to var setting between frames"
  =
  let component =
    let%sub (), inject_first = no_op_sm1 in
    let%sub (), inject_second = no_op_sm1 in
    let%arr inject_first = inject_first
    and inject_second = inject_second in
    inject_first (), inject_second ()
  in
  let module Action = struct
    type t =
      | First
      | Second
  end
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t * unit Effect.t
        type incoming = Action.t

        let view _ = ""

        let incoming (first, second) = function
          | Action.First -> first
          | Second -> second
        ;;
      end)
      component
  in
  Handle.print_stabilizations handle;
  Handle.do_actions handle [ Second; First ];
  Handle.show handle;
  [%expect {|
    skipped stabilization
    skipped stabilization |}];
  Handle.do_actions handle [ Second ];
  Handle.show handle;
  [%expect {| skipped stabilization |}];
  (* Note: We don't need an additional stabilization here because stabilization happens
     between frames (i.e. as part of [Handle.show], after applying [Second] above). *)
  Handle.do_actions handle [ First ];
  Handle.show handle;
  [%expect {| skipped stabilization |}];
  Handle.show handle;
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
     ((stabilizations_before_actions 0) (stabilizations_caused_by_var_changes 0)
      (stabilizations_skipped 4) (prunes_run 0) (branches_pruned 0)) |}]
;;

let%expect_test "A static action that affects a dynamic action forces a restabilization" =
  let component =
    let%sub input, inject_static =
      Bonsai.state_machine0
        ~default_model:0
        ~apply_action:(fun _context model () -> model + 1)
        ()
    in
    let%sub output, inject_dynamic = copying_sm1 input in
    let%arr output = output
    and inject_static = inject_static
    and inject_dynamic = inject_dynamic in
    output, inject_static (), inject_dynamic ()
  in
  let module Action = struct
    type t =
      | Static
      | Dynamic
  end
  in
  let handle =
    Handle.create
      (module struct
        type t = int * unit Effect.t * unit Effect.t
        type incoming = Action.t

        let view (int, _, _) = [%string "Model: %{int#Int}"]

        let incoming (_, static, dynamic) = function
          | Action.Static -> static
          | Dynamic -> dynamic
        ;;
      end)
      component
  in
  Handle.print_actions handle;
  Handle.print_stabilizations handle;
  Handle.show handle;
  [%expect {| Model: 0 |}];
  (* Applying the static action should increase the model of the first state machine to 1,
     meaning that applying the dynamic action should change the output to 2. *)
  Handle.do_actions handle [ Static; Dynamic ];
  Handle.show handle;
  [%expect
    {|
     skipped stabilization
     ("Processed action" (action (Sub_from (Leaf_static <opaque>))))
     stabilized
     ("Processed action" (action (Sub_into (Leaf_dynamic <opaque>))))
     Model: 1 |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
     ((stabilizations_before_actions 1) (stabilizations_caused_by_var_changes 0)
      (stabilizations_skipped 1) (prunes_run 0) (branches_pruned 0)) |}]
;;

(* These tests verify interactions between mutable bits of incremental/bonsai, which can
   be tricky to get correct. In some cases, the code written is not idiomatic Bonsai (or,
   even idiomatic Bonsai test. This is alright, because we still want to handle people
   miusing/breaking into mutable APIs. *)
let%expect_test "state_machine1 depending on a Bonsai var behaves properly" =
  let var = Bonsai.Var.create 0 in
  let component = copying_sm1 (Bonsai.Var.value var) in
  let handle =
    Handle.create
      (module struct
        type t = int * (unit -> unit Effect.t)
        type incoming = unit

        let view (int, _) = [%string "Model: %{int#Int}"]
        let incoming (_, inject) () = inject ()
      end)
      component
  in
  Handle.print_stabilizations handle;
  Handle.show handle;
  [%expect {| Model: 0 |}];
  Bonsai.Var.set var 2;
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {|
     stabilized
     Model: 2 |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
     ((stabilizations_before_actions 1) (stabilizations_caused_by_var_changes 1)
      (stabilizations_skipped 0) (prunes_run 0) (branches_pruned 0)) |}]
;;

let%expect_test "state doesn't require stabilizing when a var is set" =
  let var = Bonsai.Var.create 0 in
  let component = Bonsai.state 0 in
  let handle =
    Handle.create
      (module struct
        type t = int * (int -> unit Effect.t)
        type incoming = int

        let view (int, _) = [%string "Model: %{int#Int}"]
        let incoming (_, inject) = inject
      end)
      component
  in
  Handle.print_stabilizations handle;
  Handle.show handle;
  [%expect {| Model: 0 |}];
  Bonsai.Var.set var 2;
  Handle.do_actions handle [ 5 ];
  Handle.show handle;
  [%expect {|
     skipped stabilization
     Model: 5 |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
     ((stabilizations_before_actions 0) (stabilizations_caused_by_var_changes 0)
      (stabilizations_skipped 1) (prunes_run 0) (branches_pruned 0)) |}]
;;

let%expect_test "state_machine1 depending on an Incr.compute with an Incr var behaves \
                 properly"
  =
  let incr_var = Incr.Var.create 0 in
  let component =
    let%sub value =
      Bonsai.Var.create ()
      |> Bonsai.Var.value
      |> Bonsai.Incr.compute ~f:(fun _ -> Incr.Var.watch incr_var)
    in
    copying_sm1 value
  in
  let handle =
    Handle.create
      (module struct
        type t = int * (unit -> unit Effect.t)
        type incoming = unit

        let view (int, _) = [%string "Model: %{int#Int}"]
        let incoming (_, inject) () = inject ()
      end)
      component
  in
  Handle.print_stabilizations handle;
  Handle.show handle;
  [%expect {| Model: 0 |}];
  Incr.Var.set incr_var 2;
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {|
     stabilized
     Model: 2 |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
     ((stabilizations_before_actions 1) (stabilizations_caused_by_var_changes 1)
      (stabilizations_skipped 0) (prunes_run 0) (branches_pruned 0)) |}]
;;

let%expect_test "state_machine1 that schedules an action which sets an upstream var \
                 behaves properly"
  =
  let incr_var = Incr.Var.create 0 in
  let component =
    let%sub value =
      Bonsai.Var.create ()
      |> Bonsai.Var.value
      |> Bonsai.Incr.compute ~f:(fun _ -> Incr.Var.watch incr_var)
    in
    let%sub state, inject = copying_sm1 value in
    let%sub (), inject_mutation =
      Bonsai.state_machine0
        ~default_model:()
        ~apply_action:(fun _context () value -> Incr.Var.set incr_var value)
        ()
    in
    let%arr state = state
    and inject = inject
    and inject_mutation = inject_mutation in
    state, inject, inject_mutation
  in
  let handle =
    Handle.create
      (module struct
        type t = int * (unit -> unit Effect.t) * (int -> unit Effect.t)

        type incoming =
          [ `Set_input of int
          | `Copy_input
          ]

        let view (int, _, _) = [%string "Model: %{int#Int}"]

        let incoming (_, inject_copy, inject_mutate) = function
          | `Copy_input -> inject_copy ()
          | `Set_input x -> inject_mutate x
        ;;
      end)
      component
  in
  Handle.print_stabilizations handle;
  Handle.show handle;
  [%expect {|
     Model: 0 |}];
  (* It handles when actions with side-effects are applied in the same frame *)
  Handle.do_actions handle [ `Set_input 1; `Copy_input ];
  Handle.show handle;
  [%expect {|
     skipped stabilization
     stabilized
     Model: 1 |}];
  (* It handles when actions are applied across multiple frames *)
  Handle.do_actions handle [ `Set_input 2 ];
  Handle.show handle;
  [%expect {|
     skipped stabilization
     Model: 1 |}];
  Handle.do_actions handle [ `Copy_input ];
  Handle.show handle;
  [%expect {|
     skipped stabilization
     Model: 2 |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
     ((stabilizations_before_actions 1) (stabilizations_caused_by_var_changes 1)
      (stabilizations_skipped 3) (prunes_run 0) (branches_pruned 0)) |}]
;;

(* This test demonstrates a case where we don't stabilize even though technically we
   should. This is an example of a misuse of the [Incr.Expert.Node] API where impurity is
   leaking to the user. While we could technically guard against this, users breaking
   Incremental's abstractions do so at their own risk. *)
let%expect_test "state_machine1 depending on Incr.Expert.Node.t that breaks abstraction \
                 doesn't stabilize when marked stale"
  =
  let counter = ref 0 in
  let expert_node =
    Incr.Expert.Node.create (fun () ->
      print_endline "Expert body fired";
      let value = !counter in
      incr counter;
      value)
  in
  let component =
    let%sub value =
      Bonsai.Var.create ()
      |> Bonsai.Var.value
      |> Bonsai.Incr.compute ~f:(fun _ -> Incr.Expert.Node.watch expert_node)
    in
    let%sub model, inject_copy = copying_sm1 value in
    let%sub (), inject_stale =
      Bonsai.state_machine0
        ~default_model:()
        ~apply_action:(fun _context () () -> Incr.Expert.Node.make_stale expert_node)
        ()
    in
    let%arr model = model
    and inject_copy = inject_copy
    and inject_stale = inject_stale in
    model, inject_copy, inject_stale
  in
  let handle =
    Handle.create
      (module struct
        type t = int * (unit -> unit Effect.t) * (unit -> unit Effect.t)

        type incoming =
          [ `Make_stale
          | `Copy_input
          ]

        let view (int, _, _) = [%string "Model: %{int#Int}"]

        let incoming (_, inject_copy, inject_stale) = function
          | `Make_stale -> inject_stale ()
          | `Copy_input -> inject_copy ()
        ;;
      end)
      component
  in
  Handle.print_stabilizations handle;
  Handle.show handle;
  [%expect {|
     Expert body fired
     Model: 0 |}];
  (* Actions with side-effects are applied in the same frame *)
  Handle.do_actions handle [ `Make_stale; `Copy_input ];
  Handle.show handle;
  [%expect
    {|
     skipped stabilization
     skipped stabilization
     Expert body fired
     Model: 0 |}];
  (* Actions are applied across multiple frames *)
  Handle.do_actions handle [ `Make_stale ];
  Handle.show handle;
  [%expect {|
     skipped stabilization
     Expert body fired
     Model: 0 |}];
  Handle.do_actions handle [ `Copy_input ];
  Handle.show handle;
  [%expect {|
     skipped stabilization
     Model: 2 |}];
  Handle.print_stabilization_tracker_stats handle;
  [%expect
    {|
     ((stabilizations_before_actions 0) (stabilizations_caused_by_var_changes 0)
      (stabilizations_skipped 4) (prunes_run 0) (branches_pruned 0)) |}]
;;

let%test_module "pruning" =
  (module struct
    let trigger_prune_with action =
      (* We do [2 * num_generations_for_pruning] because pruning only happens every
         [num_generations_for_testing] and requires the path to not have received an action
         for the last [num_generations_for_testing] stabilizations. *)
      List.init
        (2 * Bonsai.Private.Stabilization_tracker.For_testing.num_generations_for_pruning)
        ~f:(fun _ -> action)
    ;;

    let sm1_requiring_stabilization =
      let%sub _, inject_first = no_op_sm0 in
      let%sub _, inject_second = no_op_sm1 in
      let%arr inject_first = inject_first
      and inject_second = inject_second in
      Ui_effect.Many [ inject_first (); inject_second () ]
    ;;

    let run_assoc_test assoc_impl =
      let input = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
      let component =
        assoc_impl (Bonsai.Var.value input) ~f:(fun _ _ -> sm1_requiring_stabilization)
      in
      let module Action = struct
        type t = Entry of int [@@deriving sexp_of]
      end
      in
      let handle =
        Handle.create
          (module struct
            type t = unit Effect.t Int.Map.t
            type incoming = Action.t

            let view _ = ""
            let incoming m (Action.Entry i) = Map.find_exn m i
          end)
          component
      in
      Handle.do_actions handle [ Entry 2; Entry 1 ];
      Handle.recompute_view_until_stable handle;
      Handle.do_actions handle (trigger_prune_with (Action.Entry 1));
      Handle.recompute_view_until_stable handle;
      print_endline "After pruning:";
      Handle.print_stabilization_tracker_stats handle;
      (* Note: at this point, we have pruned the branch corresponding to the key [2] in the
         assoc. We can still deliver actions to this path again without issues. *)
      Handle.do_actions handle [ Entry 2 ];
      Handle.recompute_view_until_stable handle;
      print_endline "After pruning + action delivery:";
      Handle.print_stabilization_tracker_stats handle
    ;;

    let%expect_test "old assoc branches are pruned after not receiving actions for a \
                     long time"
      =
      run_assoc_test (Bonsai.assoc (module Int));
      [%expect
        {|
        After pruning:
        ((stabilizations_before_actions 5402)
         (stabilizations_caused_by_var_changes 0) (stabilizations_skipped 5402)
         (prunes_run 2) (branches_pruned 1))
        After pruning + action delivery:
        ((stabilizations_before_actions 5403)
         (stabilizations_caused_by_var_changes 0) (stabilizations_skipped 5403)
         (prunes_run 2) (branches_pruned 1)) |}]
    ;;

    let%expect_test "old assoc_on branches are pruned after not receiving actions for a \
                     long time"
      =
      run_assoc_test
        (Bonsai.Expert.assoc_on
           (module Int)
           (module Unit)
           ~get_model_key:(fun _ () -> ()));
      [%expect
        {|
        After pruning:
        ((stabilizations_before_actions 5402)
         (stabilizations_caused_by_var_changes 0) (stabilizations_skipped 5402)
         (prunes_run 2) (branches_pruned 1))
        After pruning + action delivery:
        ((stabilizations_before_actions 5403)
         (stabilizations_caused_by_var_changes 0) (stabilizations_skipped 5403)
         (prunes_run 2) (branches_pruned 1)) |}]
    ;;

    let%expect_test "old switch branches are pruned after not receiving actions for a \
                     long time"
      =
      let input = Bonsai.Var.create true in
      let component =
        match%sub Bonsai.Var.value input with
        | true -> sm1_requiring_stabilization
        | false -> sm1_requiring_stabilization
      in
      let handle =
        Handle.create
          (module struct
            type t = unit Effect.t
            type incoming = unit

            let view _ = ""
            let incoming e () = e
          end)
          component
      in
      Handle.do_actions handle [ () ];
      Handle.recompute_view_until_stable handle;
      Bonsai.Var.set input false;
      Handle.recompute_view_until_stable handle;
      Handle.do_actions handle (trigger_prune_with ());
      (* Handle.do_actions handle (trigger_prune_with ()); *)
      Handle.recompute_view_until_stable handle;
      print_endline "After pruning:";
      Handle.print_stabilization_tracker_stats handle;
      (* Note: at this point, we have pruned the branch corresponding to the key [2] in the
         assoc. We can still deliver actions to this path again without issues. *)
      Bonsai.Var.set input true;
      Handle.recompute_view_until_stable handle;
      Handle.do_actions handle [ () ];
      Handle.recompute_view_until_stable handle;
      print_endline "After pruning + action delivery:";
      Handle.print_stabilization_tracker_stats handle;
      [%expect
        {|
        After pruning:
        ((stabilizations_before_actions 5401)
         (stabilizations_caused_by_var_changes 0) (stabilizations_skipped 5401)
         (prunes_run 2) (branches_pruned 1))
        After pruning + action delivery:
        ((stabilizations_before_actions 5402)
         (stabilizations_caused_by_var_changes 0) (stabilizations_skipped 5402)
         (prunes_run 2) (branches_pruned 1)) |}]
    ;;
  end)
;;

let%test_module "the optimization takes effect" =
  (module struct
    let%expect_test "state_machine1 depending on state_machine1" =
      let component =
        let%sub _, inject1 = no_op_sm1 in
        let%sub _, inject2 = no_op_sm1 in
        let%arr inject1 = inject1
        and inject2 = inject2 in
        inject1 (), inject2 ()
      in
      let handle =
        Handle.create
          (module struct
            type t = unit Effect.t * unit Effect.t

            type incoming =
              [ `Upstream
              | `Downstream
              ]

            let view _ = ""

            let incoming (i1, i2) = function
              | `Upstream -> i1
              | `Downstream -> i2
            ;;
          end)
          component
      in
      Handle.print_actions handle;
      Handle.print_stabilizations handle;
      (* In this case, we do need a stabilization between actions *)
      Handle.do_actions handle [ `Upstream; `Downstream ];
      Handle.show handle;
      [%expect
        {|
        skipped stabilization
        ("Processed action" (action (Sub_from (Leaf_dynamic <opaque>))))
        stabilized
        ("Processed action" (action (Sub_into (Leaf_dynamic <opaque>)))) |}];
      (* Whereas in this case, we don't need a stabilization between actions *)
      Handle.do_actions handle [ `Downstream; `Upstream ];
      Handle.show handle;
      [%expect
        {|
        skipped stabilization
        ("Processed action" (action (Sub_into (Leaf_dynamic <opaque>))))
        skipped stabilization
        ("Processed action" (action (Sub_from (Leaf_dynamic <opaque>)))) |}];
      (* If we schedule two downstream actions, we don't need a stabilization between them
      *)
      Handle.do_actions handle [ `Downstream; `Downstream ];
      Handle.show handle;
      [%expect
        {|
        skipped stabilization
        ("Processed action" (action (Sub_into (Leaf_dynamic <opaque>))))
        skipped stabilization
        ("Processed action" (action (Sub_into (Leaf_dynamic <opaque>)))) |}];
      Handle.print_stabilization_tracker_stats handle;
      [%expect
        {|
        ((stabilizations_before_actions 1) (stabilizations_caused_by_var_changes 0)
         (stabilizations_skipped 5) (prunes_run 0) (branches_pruned 0)) |}]
    ;;

    let%expect_test "wrap that depends on a state_machine1" =
      let component =
        let%sub _, inject_sm1 = no_op_sm1 in
        let%sub inject_wrap_inner, inject_wrap_outer =
          Bonsai.wrap
            ~default_model:()
            ~apply_action:(fun _context _result _model _action -> ())
            ~f:(fun _model inject_outer ->
              let%sub _, inject_inner = no_op_sm1 in
              let%arr inject_inner = inject_inner
              and inject_outer = inject_outer in
              inject_inner, inject_outer)
            ()
        in
        let%arr inject_sm1 = inject_sm1
        and inject_wrap_inner = inject_wrap_inner
        and inject_wrap_outer = inject_wrap_outer in
        inject_sm1 (), inject_wrap_inner (), inject_wrap_outer ()
      in
      let handle =
        Handle.create
          (module struct
            type t = unit Effect.t * unit Effect.t * unit Effect.t

            type incoming =
              [ `Sm1
              | `Wrap_inner
              | `Wrap_outer
              ]

            let view _ = ""

            let incoming (sm1, inner, outer) = function
              | `Sm1 -> sm1
              | `Wrap_inner -> inner
              | `Wrap_outer -> outer
            ;;
          end)
          component
      in
      Handle.print_stabilizations handle;
      Handle.show handle;
      let actions = [ `Sm1; `Wrap_inner; `Wrap_outer ] in
      List.cartesian_product actions actions
      |> List.iter ~f:(fun (i, j) ->
           print_s
             [%message
               "Applying:"
                 ~_:(i : [ `Sm1 | `Wrap_inner | `Wrap_outer ])
                 ~_:(j : [ `Sm1 | `Wrap_inner | `Wrap_outer ])];
           Handle.do_actions handle [ i; j ];
           Handle.show handle);
      (* Here, we expect a test to stabilize before its second action if and only if
         the second action isn't [`Sm1]. *)
      [%expect
        {|
        (Applying: Sm1 Sm1)
        skipped stabilization
        skipped stabilization

        (Applying: Sm1 Wrap_inner)
        skipped stabilization
        stabilized

        (Applying: Sm1 Wrap_outer)
        skipped stabilization
        stabilized

        (Applying: Wrap_inner Sm1)
        skipped stabilization
        skipped stabilization

        (Applying: Wrap_inner Wrap_inner)
        skipped stabilization
        skipped stabilization

        (Applying: Wrap_inner Wrap_outer)
        skipped stabilization
        stabilized

        (Applying: Wrap_outer Sm1)
        skipped stabilization
        skipped stabilization

        (Applying: Wrap_outer Wrap_inner)
        skipped stabilization
        stabilized

        (Applying: Wrap_outer Wrap_outer)
        skipped stabilization
        stabilized |}];
      Handle.do_actions handle [ `Wrap_inner; `Wrap_inner; `Wrap_inner ];
      (* Chaining many of these wrap inners does not require stabilization *)
      Handle.show handle;
      [%expect
        {|
        skipped stabilization
        skipped stabilization
        skipped stabilization |}];
      Handle.do_actions handle [ `Wrap_outer; `Wrap_outer; `Wrap_outer ];
      (* Chaining many of these wrap outers together continues to require stabilization *)
      Handle.show handle;
      [%expect {|
        skipped stabilization
        stabilized
        stabilized |}]
    ;;
  end)
;;

let%test_module "interesting action delivery cases" =
  (module struct
    let%expect_test "dynamic action being delivered to a switch that requires \
                     stabilization"
      =
      let var = Bonsai.Var.create false in
      let component =
        let%sub input, set_input = Bonsai.state 0 in
        match%sub Bonsai.Var.value var with
        | false ->
          let%sub _state, set_state = Bonsai.state () in
          let%arr set_state = set_state
          and set_input = set_input in
          0, set_state, set_input
        | true ->
          let%sub input, copy_input = copying_sm1 input in
          let%arr input = input
          and copy_input = copy_input
          and set_input = set_input in
          input, copy_input, set_input
      in
      let handle =
        Handle.create
          (module struct
            type t = int * (unit -> unit Effect.t) * (int -> unit Effect.t)

            type incoming =
              [ `Set of int
              | `Copy
              ]

            let view (i, _, _) = Int.to_string i

            let incoming (_, copy, set) = function
              | `Set i -> set i
              | `Copy -> copy ()
            ;;
          end)
          component
      in
      (* We deliver an action to the true branch, and then switch to the false branch and
         perform both a set and a copy in the same [do_actions]. The copy should require a
         stabilization and correctly observe the set value. *)
      Handle.show handle;
      [%expect {| 0 |}];
      Handle.do_actions handle [ `Copy ];
      Bonsai.Var.set var true;
      Handle.recompute_view_until_stable handle;
      Handle.do_actions handle [ `Set 2; `Copy ];
      Handle.show handle;
      [%expect {| 2 |}]
    ;;

    let%expect_test "dynamic action being delivered to an assoc that requires \
                     stabilization"
      =
      let component =
        let%sub input, set_input = Bonsai.state 0 in
        let%sub m =
          Bonsai.assoc
            (module Int)
            (opaque_const_value (Int.Map.of_alist_exn [ 1, (); 2, () ]))
            ~f:(fun _key _data -> copying_sm1 input)
        in
        let%arr m = m
        and set_input = set_input in
        m, set_input
      in
      let handle =
        Handle.create
          (module struct
            type t = (int * (unit -> unit Effect.t)) Int.Map.t * (int -> unit Effect.t)

            type incoming =
              [ `Copy of int
              | `Set of int
              ]

            let view (m, _) =
              Map.to_alist m
              |> List.map ~f:(fun (key, (state, _set)) -> key, state)
              |> [%sexp_of: (int * int) list]
              |> Sexp.to_string_hum
            ;;

            let incoming (m, set) = function
              | `Copy i ->
                let _, copy = Map.find_exn m i in
                copy ()
              | `Set i -> set i
            ;;
          end)
          component
      in
      (* We deliver an action to the 1-key, and then another one to the 2-key, with sets
         prior to both. Both copies should notice the need for a stabilization to
         correctly observe the set value. *)
      Handle.show handle;
      [%expect {| ((1 0) (2 0)) |}];
      Handle.do_actions handle [ `Set 2; `Copy 1; `Set 3; `Copy 2 ];
      Handle.show handle;
      [%expect {| ((1 2) (2 3)) |}]
    ;;

    let%expect_test "dynamic action being delivered to an assoc that requires \
                     stabilization"
      =
      let component =
        let%sub input, set_input = Bonsai.state 0 in
        let%sub m =
          Bonsai.Expert.assoc_on
            (module Int)
            (module Unit)
            ~get_model_key:(fun _ _ -> ())
            (opaque_const_value (Int.Map.of_alist_exn [ 1, (); 2, () ]))
            ~f:(fun _key _data -> copying_sm1 input)
        in
        let%arr m = m
        and set_input = set_input in
        m, set_input
      in
      let handle =
        Handle.create
          (module struct
            type t = (int * (unit -> unit Effect.t)) Int.Map.t * (int -> unit Effect.t)

            type incoming =
              [ `Copy of int
              | `Set of int
              ]

            let view (m, _) =
              Map.to_alist m
              |> List.map ~f:(fun (key, (state, _set)) -> key, state)
              |> [%sexp_of: (int * int) list]
              |> Sexp.to_string_hum
            ;;

            let incoming (m, set) = function
              | `Copy i ->
                let _, copy = Map.find_exn m i in
                copy ()
              | `Set i -> set i
            ;;
          end)
          component
      in
      (* We deliver an action to the 1-key, and then another one to the 2-key, with sets
         prior to both. Both copies should notice the need for a stabilization to
         correctly observe the set value. *)
      Handle.show handle;
      [%expect {| ((1 0) (2 0)) |}];
      Handle.do_actions handle [ `Set 2; `Copy 1 ];
      Handle.show handle;
      [%expect {| ((1 2) (2 2)) |}];
      Handle.do_actions handle [ `Set 3; `Copy 2 ];
      Handle.show handle;
      [%expect {|
        ((1 3) (2 3)) |}]
    ;;

    (* This test case exercises the logic for a model reset outer action being followed by
       model reset inner action within the same frame. *)
    let%expect_test "a model reset containing a wrap" =
      let component =
        Bonsai.with_model_resetter
          (Bonsai.wrap
             ~default_model:0
             ~apply_action:(fun _context (result, _) _model -> function
               | `Set i -> i
               | `Copy -> result)
             ~f:(fun model inject_outer ->
               let%arr model = model
               and inject_outer = inject_outer in
               model, inject_outer)
             ())
      in
      let handle =
        Handle.create
          (module struct
            type t = (int * ([ `Set of int | `Copy ] -> unit Effect.t)) * unit Effect.t

            type incoming =
              [ `Set of int
              | `Copy
              | `Reset
              ]

            let view ((state, _), _) = Int.to_string state

            let incoming ((_, inject), reset) = function
              | `Set i -> inject (`Set i)
              | `Copy -> inject `Copy
              | `Reset -> reset
            ;;
          end)
          component
      in
      Handle.show handle;
      [%expect {| 0 |}];
      Handle.do_actions handle [ `Set 10 ];
      Handle.show handle;
      [%expect {| 10 |}];
      (* Resetting and copying in the same frame should copy the new value of 0, instead
         of the old value of 10. *)
      Handle.do_actions handle [ `Reset; `Copy ];
      Handle.show handle;
      [%expect {| 0 |}]
    ;;

    let%expect_test "consecutive wrap inner actions don't require stabilization, even if \
                     they call inject_outer"
      =
      (* This test demonstrates that successive [Wrap_inner] actions don't require
         stabiliztaion. The only way that [Wrap_inner] could influence its own inputs is
         by calling the supplied [inject_outer] input, which is what this test does. *)
      let component =
        Bonsai.wrap
          ~default_model:0
          ~apply_action:(fun _context (result, _) _model -> function
            | `Set i -> i
            | `Copy -> result)
          ~f:(fun model inject_outer ->
            let%sub (), inject_inner =
              Bonsai.state_machine1
                ~default_model:()
                ~apply_action:(fun context inject_outer _model action ->
                  match inject_outer with
                  | Inactive -> raise_s [%message "BUG"]
                  | Active inject_outer ->
                    let schedule_event =
                      Bonsai.Apply_action_context.schedule_event context
                    in
                    schedule_event (inject_outer action))
                inject_outer
            in
            let%arr model = model
            and inject_inner = inject_inner in
            model, inject_inner)
          ()
      in
      let handle =
        Handle.create
          (module struct
            type t = int * ([ `Set of int | `Copy ] -> unit Effect.t)

            type incoming =
              [ `Set of int
              | `Copy
              ]

            let view (state, _) = Int.to_string state

            let incoming (_, inject) = function
              | `Set i -> inject (`Set i)
              | `Copy -> inject `Copy
            ;;
          end)
          component
      in
      Handle.print_stabilizations handle;
      Handle.print_actions handle;
      Handle.show handle;
      [%expect {| 0 |}];
      (* Setting the model to 5 and then copying it in the same frame should cause the
         model to (correctly) update to 5 *)
      Handle.do_actions handle [ `Set 5; `Copy ];
      Handle.show handle;
      (* The actions below demonstrate why this action is safe in the first place: even
         though we call the outer injection function from the body of the wrap, that
         results in a [Wrap_outer] action being generated. We require stabilization
         between [Wrap_inner] and [Wrap_outer] events, so the data is all up-to-date. *)
      [%expect
        {|
        skipped stabilization
        ("Processed action" (action (Wrap_inner (Leaf_dynamic <opaque>))))
        skipped stabilization
        ("Processed action" (action (Wrap_inner (Leaf_dynamic <opaque>))))
        stabilized
        ("Processed action" (action (Wrap_outer <opaque>)))
        stabilized
        ("Processed action" (action (Wrap_outer <opaque>)))
        5 |}]
    ;;
  end)
;;
