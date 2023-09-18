open! Core
open! Import
open Bonsai.For_open
open Bonsai.Let_syntax
module Query_response_tracker = Bonsai.Effect.For_testing.Query_response_tracker
open Proc
module Action = Bonsai.Private.Action

let unreachable_action : Nothing.t Action.leaf Action.t -> 'b = function
  | Action.Leaf_dynamic _ -> .
  | Leaf_static _ -> .
;;

let sexp_of_packed_computation : type a. a Bonsai.Private.Computation.t -> Sexp.t =
  fun t ->
  Bonsai.Private.Skeleton.Computation.of_computation t
  |> Bonsai.Private.Skeleton.Computation.sanitize_for_testing
  |> Bonsai.Private.Skeleton.Computation.minimal_sexp_of_t
;;

let%expect_test "cutoff" =
  let var = Bonsai.Var.create 0 in
  let value = Bonsai.Var.value var in
  let component = return @@ Value.cutoff value ~equal:(fun a b -> a % 2 = b % 2) in
  let handle = Handle.create (Result_spec.string (module Int)) component in
  Handle.show handle;
  [%expect {| 0 |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {| 0 |}];
  Bonsai.Var.set var 1;
  Handle.show handle;
  [%expect {| 1 |}]
;;

let%expect_test "debug on change" =
  let var = Bonsai.Var.create 0 in
  let value = Bonsai.Var.value var in
  let component = Bonsai.Debug.on_change value ~f:(fun i -> printf "%d" i) in
  let handle = Handle.create Result_spec.invisible component in
  Handle.show handle;
  [%expect {| 0 |}];
  Bonsai.Var.set var 1;
  Handle.show handle;
  [%expect {| 1 |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {| 2 |}]
;;

let%expect_test "Setting cutoff on Bonsai values should not change previously set cutoffs"
  =
  let var = Bonsai.Var.create (0, 0) in
  let value = Bonsai.Var.value var in
  let component =
    let%sub pair = return (Value.cutoff ~equal:phys_equal value) in
    let%sub _ = return (Value.cutoff value ~equal:(fun (a1, _) (a2, _) -> a1 = a2)) in
    return pair
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int * int [@@deriving sexp]
         end))
      component
  in
  Handle.show handle;
  [%expect {| (0 0) |}];
  Bonsai.Var.set var (1, 0);
  Handle.show handle;
  [%expect {| (1 0) |}];
  Bonsai.Var.set var (1, 2);
  Handle.show handle;
  [%expect {| (1 2) |}]
;;

let%expect_test "Cutoff set by let%arr ppx should not be applied to different \
                 incremental nodes"
  =
  let var = Bonsai.Var.create (0, 0) in
  let value = Bonsai.Var.value var in
  let component =
    let%sub pair = return value in
    let%sub _ =
      let%arr a, _ = value in
      a
    in
    return pair
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int * int [@@deriving sexp]
         end))
      component
  in
  Handle.show handle;
  [%expect {| (0 0) |}];
  Bonsai.Var.set var (1, 0);
  Handle.show handle;
  [%expect {| (1 0) |}];
  Bonsai.Var.set var (1, 2);
  Handle.show handle;
  [%expect {| (1 2) |}]
;;

let%expect_test "Cutoff propragates on named values regression" =
  (* This test tests against a regression on [cutoff].

     Since named values are evaled into a map, and
     [Value.cutoff] compiled to the mutable [Incremental.set_cutoff], everytime
     that set_cutoff happens it affects all occurrences of the named value.

     This is tested for here by giving the same named value node different
     cutoff functions (one for the left element and another for the second element)
     and showcasing that each node is not affected by the other cutoff node.
  *)
  let var = Bonsai.Var.create (0, 0) in
  let value = Bonsai.Var.value var in
  let component =
    let%sub tupled_input = return value in
    let%sub left =
      let tupled_input =
        Value.cutoff tupled_input ~equal:(fun (old, _) (new_, _) -> phys_equal old new_)
      in
      let%arr left, _ = tupled_input in
      left
    in
    let%sub right =
      let tupled_input =
        Value.cutoff tupled_input ~equal:(fun (_, old) (_, new_) -> phys_equal old new_)
      in
      let%arr _, right = tupled_input in
      right
    in
    let%arr left = left
    and right = right in
    left, right
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int * int [@@deriving sexp]
         end))
      component
  in
  Handle.show handle;
  [%expect {| (0 0) |}];
  (* First element changes. *)
  Bonsai.Var.set var (1, 0);
  (* Missed trigger! *)
  Handle.show handle;
  [%expect {| (1 0) |}];
  Bonsai.Var.set var (1, 2);
  (* When the second element changes, this is fine since its cutoff function
     won.*)
  Handle.show handle;
  [%expect {| (1 2) |}]
;;

let%expect_test "What happens when cutoff nodes are nested?" =
  let var = Bonsai.Var.create (0, 0) in
  let value = Bonsai.Var.value var in
  let component =
    let first_cutoff = Value.cutoff value ~equal:(fun (_, a) (_, b) -> phys_equal a b) in
    let second_cutoff =
      Value.cutoff first_cutoff ~equal:(fun (a, _) (b, _) -> phys_equal a b)
    in
    return second_cutoff
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int * int [@@deriving sexp]
         end))
      component
  in
  Handle.show handle;
  [%expect {| (0 0) |}];
  (* First element changes. *)
  Bonsai.Var.set var (1, 0);
  Handle.show handle;
  (* Does not recompute! (first cutoff still says they're equal.) *)
  [%expect {| (0 0) |}];
  (* Second element changes. *)
  Bonsai.Var.set var (0, 2);
  (* Does not recompute! (second cutoff still says they're equal.) *)
  Handle.show handle;
  [%expect {| (0 0) |}];
  Bonsai.Var.set var (1, 2);
  (* Only once both cutoffs say that they're unequal, recomputation happens. *)
  Handle.show handle;
  [%expect {| (1 2) |}]
;;

let%expect_test "arrow-syntax" =
  let component =
    let%sub a = Bonsai.const "hi" in
    let%sub b = Bonsai.const 5 in
    let%arr a = a
    and b = b in
    sprintf "%s %d" a b
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hi 5 |}]
;;

let%expect_test "mapn" =
  let (_ : unit Computation.t) =
    let%mapn.Computation () = Bonsai.const ()
    and () = Bonsai.const ()
    and () = Bonsai.const () in
    ()
  in
  ()
;;

let%expect_test "if%sub" =
  let component input =
    let a = Value.return "hello" in
    let b = Value.return "world" in
    if%sub input then Bonsai.read a else Bonsai.read b
  in
  let var = Bonsai.Var.create true in
  let handle =
    Handle.create (Result_spec.string (module String)) (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {| hello |}];
  Bonsai.Var.set var false;
  Handle.show handle;
  [%expect {| world |}]
;;

let%expect_test "call component" =
  let add_one = Bonsai.pure (fun x -> x + 1) in
  let component input =
    let%sub a = add_one input in
    return a
  in
  let var = Bonsai.Var.create 1 in
  let handle =
    Handle.create (Result_spec.sexp (module Int)) (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {| 2 |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {| 3 |}]
;;

let%expect_test "store named in a ref" =
  let name_ref = ref None in
  let component =
    let%sub x =
      let%sub a = opaque_const 5 in
      name_ref := Some a;
      let%arr a = a in
      a
    in
    let%arr x = x
    and y = Option.value_exn !name_ref in
    x + y
  in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    Handle.create (Result_spec.sexp (module Int)) component);
  [%expect
    {| "A Value.t introduced by the [let%sub] expression at TEST_FILENAME:0:0 was used outside of the scope that it was declared in. Make sure that you aren't storing it inside a ref." |}]
;;

let%expect_test "on_display" =
  let component =
    let%sub state, set_state =
      Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
    in
    let update =
      let%map state = state
      and set_state = set_state in
      set_state (state + 1)
    in
    let%sub () = Bonsai.Edge.after_display update in
    return state
  in
  let handle = Handle.create (Result_spec.sexp (module Int)) component in
  Handle.show handle;
  [%expect {| 0 |}];
  Handle.show handle;
  [%expect {| 1 |}];
  Handle.show handle;
  [%expect {| 2 |}];
  Handle.show handle;
  [%expect {| 3 |}]
;;

let%expect_test "on_display for updating a state" =
  let component input =
    let%sub state, set_state =
      Bonsai.state_opt () ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
    in
    let%sub update =
      match%sub state with
      | None ->
        return
        @@
        let%map set_state = set_state
        and input = input in
        Some (set_state (Some input))
      | Some state ->
        return
        @@
        let%map state = state
        and set_state = set_state
        and input = input in
        if Int.equal state input then None else Some (set_state (Some input))
    in
    let%sub () = Bonsai.Edge.after_display' update in
    return (Value.both input state)
  in
  let var = Bonsai.Var.create 1 in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int * int option [@@deriving sexp_of]
         end))
      (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {| (1 ()) |}];
  Handle.show handle;
  [%expect {| (1 (1)) |}];
  Handle.show handle;
  [%expect {| (1 (1)) |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {| (2 (1)) |}];
  Handle.show handle;
  [%expect {| (2 (2)) |}];
  Handle.show handle;
  [%expect {| (2 (2)) |}]
;;

let%expect_test "path" =
  let component =
    let%sub () = opaque_const () in
    let%sub path = Bonsai.Private.path in
    return (Value.map path ~f:Bonsai.Private.Path.sexp_of_t)
  in
  let handle = Handle.create (Result_spec.sexp (module Sexp)) component in
  Handle.show handle;
  (* The first of these "Subst_from" is actually a component that is
     added by the testing helpers. *)
  [%expect {| (Subst_from Subst_into Subst_into Subst_from) |}]
;;

let%expect_test "assoc and enum path" =
  let component =
    Bonsai.assoc
      (module Int)
      (opaque_const_value (Int.Map.of_alist_exn [ -1, (); 1, () ]))
      ~f:(fun i _ ->
        if%sub i >>| ( > ) 0 then Bonsai.Private.path else Bonsai.Private.path)
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = Bonsai.Private.Path.t Int.Map.t [@@deriving sexp_of]
         end))
      component
  in
  Handle.show handle;
  [%expect
    {|
    ((-1 (Subst_from (Assoc -1) Subst_into (Switch 0)))
     (1 (Subst_from (Assoc 1) Subst_into (Switch 1)))) |}]
;;

let%expect_test "constant folded assoc path" =
  let component =
    Bonsai.assoc
      (module Int)
      (Value.return (Int.Map.of_alist_exn [ -1, (); 1, () ]))
      ~f:(fun _ _ ->
        (* NOTE: Since this test case uses both a constant map and previously
           only made use of the path, then this combination resulted in the optimization
           that makes a call to Map.mapi directly to trigger. To avoid this, we
           artifically introduce some state, and more importantly, use the state trivially
           such that the simplication optimization is not triggered. *)
        let%sub x, _ =
          Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
        in
        let%sub path = Bonsai.Private.path in
        let%sub path, _ =
          let%arr path = path
          and x = x in
          path, x
        in
        Bonsai.read path)
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = Bonsai.Private.Path.t Int.Map.t [@@deriving sexp_of]
         end))
      component
  in
  Handle.show handle;
  [%expect
    {|
    ((-1
      (Subst_from Subst_from Subst_from Subst_from Subst_into Subst_into
       Subst_from))
     (1
      (Subst_from Subst_from Subst_into Subst_from Subst_from Subst_into
       Subst_into Subst_from))) |}]
;;

let%expect_test "constant folded assoc lifecycles are unchanged" =
  let runtest input =
    let component =
      let%sub _ =
        Bonsai.assoc
          (module Int)
          (input (Int.Map.of_alist_exn [ -1, (); 1, () ]))
          ~f:(fun key _ ->
            Bonsai.Edge.lifecycle
              ~on_activate:
                (let%map key = key in
                 Effect.print_s [%message (key : int)])
              ())
      in
      Bonsai.const ()
    in
    let handle = Handle.create (Result_spec.sexp (module Unit)) component in
    Handle.show handle
  in
  runtest opaque_const_value;
  let unoptimized = Expect_test_helpers_base.expect_test_output [%here] in
  runtest Value.return;
  let optimized = Expect_test_helpers_base.expect_test_output [%here] in
  print_endline (Expect_test_patdiff.patdiff ~context:0 unoptimized optimized)
;;

let%expect_test "constant map + simplifiable assoc ~f => constant map proper evaluation" =
  (* This test case just tests that the map function is applied properly. *)
  let component =
    Bonsai.assoc
      (module String)
      (Value.return (String.Map.of_alist_exn [ "hello", 0; "world", 5 ]))
      ~f:(fun _ v ->
        let%arr v = v in
        v + 100)
  in
  let module Model = struct
    type t = int String.Map.t [@@deriving sexp, equal]
  end
  in
  let handle = Handle.create (Result_spec.sexp (module Model)) component in
  Handle.show handle;
  [%expect {| ((hello 100) (world 105)) |}]
;;

let%expect_test "assoc_on" =
  let var = Bonsai.Var.create (Int.Map.of_alist_exn [ 0, (); 1, (); 2, () ]) in
  let component =
    Bonsai.Expert.assoc_on
      (module Int)
      (module Int)
      (Bonsai.Var.value var)
      ~get_model_key:(fun key _data -> key % 2)
      ~f:(fun _key _data ->
        Bonsai.state_machine1
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~apply_action:
            (fun
              (_ : _ Bonsai.Apply_action_context.t) input model new_model ->
            match input with
            | Active () -> new_model
            | Inactive ->
              print_endline "inactive";
              model)
          (opaque_const_value ()))
  in
  let handle =
    Handle.create
      (module struct
        type t = (int * (int -> unit Effect.t)) Int.Map.t
        type incoming = Nothing.t

        let incoming _ = Nothing.unreachable_code

        let view (map : t) =
          map
          |> Map.to_alist
          |> List.map ~f:(fun (i, (s, _)) -> i, s)
          |> [%sexp_of: (int * int) list]
          |> Sexp.to_string_hum
        ;;
      end)
      component
  in
  Handle.show handle;
  [%expect {| ((0 0) (1 0) (2 0)) |}];
  let result = Handle.last_result handle in
  let set_two what =
    let _, set = Map.find_exn result 2 in
    Ui_effect.Expert.handle (set what)
  in
  set_two 3;
  Handle.show handle;
  [%expect {| ((0 3) (1 0) (2 3)) |}];
  Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, () ]);
  Handle.show handle;
  [%expect {| ((1 0)) |}];
  set_two 4;
  Handle.show handle;
  [%expect {|
    inactive
    ((1 0)) |}];
  Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, (); 2, () ]);
  Handle.show handle;
  [%expect {| ((1 0) (2 3)) |}]
;;

let%expect_test "simplify assoc_on" =
  let var = Bonsai.Var.create (Int.Map.of_alist_exn [ 0, (); 1, (); 2, () ]) in
  let component =
    Bonsai.Expert.assoc_on
      (module Int)
      (module Int)
      (Bonsai.Var.value var)
      ~get_model_key:(fun key _data -> key % 2)
      ~f:(fun _key data -> return data)
  in
  component
  |> Bonsai.Private.reveal_computation
  |> Bonsai.Private.pre_process
  |> sexp_of_packed_computation
  |> print_s;
  [%expect {| (Assoc_simpl (map Incr)) |}]
;;

let%expect_test "simple-assoc works with paths" =
  let component =
    Bonsai.assoc
      (module String)
      (opaque_const_value (String.Map.of_alist_exn [ "hello", (); "world", () ]))
      ~f:(fun _ _ ->
        let%sub a = Bonsai.Private.path in
        let%sub b = Bonsai.Private.path in
        return (Bonsai.Value.both a b))
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = (Bonsai.Private.Path.t * Bonsai.Private.Path.t) String.Map.t
           [@@deriving sexp_of]
         end))
      component
  in
  Handle.show handle;
  [%expect
    {|
    ((hello
      ((Subst_from (Assoc hello) Subst_from)
       (Subst_from (Assoc hello) Subst_into Subst_from)))
     (world
      ((Subst_from (Assoc world) Subst_from)
       (Subst_from (Assoc world) Subst_into Subst_from)))) |}];
  component
  |> Bonsai.Private.reveal_computation
  |> Bonsai.Private.pre_process
  |> sexp_of_packed_computation
  |> print_s;
  [%expect {|
    (Assoc_simpl (map Incr)) |}]
;;

let test_assoc_simpl_on_cutoff ~added_by_let_syntax =
  let cutoff value ~equal =
    Bonsai.Private.reveal_value value
    |> Bonsai.Private.Value.cutoff ~added_by_let_syntax ~equal
    |> Bonsai.Private.conceal_value
  in
  let component =
    Bonsai.assoc
      (module String)
      (opaque_const_value (String.Map.of_alist_exn [ "capy", (); "bara", () ]))
      ~f:(fun _ data -> return (cutoff ~equal:(fun _ _ -> true) data))
  in
  print_s
    Bonsai.Private.(
      sexp_of_packed_computation
        (Bonsai.Private.Pre_process.pre_process (reveal_computation component)))
;;

let%expect_test "assoc simplification behavior on cutoffs" =
  test_assoc_simpl_on_cutoff ~added_by_let_syntax:true;
  [%expect {|
    (Assoc_simpl (map Incr)) |}];
  test_assoc_simpl_on_cutoff ~added_by_let_syntax:false;
  [%expect
    {|
    (Assoc
      (map Incr)
      (key_id  (Test 1))
      (cmp_id  (Test 2))
      (data_id (Test 3))
      (by (
        Return (
          value (Cutoff (t (Named (uid (Test 3)))) (added_by_let_syntax false)))))) |}]
;;

let%expect_test "assoc_list unique" =
  let run input =
    let component =
      Bonsai.assoc_list
        (module Int)
        (opaque_const_value input)
        ~get_key:fst
        ~f:(fun _ kv_pair ->
          let%sub _, value = return kv_pair in
          return value)
    in
    let handle =
      Handle.create
        (Result_spec.sexp
           (module struct
             type t =
               [ `Ok of string list
               | `Duplicate_key of int
               ]
             [@@deriving sexp_of]
           end))
        component
    in
    Handle.show handle
  in
  run [ 2, "a"; 1, "b"; 3, "c"; 1, "d" ];
  [%expect {| (Duplicate_key 1) |}];
  run [ 2, "a"; 1, "c"; 3, "b" ];
  [%expect {| (Ok (a c b)) |}]
;;

let%expect_test "chain" =
  let add_one = Bonsai.pure (fun x -> x + 1) in
  let double = Bonsai.pure (fun x -> x * 2) in
  let component input =
    let%sub a = add_one input in
    let%sub b = double a in
    return b
  in
  let var = Bonsai.Var.create 1 in
  let handle =
    Handle.create (Result_spec.sexp (module Int)) (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {| 4 |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {| 6 |}]
;;

let%expect_test "chain + both" =
  let add_one = Bonsai.pure (fun x -> x + 1) in
  let double = Bonsai.pure (fun x -> x * 2) in
  let add = Bonsai.pure (fun (x, y) -> x + y) in
  let component input =
    let%sub a = add_one input in
    let%sub b = double a in
    let%sub c = add (Value.both a b) in
    return c
  in
  let var = Bonsai.Var.create 1 in
  let handle =
    Handle.create (Result_spec.sexp (module Int)) (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {| 6 |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {| 9 |}]
;;

let%expect_test "wrap" =
  let component =
    Bonsai.wrap
      ()
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) (result, _) model () ->
        String.length result + model)
      ~f:(fun model inject ->
        return
        @@
        let%map model = model
        and inject = inject in
        Int.to_string model, inject)
  in
  let handle =
    Handle.create
      (module struct
        type t = string * (unit -> unit Effect.t)
        type incoming = unit

        let view = Tuple2.get1
        let incoming (_, x) () = x ()
      end)
      component
  in
  Handle.show handle;
  [%expect {| 0 |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| 1 |}];
  Handle.do_actions handle [ (); (); (); (); (); (); (); (); (); () ];
  Handle.show handle;
  [%expect {| 12 |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| 14 |}]
;;

let%expect_test "match%sub" =
  let var : (string, int) Either.t Bonsai.Var.t =
    Bonsai.Var.create (Either.First "hello")
  in
  let component =
    match%sub Bonsai.Var.value var with
    | First s -> Bonsai.read (Value.map s ~f:(sprintf "%s world"))
    | Second i -> Bonsai.read (Value.map i ~f:Int.to_string)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hello world |}];
  Bonsai.Var.set var (Second 2);
  Handle.show handle;
  [%expect {| 2 |}]
;;

let%expect_test "match%sub" =
  let var : (string, int) Either.t Bonsai.Var.t =
    Bonsai.Var.create (Either.First "hello")
  in
  let component =
    match%sub Bonsai.Var.value var with
    | First s -> Bonsai.read (Value.map s ~f:(sprintf "%s world"))
    | Second i -> Bonsai.read (Value.map i ~f:Int.to_string)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hello world |}];
  Bonsai.Var.set var (Second 2);
  Handle.show handle;
  [%expect {| 2 |}]
;;

type thing =
  | Loading of string
  | Search_results of int

let%expect_test "match%sub repro" =
  let open Bonsai.Let_syntax in
  let component current_page =
    match%sub current_page with
    | Loading x ->
      Bonsai.read
        (let%map x = x in
         "loading " ^ x)
    | Search_results s ->
      Bonsai.read
        (let%map s = s in
         sprintf "search results %d" s)
  in
  let var = Bonsai.Var.create (Loading "hello") in
  let handle =
    Handle.create (Result_spec.string (module String)) (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {| loading hello |}];
  Bonsai.Var.set var (Search_results 5);
  Handle.show handle;
  [%expect {| search results 5 |}]
;;

let%expect_test "if%sub" =
  let component input =
    let a = Value.return "hello" in
    let b = Value.return "world" in
    if%sub input then Bonsai.read a else Bonsai.read b
  in
  let var = Bonsai.Var.create true in
  let handle =
    Handle.create (Result_spec.string (module String)) (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {| hello |}];
  Bonsai.Var.set var false;
  Handle.show handle;
  [%expect {| world |}]
;;

let%expect_test "match%sub defers exceptions until runtime" =
  let var = Bonsai.Var.create true in
  let component =
    match%sub Bonsai.Var.value var with
    | true -> Bonsai.const "yay!"
    | false -> assert false
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| yay! |}]
;;

let%expect_test "let%sub patterns" =
  let component =
    let%sub a, _b = Bonsai.const ("hello world", 5) in
    return a
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hello world |}]
;;

let%expect_test "sub constant folding optimization" =
  let component =
    let%sub a = Bonsai.const 5 in
    let%sub b = Bonsai.const 6 in
    return
      (let%map a = a
       and b = b in
       a + b)
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect
    {|
    (Sub
      (from (Return (value (Constant (id (Test 0))))))
      (via (Test 1))
      (into (
        Sub
        (from (Return (value (Constant (id (Test 2))))))
        (via (Test 3))
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid (Test 1)))
                      (Named (uid (Test 3))))))))))))))) |}];
  let component =
    component
    |> Bonsai.Private.reveal_computation
    |> Bonsai.Private.Constant_fold.constant_fold
    |> Bonsai.Private.conceal_computation
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect {| (Return (value (Constant (id (Test 0))))) |}];
  let component =
    component |> Bonsai.Private.reveal_computation |> Bonsai.Private.conceal_computation
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "let%map constant folding optimization" =
  let component =
    let%sub a =
      let%arr a = Bonsai.Value.return 5 in
      a + 1
    in
    let%sub b = Bonsai.const 6 in
    return
      (let%map a = a
       and b = b in
       a + b)
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect
    {|
    (Sub
      (from (Return (value (Mapn (inputs ((Constant (id (Test 0)))))))))
      (via (Test 2))
      (into (
        Sub
        (from (Return (value (Constant (id (Test 3))))))
        (via (Test 4))
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid (Test 2)))
                      (Named (uid (Test 4))))))))))))))) |}];
  let component =
    component
    |> Bonsai.Private.reveal_computation
    |> Bonsai.Private.Constant_fold.constant_fold
    |> Bonsai.Private.conceal_computation
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect {| (Return (value (Constant (id (Test 0))))) |}];
  let component =
    component |> Bonsai.Private.reveal_computation |> Bonsai.Private.conceal_computation
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "assoc simplifies its inner computation, if possible" =
  let value = opaque_const_value String.Map.empty in
  let component =
    Bonsai.assoc
      (module String)
      value
      ~f:(fun key data -> Bonsai.read (Value.both key data))
  in
  print_s
    Bonsai.Private.(
      sexp_of_packed_computation (pre_process (reveal_computation component)));
  [%expect {|
    (Assoc_simpl (map Incr)) |}]
;;

let%expect_test "assoc with sub simplifies its inner computation, if possible" =
  let value = opaque_const_value String.Map.empty in
  let component =
    Bonsai.assoc
      (module String)
      value
      ~f:(fun key data ->
        let%sub key = Bonsai.read key in
        Bonsai.read (Bonsai.Value.both key data))
  in
  print_s
    Bonsai.Private.(
      sexp_of_packed_computation (pre_process (reveal_computation component)));
  [%expect {|
    (Assoc_simpl (map Incr)) |}]
;;

let%expect_test "assoc with sub simplifies its inner computation, if possible" =
  let value = opaque_const_value String.Map.empty in
  let component =
    Bonsai.assoc
      (module String)
      value
      ~f:(fun key data ->
        let%sub key = Bonsai.read key in
        Bonsai.read (Bonsai.Value.both key data))
  in
  print_s
    Bonsai.Private.(
      sexp_of_packed_computation (pre_process (reveal_computation component)));
  [%expect {|
    (Assoc_simpl (map Incr)) |}]
;;

let%expect_test "map > lazy" =
  let open Bonsai.Let_syntax in
  let module M = struct
    type t =
      { label : string
      ; children : t Int.Map.t
      }
  end
  in
  let rec f ~t ~depth =
    let%sub { M.label; children } = return t in
    let%sub children =
      Bonsai.assoc
        (module Int)
        children
        ~f:(fun _ v ->
          let depth =
            let%map depth = depth in
            depth + 1
          in
          (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (f ~t:v ~depth)))
    in
    return
    @@
    let%map label = label
    and children = children
    and depth = depth in
    [%message label (depth : int) (children : Sexp.t Int.Map.t)]
  in
  let t_var = Bonsai.Var.create { M.label = "hi"; children = Int.Map.empty } in
  let t_value = Bonsai.Var.value t_var in
  let handle =
    Handle.create
      (Result_spec.sexp (module Sexp))
      (f ~t:t_value ~depth:(Bonsai.Value.return 0))
  in
  [%expect {| |}];
  Handle.show handle;
  [%expect {| (hi (depth 0) (children ())) |}];
  Bonsai.Var.set
    t_var
    { M.label = "hi"
    ; children = Int.Map.singleton 0 { M.label = "hello"; children = Int.Map.empty }
    };
  Handle.show handle;
  [%expect {| (hi (depth 0) (children ((0 (hello (depth 1) (children ())))))) |}]
;;

let%expect_test "map > fix2" =
  let open Bonsai.Let_syntax in
  let module M = struct
    type t =
      { label : string
      ; children : t Int.Map.t
      }
  end
  in
  let f ~t ~depth =
    Bonsai.fix2 t depth ~f:(fun ~recurse t depth ->
      let%sub { M.label; children } = return t in
      let%sub children =
        Bonsai.assoc
          (module Int)
          children
          ~f:(fun _ v ->
            let depth =
              let%map depth = depth in
              depth + 1
            in
            recurse v depth)
      in
      return
      @@
      let%map label = label
      and children = children
      and depth = depth in
      [%message label (depth : int) (children : Sexp.t Int.Map.t)])
  in
  let t_var = Bonsai.Var.create { M.label = "hi"; children = Int.Map.empty } in
  let t_value = Bonsai.Var.value t_var in
  let handle =
    Handle.create
      (Result_spec.sexp (module Sexp))
      (f ~t:t_value ~depth:(Bonsai.Value.return 0))
  in
  [%expect {| |}];
  Handle.show handle;
  [%expect {| (hi (depth 0) (children ())) |}];
  Bonsai.Var.set
    t_var
    { M.label = "hi"
    ; children = Int.Map.singleton 0 { M.label = "hello"; children = Int.Map.empty }
    };
  Handle.show handle;
  [%expect {| (hi (depth 0) (children ((0 (hello (depth 1) (children ())))))) |}]
;;

let%expect_test "Using fix to implement mutual recursion (collatz)" =
  let open Bonsai.Let_syntax in
  let step ~f state even odd =
    let%sub n, depth = return state in
    let%sub n =
      let%arr n = n in
      f n
    in
    let%sub is_even =
      let%arr n = n in
      n % 2 = 0
    in
    let%sub depth =
      let%arr depth = depth in
      depth + 1
    in
    let%sub state =
      let%arr n = n
      and depth = depth in
      n, depth
    in
    if%sub is_even then even state else odd state
  in
  let even odd state =
    Bonsai.fix state ~f:(fun ~recurse state -> step ~f:(fun n -> n / 2) state recurse odd)
  in
  let odd state =
    Bonsai.fix state ~f:(fun ~recurse state ->
      let%sub one =
        let%arr n, _ = state in
        n = 1
      in
      if%sub one
      then (
        let%arr _, depth = state in
        depth)
      else step ~f:(fun n -> (3 * n) + 1) state (even recurse) recurse)
  in
  let even = even odd in
  let collatz n =
    let%sub state =
      let%arr n = n in
      n, -1
    in
    step ~f:(fun x -> x) state even odd
  in
  let var = Bonsai.Var.create 5 in
  let value = Bonsai.Var.value var in
  let handle =
    Handle.create
      (module struct
        type t = int
        type incoming = Nothing.t

        let view = Int.to_string
        let incoming _ = Nothing.unreachable_code
      end)
      (collatz value)
  in
  Handle.show handle;
  (* 5 -> 16 -> 8 -> 4 -> 2 -> 1 *)
  [%expect {| 5 |}];
  Bonsai.Var.set var 6;
  Handle.show handle;
  (* 6 -> 3 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1 *)
  [%expect {| 8 |}]
;;

let%expect_test "dynamic action sent to non-existent assoc element" =
  let var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
  let component =
    Bonsai.assoc
      (module Int)
      (Bonsai.Var.value var)
      ~f:(fun _key _data ->
        Bonsai.state_machine1
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~apply_action:
            (fun
              (_ : _ Bonsai.Apply_action_context.t) input model new_model ->
            match input with
            | Active () -> new_model
            | Inactive ->
              print_endline "inactive";
              model)
          (opaque_const_value ()))
  in
  let handle =
    Handle.create
      (module struct
        type t = (int * (int -> unit Effect.t)) Int.Map.t
        type incoming = Nothing.t

        let incoming _ = Nothing.unreachable_code

        let view (map : t) =
          map
          |> Map.to_alist
          |> List.map ~f:(fun (i, (s, _)) -> i, s)
          |> [%sexp_of: (int * int) list]
          |> Sexp.to_string_hum
        ;;
      end)
      component
  in
  Handle.show handle;
  [%expect {| ((1 0) (2 0)) |}];
  let result = Handle.last_result handle in
  let set_two what =
    let _, set = Map.find_exn result 2 in
    Ui_effect.Expert.handle (set what)
  in
  set_two 3;
  Handle.show handle;
  [%expect {| ((1 0) (2 3)) |}];
  Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, () ]);
  Handle.show handle;
  [%expect {| ((1 0)) |}];
  set_two 4;
  Handle.show handle;
  [%expect {|
    inactive
    ((1 0)) |}];
  Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, (); 2, () ]);
  Handle.show handle;
  [%expect {| ((1 0) (2 3)) |}]
;;

let%test_module "inactive delivery" =
  (module struct
    let rec censor_sexp = function
      | Sexp.List l ->
        (match List.filter_map l ~f:censor_sexp with
         | [] -> None
         | [ x ] -> Some x
         | all -> Some (Sexp.List all))
      | Sexp.Atom s ->
        if String.is_prefix s ~prefix:"lib/bonsai" then None else Some (Atom s)
    ;;

    let print_computation computation =
      computation (Bonsai.Value.return ())
      |> Bonsai.Private.reveal_computation
      |> Bonsai.Private.pre_process
      |> sexp_of_packed_computation
      |> censor_sexp
      |> Option.value ~default:(Sexp.List [])
      |> print_s
    ;;

    let test_delivery_to_inactive_component computation =
      let run_test which_assoc =
        let var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
        let component =
          match which_assoc with
          | `Assoc ->
            let%sub i = Bonsai.const () in
            Bonsai.assoc
              (module Int)
              (Bonsai.Var.value var)
              ~f:(fun _key _data -> computation i)
          | `Assoc_on ->
            let%sub i = Bonsai.const () in
            Bonsai.Expert.assoc_on
              (module Int)
              (module String)
              (Bonsai.Var.value var)
              ~get_model_key:(fun key _data -> Int.to_string key)
              ~f:(fun _key _data -> computation i)
        in
        let handle =
          Handle.create
            (module struct
              type t = (int * (int -> unit Effect.t)) Int.Map.t
              type incoming = Nothing.t

              let incoming _ = Nothing.unreachable_code

              let view (map : t) =
                map
                |> Map.to_alist
                |> List.map ~f:(fun (i, (s, _)) -> i, s)
                |> [%sexp_of: (int * int) list]
                |> Sexp.to_string_hum
              ;;
            end)
            component
        in
        Handle.show handle;
        let result = Handle.last_result handle in
        let set_two what =
          let _, set = Map.find_exn result 2 in
          Ui_effect.Expert.handle (set what)
        in
        set_two 3;
        Handle.show handle;
        Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, () ]);
        Handle.show handle;
        set_two 4;
        Handle.show handle;
        Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, (); 2, () ]);
        Handle.show handle;
        Expect_test_helpers_base.expect_test_output [%here]
      in
      let assoc = run_test `Assoc in
      let assoc_on = run_test `Assoc_on in
      print_computation computation;
      print_endline assoc;
      print_endline "==== Diff between assoc and assoc_on: ====";
      print_endline (Expect_test_patdiff.patdiff ~context:0 assoc assoc_on)
    ;;

    let%expect_test "state_machine1 inactive-delivery" =
      (fun _ ->
        Bonsai.state_machine1
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~apply_action:
            (fun
              (_ : _ Bonsai.Apply_action_context.t) input _model new_model ->
            (match input with
             | Inactive -> print_endline "static action"
             | Active () -> print_endline "dynamic action");
            new_model)
          (Value.return ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        Leaf0
        ((1 0) (2 0))
        dynamic action
        ((1 0) (2 3))
        ((1 0))
        dynamic action
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "race inactive-delivery (but an active input)" =
      (fun input ->
        Bonsai.state_machine1
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~apply_action:
            (fun
              (_ : _ Bonsai.Apply_action_context.t) input _model new_model ->
            (match input with
             | Inactive -> print_endline "static action"
             | Active () -> print_endline "dynamic action");
            new_model)
          input)
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        Leaf0
        ((1 0) (2 0))
        dynamic action
        ((1 0) (2 3))
        ((1 0))
        dynamic action
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "dynamic action inactive-delivery" =
      (fun _ ->
        Bonsai.state_machine1
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~apply_action:
            (fun
              (_ : _ Bonsai.Apply_action_context.t) input model new_model ->
            match input with
            | Active () -> new_model
            | Inactive ->
              print_endline "inactive";
              model)
          (opaque_const_value ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Leaf1 (input Incr))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        inactive
        ((1 0))
        ((1 0) (2 3))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "actor1 inactive-delivery" =
      (fun _ ->
        Bonsai.actor1
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~recv:(fun ~schedule_event:_ input model new_model ->
            match input with
            | Active () -> new_model, ()
            | Inactive ->
              print_endline
                "action sent to actor1 has been received while the input was inactive.";
              model, ())
          (opaque_const_value ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Leaf1 (input Incr)))
          (via (Test 1))
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid (Test 1))))))))
            (via (Test 3))
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid (Test 1))))))))
              (via (Test 5))
              (into (
                Sub
                (from (Return (value (Mapn (inputs (Named (uid (Test 5))))))))
                (via (Test 7))
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          (Named (uid (Test 3)))
                          (Named (uid (Test 7))))))))))))))))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        action sent to actor1 has been received while the input was inactive.
        ((1 0))
        ((1 0) (2 3))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "actor0 inactive-delivery" =
      (fun _ ->
        Bonsai.actor0
          ()
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~recv:(fun ~schedule_event:_ _model new_model -> new_model, ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from Leaf0)
          (via (Test 0))
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid (Test 0))))))))
            (via (Test 2))
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid (Test 0))))))))
              (via (Test 4))
              (into (
                Sub
                (from (Return (value (Mapn (inputs (Named (uid (Test 4))))))))
                (via (Test 6))
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          (Named (uid (Test 2)))
                          (Named (uid (Test 6))))))))))))))))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "actor1 with constant input downgrades to actor0" =
      (fun _ ->
        Bonsai.actor1
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~recv:(fun ~schedule_event:_ input model new_model ->
            match input with
            | Active () -> new_model, ()
            | Inactive ->
              print_endline
                "action sent to actor1 has been received while the input was inactive.";
              model, ())
          (Value.return ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from Leaf0)
          (via (Test 0))
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid (Test 0))))))))
            (via (Test 2))
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid (Test 0))))))))
              (via (Test 4))
              (into (
                Sub
                (from (Return (value (Mapn (inputs (Named (uid (Test 4))))))))
                (via (Test 6))
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          (Named (uid (Test 2)))
                          (Named (uid (Test 6))))))))))))))))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static action inactive-delivery" =
      (fun _ -> Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t])
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        Leaf0
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static inside of a lazy" =
      (fun _ ->
        opaque_computation
          ((Bonsai.lazy_ [@alert "-deprecated"])
             (lazy
               (Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]))))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
          (Sub
            (from (Return (value Incr)))
            (via (Test 1))
            (into (
              Switch
              (match_ (Mapn (inputs (Named (uid (Test 1))))))
              (arms ((Lazy t) (Return (value Exception)))))))
          ((1 0) (2 0))
          ((1 0) (2 3))
          ((1 0))
          ((1 0))
          ((1 0) (2 4))

          ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static inside of a lazy (optimized away)" =
      (fun _ ->
        (Bonsai.lazy_ [@alert "-deprecated"])
          (lazy (Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t])))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        Leaf0
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static inside of a fix" =
      (fun _ ->
        opaque_computation
          (Bonsai.fix (opaque_const_value true) ~f:(fun ~recurse v ->
             match%sub v with
             | true -> recurse (opaque_const_value false)
             | false ->
               Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t])))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
          (Sub
            (from (Return (value Incr)))
            (via (Test 1))
            (into (
              Switch
              (match_ (Mapn (inputs (Named (uid (Test 1))))))
              (arms (
                (Sub
                  (from (Return (value Incr)))
                  (via (Test 4))
                  (into (
                    Switch
                    (match_ (Mapn (inputs (Named (uid (Test 4))))))
                    (arms ((Lazy t) Leaf0)))))
                (Return (value Exception)))))))
          ((1 0) (2 0))
          ((1 0) (2 3))
          ((1 0))
          ((1 0))
          ((1 0) (2 4))

          ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static inside of a fix (optimized away)" =
      (fun _ ->
        Bonsai.fix (Value.return true) ~f:(fun ~recurse v ->
          match%sub v with
          | true -> recurse (Value.return false)
          | false ->
            Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        Leaf0
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static inside of a wrap" =
      (fun _ ->
        Bonsai.wrap
          ()
          ~sexp_of_model:[%sexp_of: Unit.t]
          ~equal:[%equal: Unit.t]
          ~default_model:()
          ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) _ () () -> ())
          ~f:(fun _model _inject ->
            Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Wrap
          (model_id  (Test 0))
          (inject_id (Test 1))
          (inner Leaf0))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static inside of a match%sub" =
      (fun _ ->
        match%sub opaque_const_value () with
        | () -> Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t])
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Return (value Incr)))
          (via (Test 1))
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid (Test 1))))))))
            (via (Test 3))
            (into Leaf0))))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static inside of a with_model_resetter" =
      (fun _ ->
        let%sub r, _reset =
          Bonsai.with_model_resetter
            (Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t])
        in
        return r)
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (
            With_model_resetter
            (inner (
              Sub
              (from Leaf0)
              (via (Test 1))
              (into (
                Return (
                  value (
                    Mapn (
                      inputs (
                        (Named (uid (Test 1)))
                        (Named (uid (Test 0)))))))))))
            (reset_id (Test 0))))
          (via (Test 3))
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid (Test 3))))))))
            (via (Test 5))
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid (Test 3))))))))
              (via (Test 7))
              (into (Return (value (Named (uid (Test 5)))))))))))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "resetting while inactive" =
      let which_branch = Bonsai.Var.create true in
      let component =
        if%sub Bonsai.Var.value which_branch
        then
          Bonsai.with_model_resetter
            (Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t])
        else Bonsai.const ((-1, fun _ -> Effect.Ignore), Effect.Ignore)
      in
      let handle =
        Handle.create
          (module struct
            type t = (int * (int -> unit Effect.t)) * unit Effect.t
            type incoming = Nothing.t

            let incoming _ = Nothing.unreachable_code
            let view ((i, _), _) = Int.to_string i
          end)
          component
      in
      Handle.show handle;
      let (_, set_value), reset = Handle.last_result handle in
      let set_value i = Ui_effect.Expert.handle (set_value i) in
      let reset () = Ui_effect.Expert.handle reset in
      set_value 3;
      Handle.show handle;
      Bonsai.Var.set which_branch false;
      Handle.show handle;
      set_value 4;
      Handle.show handle;
      Bonsai.Var.set which_branch true;
      Handle.show handle;
      [%expect {|
        0
        3
        -1
        -1
        4 |}];
      Bonsai.Var.set which_branch false;
      Handle.show handle;
      [%expect {| -1 |}];
      reset ();
      Bonsai.Var.set which_branch true;
      Handle.show handle;
      [%expect {| 0 |}]
    ;;

    let%expect_test "resetting while inactive via the reset passed in" =
      let which_branch = Bonsai.Var.create true in
      let component =
        if%sub Bonsai.Var.value which_branch
        then
          Bonsai.with_model_resetter' (fun ~reset ->
            Bonsai.Computation.both
              (Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t])
              (return reset))
        else Bonsai.const ((-1, fun _ -> Effect.Ignore), Effect.Ignore)
      in
      let handle =
        Handle.create
          (module struct
            type t = (int * (int -> unit Effect.t)) * unit Effect.t
            type incoming = Nothing.t

            let incoming _ = Nothing.unreachable_code
            let view ((i, _), _) = Int.to_string i
          end)
          component
      in
      Handle.show handle;
      let (_, set_value), reset = Handle.last_result handle in
      let set_value i = Ui_effect.Expert.handle (set_value i) in
      let reset () = Ui_effect.Expert.handle reset in
      set_value 3;
      Handle.show handle;
      Bonsai.Var.set which_branch false;
      Handle.show handle;
      set_value 4;
      Handle.show handle;
      Bonsai.Var.set which_branch true;
      Handle.show handle;
      [%expect {|
        0
        3
        -1
        -1
        4 |}];
      Bonsai.Var.set which_branch false;
      Handle.show handle;
      [%expect {| -1 |}];
      reset ();
      Bonsai.Var.set which_branch true;
      Handle.show handle;
      [%expect {| 0 |}]
    ;;

    let%test_module "component reset" =
      (module struct
        type 'a action =
          | Action of 'a
          | Reset

        let build_handle (type result incoming) component ~sexp_of =
          Handle.create
            (module struct
              type t = (result * (incoming -> unit Effect.t)) * unit Effect.t
              type nonrec incoming = incoming action

              let incoming ((_result, do_action), reset) = function
                | Action action -> do_action action
                | Reset -> reset
              ;;

              let view ((result, _), _) = sexp_of result |> Sexp.to_string_hum
            end)
            (Bonsai.with_model_resetter component)
        ;;

        let%expect_test "custom reset" =
          let component =
            Bonsai.state
              0
              ~sexp_of_model:[%sexp_of: Int.t]
              ~equal:[%equal: Int.t]
              ~reset:(fun m -> m * 2)
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let set_value i = Handle.do_actions handle [ Action i ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          set_value 3;
          Handle.show handle;
          [%expect {| 3 |}];
          set_value 4;
          Handle.show handle;
          [%expect {| 4 |}];
          reset ();
          Handle.show handle;
          [%expect {| 8 |}];
          set_value 1;
          reset ();
          Handle.show handle;
          [%expect {| 2 |}];
          set_value 1;
          reset ();
          set_value 10;
          Handle.show handle;
          [%expect {| 10 |}]
        ;;

        let%expect_test "reset by bouncing back to an action (state_machine0)" =
          let component =
            Bonsai.state_machine0
              ()
              ~sexp_of_model:[%sexp_of: Int.t]
              ~equal:[%equal: Int.t]
              ~sexp_of_action:[%sexp_of: Bool.t]
              ~default_model:0
              ~apply_action:
                (fun
                  (_ : _ Bonsai.Apply_action_context.t) model is_increment ->
                if is_increment then model + 1 else 999)
              ~reset:(fun context model ->
                Bonsai.Apply_action_context.schedule_event
                  context
                  (Bonsai.Apply_action_context.inject context false);
                model)
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let increment () = Handle.do_actions handle [ Action true ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          increment ();
          increment ();
          Handle.show handle;
          [%expect {| 2 |}];
          increment ();
          Handle.show handle;
          [%expect {| 3 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "reset by bouncing back to an action (state_machine1)" =
          let component =
            Bonsai.state_machine1
              ~sexp_of_model:[%sexp_of: Int.t]
              ~equal:[%equal: Int.t]
              ~sexp_of_action:[%sexp_of: Bool.t]
              (opaque_const_value ())
              ~default_model:0
              ~apply_action:
                (fun
                  (_ : _ Bonsai.Apply_action_context.t) input model is_increment ->
                match input with
                | Active () -> if is_increment then model + 1 else 999
                | Inactive ->
                  print_endline "inactive";
                  model)
              ~reset:(fun context model ->
                Bonsai.Apply_action_context.schedule_event
                  context
                  (Bonsai.Apply_action_context.inject context false);
                model)
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let increment () = Handle.do_actions handle [ Action true ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          increment ();
          Handle.show handle;
          [%expect {| 1 |}];
          increment ();
          Handle.show handle;
          [%expect {| 2 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "inside match%sub" =
          let component =
            match%sub opaque_const_value true with
            | true ->
              Bonsai.state
                0
                ~sexp_of_model:[%sexp_of: Int.t]
                ~reset:(fun _ -> 999)
                ~equal:[%equal: Int.t]
            | false -> assert false
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let set_value i = Handle.do_actions handle [ Action i ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          set_value 3;
          Handle.show handle;
          [%expect {| 3 |}];
          set_value 4;
          Handle.show handle;
          [%expect {| 4 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "inside forced lazy" =
          let component =
            match%sub opaque_const_value true with
            | true ->
              (Bonsai.lazy_ [@alert "-deprecated"])
                (lazy
                  (Bonsai.state
                     0
                     ~sexp_of_model:[%sexp_of: Int.t]
                     ~equal:[%equal: Int.t]
                     ~reset:(fun _ -> 999)))
            | false -> assert false
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let set_value i = Handle.do_actions handle [ Action i ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          set_value 3;
          Handle.show handle;
          [%expect {| 3 |}];
          set_value 4;
          Handle.show handle;
          [%expect {| 4 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "next to an inactive infinitely-recursive lazy" =
          let rec infinitely_recursive_component () =
            (Bonsai.lazy_ [@alert "-deprecated"])
              (lazy (infinitely_recursive_component ()))
          in
          let component =
            match%sub opaque_const_value true with
            | true ->
              Bonsai.state
                0
                ~sexp_of_model:[%sexp_of: Int.t]
                ~reset:(fun _ -> 999)
                ~equal:[%equal: Int.t]
            | false -> infinitely_recursive_component ()
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let set_value i = Handle.do_actions handle [ Action i ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          set_value 3;
          Handle.show handle;
          [%expect {| 3 |}];
          set_value 4;
          Handle.show handle;
          [%expect {| 4 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "next to an inactive infinitely-recursive fix" =
          let infinitely_recursive_component =
            Bonsai.fix (Value.return ()) ~f:(fun ~recurse v -> recurse v)
          in
          let component =
            match%sub opaque_const_value true with
            | true ->
              Bonsai.state
                0
                ~sexp_of_model:[%sexp_of: Int.t]
                ~reset:(fun _ -> 999)
                ~equal:[%equal: Int.t]
            | false -> infinitely_recursive_component
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let set_value i = Handle.do_actions handle [ Action i ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          set_value 3;
          Handle.show handle;
          [%expect {| 3 |}];
          set_value 4;
          Handle.show handle;
          [%expect {| 4 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "inside assoc" =
          let component =
            let%sub map =
              Bonsai.assoc
                (module Int)
                (opaque_const_value (Int.Map.of_alist_exn [ 0, (); 1, () ]))
                ~f:(fun _ _ ->
                  Bonsai.state
                    0
                    ~sexp_of_model:[%sexp_of: Int.t]
                    ~equal:[%equal: Int.t]
                    ~reset:(fun _ ->
                    print_endline "resetting";
                    999))
            in
            let%arr map = map in
            let res = Map.to_alist map |> List.map ~f:(fun (k, (v, _)) -> k, v) in
            let setter (i, v) = (Map.find_exn map i |> Tuple2.get2) v in
            res, setter
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: (int * int) list] in
          Handle.show handle;
          [%expect {| ((0 0) (1 0)) |}];
          let set_value i v = Handle.do_actions handle [ Action (i, v) ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          set_value 0 3;
          Handle.show handle;
          [%expect {| ((0 3) (1 0)) |}];
          set_value 1 5;
          Handle.show handle;
          [%expect {| ((0 3) (1 5)) |}];
          reset ();
          Handle.show handle;
          [%expect
            {|
            resetting
            resetting
            ((0 999) (1 999)) |}]
        ;;

        let%expect_test "reset by bouncing back to an action (race)" =
          let component =
            let%sub model, inject =
              Bonsai.state_machine1
                ~sexp_of_model:[%sexp_of: Int.t]
                ~equal:[%equal: Int.t]
                ~sexp_of_action:[%sexp_of: Bool.t]
                (opaque_const_value ())
                ~default_model:0
                ~apply_action:
                  (fun
                    (_ : _ Bonsai.Apply_action_context.t)
                    (_ : unit Bonsai.Computation_status.t)
                    model
                    is_increment
                    -> if is_increment then model + 1 else 999)
                ~reset:(fun context model ->
                  Bonsai.Apply_action_context.schedule_event
                    context
                    (Bonsai.Apply_action_context.inject context false);
                  model)
            in
            return (Value.both model inject)
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let increment () = Handle.do_actions handle [ Action true ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          increment ();
          Handle.show handle;
          [%expect {| 1 |}];
          increment ();
          Handle.show handle;
          [%expect {| 2 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "for wrap" =
          let component =
            Bonsai.wrap
              ()
              ~sexp_of_model:[%sexp_of: Int.t]
              ~equal:[%equal: Int.t]
              ~default_model:0
              ~apply_action:
                (fun
                  (_ : _ Bonsai.Apply_action_context.t) _ model is_increment ->
                if is_increment then model + 1 else 999)
              ~reset:(fun context model ->
                Bonsai.Apply_action_context.schedule_event
                  context
                  (Bonsai.Apply_action_context.inject context false);
                model)
              ~f:(fun model inject -> return (Value.both model inject))
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let increment () = Handle.do_actions handle [ Action true ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          increment ();
          Handle.show handle;
          [%expect {| 1 |}];
          increment ();
          Handle.show handle;
          [%expect {| 2 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "inside a wrap" =
          let component =
            Bonsai.wrap
              ()
              ~sexp_of_model:[%sexp_of: Unit.t]
              ~equal:[%equal: Unit.t]
              ~default_model:()
              ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) _ () () -> ())
              ~f:(fun _ _ ->
                Bonsai.state_machine0
                  ()
                  ~sexp_of_model:[%sexp_of: Int.t]
                  ~equal:[%equal: Int.t]
                  ~sexp_of_action:[%sexp_of: Bool.t]
                  ~default_model:0
                  ~apply_action:
                    (fun
                      (_ : _ Bonsai.Apply_action_context.t) model is_increment ->
                    if is_increment then model + 1 else 999)
                  ~reset:(fun context model ->
                    Bonsai.Apply_action_context.schedule_event
                      context
                      (Bonsai.Apply_action_context.inject context false);
                    model))
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: int] in
          let increment () = Handle.do_actions handle [ Action true ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          Handle.show handle;
          [%expect {| 0 |}];
          increment ();
          Handle.show handle;
          [%expect {| 1 |}];
          increment ();
          Handle.show handle;
          [%expect {| 2 |}];
          reset ();
          Handle.show handle;
          [%expect {| 999 |}]
        ;;

        let%expect_test "inside assoc_on" =
          let component =
            let%sub map =
              Bonsai.Expert.assoc_on
                (module Int)
                (module Bool)
                (opaque_const_value (Int.Map.of_alist_exn [ 0, true; 1, false; 2, true ]))
                ~get_model_key:(fun _ -> Fn.id)
                ~f:(fun _ _ ->
                  Bonsai.state
                    0
                    ~sexp_of_model:[%sexp_of: Int.t]
                    ~equal:[%equal: Int.t]
                    ~reset:(fun _ ->
                    print_endline "resetting";
                    999))
            in
            let%arr map = map in
            let res = Map.to_alist map |> List.map ~f:(fun (k, (v, _)) -> k, v) in
            let setter (i, v) = (Map.find_exn map i |> Tuple2.get2) v in
            res, setter
          in
          let handle = build_handle component ~sexp_of:[%sexp_of: (int * int) list] in
          Handle.show handle;
          [%expect {| ((0 0) (1 0) (2 0)) |}];
          let set_value i v = Handle.do_actions handle [ Action (i, v) ] in
          let reset () = Handle.do_actions handle [ Reset ] in
          set_value 0 3;
          Handle.show handle;
          [%expect {| ((0 3) (1 0) (2 3)) |}];
          set_value 1 5;
          Handle.show handle;
          [%expect {| ((0 3) (1 5) (2 3)) |}];
          reset ();
          Handle.recompute_view handle;
          (* notice that there are two printings of 'resetting' because even though
             there's three active components, there are only two models between them *)
          [%expect {|
              resetting
              resetting |}];
          Handle.show handle;
          [%expect {| ((0 999) (1 999) (2 999)) |}]
        ;;
      end)
    ;;

    let%expect_test "inactive delivery to assoc_on with shared model keys" =
      let var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
      let component =
        Bonsai.Expert.assoc_on
          (module Int)
          (module Unit)
          (Bonsai.Var.value var)
          ~get_model_key:(fun _key _data -> ())
          ~f:(fun _key _data ->
            Bonsai.state_machine1
              ~sexp_of_model:[%sexp_of: Int.t]
              ~equal:[%equal: Int.t]
              ~sexp_of_action:[%sexp_of: Int.t]
              ~default_model:0
              ~apply_action:
                (fun
                  (_ : _ Bonsai.Apply_action_context.t) input model new_model ->
                match input with
                | Active () -> new_model
                | Inactive ->
                  print_endline "inactive";
                  model)
              (opaque_const_value ()))
      in
      let handle =
        Handle.create
          (module struct
            type t = (int * (int -> unit Effect.t)) Int.Map.t
            type incoming = Nothing.t

            let incoming _ = Nothing.unreachable_code

            let view (map : t) =
              map
              |> Map.to_alist
              |> List.map ~f:(fun (i, (s, _)) -> i, s)
              |> [%sexp_of: (int * int) list]
              |> Sexp.to_string_hum
            ;;
          end)
          component
      in
      print_computation (fun _ -> component);
      Handle.show handle;
      let result = Handle.last_result handle in
      let set key to_what =
        let _, set = Map.find_exn result key in
        Ui_effect.Expert.handle (set to_what)
      in
      let set_one = set 1 in
      let set_two = set 2 in
      (* Delivery to existing key in input map works *)
      set_two 3;
      Handle.show handle;
      Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, () ]);
      Handle.show handle;
      (* 2 is no longer in the input map, so setting it should fail, even though its model
         is still in the model map *)
      set_two 4;
      Handle.show handle;
      (* 1 is still in the input map, however, so it can be set *)
      set_one 5;
      Handle.show handle;
      (* Reintroducing 2 will have it share the model *)
      Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, (); 2, () ]);
      Handle.show handle;
      [%expect
        {|
          (Assoc_on
            (map Incr)
            (io_key_id    (Test 1))
            (model_key_id (Test 2))
            (model_cmp_id (Test 3))
            (data_id      (Test 4))
            (by (Leaf1 (input Incr))))
          ((1 0) (2 0))
          ((1 3) (2 3))
          ((1 3))
          inactive
          ((1 3))
          ((1 5))
          ((1 5) (2 5)) |}]
    ;;
  end)
;;

let%test_module "testing Bonsai internals" =
  (module struct
    (* This module tests internal details of Bonsai, and the results are sensitive to
       implementation changes. *)
    [@@@alert "-rampantly_nondeterministic"]

    let%expect_test "remove unused models in assoc" =
      let var = Bonsai.Var.create Int.Map.empty in
      let module State_with_setter = struct
        type t =
          { state : string
          ; set_state : string -> unit Effect.t
          }
      end
      in
      let module Action = struct
        type t = Set of string
      end
      in
      let component =
        Bonsai.assoc
          (module Int)
          (Bonsai.Var.value var)
          ~f:(fun _key _data ->
            let%sub v =
              Bonsai.state
                "hello"
                ~sexp_of_model:[%sexp_of: String.t]
                ~equal:[%equal: String.t]
            in
            return
            @@
            let%map state, set_state = v in
            { State_with_setter.state; set_state })
      in
      let handle =
        Handle.create
          (module struct
            type t = State_with_setter.t Int.Map.t
            type incoming = int * Action.t

            let incoming (map : t) (id, action) =
              let t = Map.find_exn map id in
              match (action : Action.t) with
              | Set value -> t.set_state value
            ;;

            let view (map : t) =
              map
              |> Map.to_alist
              |> List.map ~f:(fun (i, { state; set_state = _ }) -> i, state)
              |> [%sexp_of: (int * string) list]
              |> Sexp.to_string_hum
            ;;
          end)
          component
      in
      Handle.show_model handle;
      [%expect {| () |}];
      Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, (); 2, () ]);
      Handle.show_model handle;
      [%expect {| () |}];
      (* use the setter to re-establish the default *)
      Handle.do_actions handle [ 1, Set "test" ];
      Handle.show_model handle;
      [%expect {| ((1 test)) |}];
      Handle.do_actions handle [ 1, Set "hello" ];
      Handle.show_model handle;
      [%expect {| () |}]
    ;;
  end)
;;

let%expect_test "multiple maps respect cutoff" =
  let component input =
    input
    |> Value.map ~f:(fun (_ : int) -> ())
    |> Value.map ~f:(fun () -> print_endline "triggered")
    |> return
  in
  let var = Bonsai.Var.create 1 in
  let handle =
    Handle.create (Result_spec.sexp (module Unit)) (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {|
         triggered
         () |}];
  Bonsai.Var.set var 2;
  (* Cutoff happens on the unit, so "triggered" isn't printed *)
  Handle.show handle;
  [%expect {| () |}]
;;

let%expect_test "let syntax is collapsed upon eval" =
  let computation =
    let%arr () = opaque_const_value ()
    and () = opaque_const_value ()
    and () = opaque_const_value ()
    and () = opaque_const_value ()
    and () = opaque_const_value ()
    and () = opaque_const_value ()
    and () = opaque_const_value () in
    ()
  in
  let packed =
    let open Bonsai.Private in
    let computation = reveal_computation computation in
    let (T { model; input = _; apply_action = _; action; run; reset = _ }) =
      computation |> pre_process |> gather
    in
    let T =
      Bonsai.Private.Action.Type_id.same_witness_exn Action.Type_id.nothing action
    in
    let snapshot =
      run
        ~environment:Environment.empty
        ~path:Path.empty
        ~clock:(Bonsai.Time_source.create ~start:(Time_ns.now ()))
        ~inject:unreachable_action
        ~model:(Ui_incr.return model.default)
    in
    Snapshot.result snapshot |> Ui_incr.pack
  in
  let filename = Stdlib.Filename.temp_file "incr" "out" in
  Ui_incr.Packed.save_dot_to_file filename [ packed ];
  let dot_contents = In_channel.read_all filename in
  require
    [%here]
    ~if_false_then_print_s:(lazy [%message "No Map7 node found"])
    (String.is_substring dot_contents ~substring:"Map7")
;;

let%test_unit "constant prop doesn't happen" =
  (* Just make sure that this expression doesn't crash *)
  let (_ : int Computation.t) =
    match%sub Value.return (First 1) with
    | First x -> Bonsai.read x
    | Second x -> Bonsai.read x
  in
  ()
;;

let%expect_test "ignored result of assoc" =
  let var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
  let component =
    let%sub _ =
      Bonsai.assoc
        (module Int)
        (Bonsai.Var.value var)
        ~f:(fun _key data ->
          (* this sub is here to make sure that bonsai doesn't
             optimize the component into an "assoc_simple" *)
          let%sub _ = Bonsai.const () in
          Bonsai.read data)
    in
    Bonsai.const ()
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  Handle.show handle;
  [%expect {|
         () |}];
  Bonsai.Var.set var (Int.Map.of_alist_exn []);
  Expect_test_helpers_core.require_does_not_raise [%here] (fun () -> Handle.show handle);
  [%expect {| () |}]
;;

let%expect_test "constant_folding on assoc containing a lifecycle" =
  let component =
    Bonsai.assoc
      (module Int)
      (opaque_const_value Int.Map.empty)
      ~f:(fun _key data ->
        let%sub () =
          Bonsai.Edge.lifecycle
            ()
            ~on_activate:(Value.return (Ui_effect.print_s [%message "hello"]))
        in
        return data)
  in
  component
  |> Bonsai.Private.reveal_computation
  |> Bonsai.Private.pre_process
  |> sexp_of_packed_computation
  |> print_s;
  [%expect
    {|
    (Assoc
      (map Incr)
      (key_id  (Test 1))
      (cmp_id  (Test 2))
      (data_id (Test 3))
      (by (
        Sub
        (from (Lifecycle (value (Constant (id (Test 4))))))
        (via (Test 5))
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
          (via (Test 7))
          (into (Return (value (Named (uid (Test 3))))))))))) |}]
;;

let%expect_test "constant_folding on assoc containing a lifecycle that depends on a \
                 value bound outside"
  =
  let component =
    let%sub a = opaque_const "hello" in
    Bonsai.assoc
      (module Int)
      (opaque_const_value Int.Map.empty)
      ~f:(fun _key data ->
        let%sub () =
          Bonsai.Edge.lifecycle
            ()
            ~on_activate:
              (let%map a = a in
               Ui_effect.print_s [%message a])
        in
        return data)
  in
  component
  |> Bonsai.Private.reveal_computation
  |> Bonsai.Private.pre_process
  |> sexp_of_packed_computation
  |> print_s;
  [%expect
    {|
      (Sub
        (from (Return (value Incr)))
        (via (Test 1))
        (into (
          Assoc
          (map Incr)
          (key_id  (Test 3))
          (cmp_id  (Test 4))
          (data_id (Test 5))
          (by (
            Sub
            (from (
              Sub
              (from (
                Return (
                  value (
                    Mapn (
                      inputs ((
                        Mapn (
                          inputs ((
                            Mapn (inputs ((Mapn (inputs ((Named (uid (Test 1))))))))))))))))))
              (via (Test 10))
              (into (
                Sub
                (from (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))
                (via (Test 12))
                (into (Lifecycle (value (Named (uid (Test 12))))))))))
            (via (Test 13))
            (into (
              Sub
              (from (Return (value (Mapn (inputs ((Named (uid (Test 13)))))))))
              (via (Test 15))
              (into (Return (value (Named (uid (Test 5))))))))))))) |}]
;;

let%expect_test "constant_folding on assoc containing a dynamic_scope" =
  let dyn_var = Bonsai.Dynamic_scope.create ~name:"dyn_var" ~fallback:0 () in
  let component =
    Bonsai.assoc
      (module Int)
      (opaque_const_value Int.Map.empty)
      ~f:(fun _key data ->
        Bonsai.Dynamic_scope.set
          dyn_var
          (opaque_const_value 1)
          ~inside:
            (let%sub x = Bonsai.Dynamic_scope.lookup dyn_var in
             return (Value.both data x)))
  in
  component
  |> Bonsai.Private.reveal_computation
  |> Bonsai.Private.pre_process
  |> sexp_of_packed_computation
  |> print_s;
  [%expect
    {|
    (Assoc
      (map Incr)
      (key_id  (Test 1))
      (cmp_id  (Test 2))
      (data_id (Test 3))
      (by (
        Store
        (id (Test 4))
        (value Incr)
        (inner (
          Sub
          (from (Fetch (id (Test 4))))
          (via (Test 6))
          (into (
            Return (
              value (
                Mapn (
                  inputs (
                    (Named (uid (Test 3)))
                    (Named (uid (Test 6)))))))))))))) |}]
;;

let%expect_test "on_display for updating a state (using on_change)" =
  let callback =
    Value.return (fun prev cur ->
      Ui_effect.print_s [%message "change!" (prev : int option) (cur : int)])
  in
  let component input =
    Bonsai.Edge.on_change'
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~callback
      input
  in
  let var = Bonsai.Var.create 1 in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = unit

           let sexp_of_t () = Sexp.Atom "rendering..."
         end))
      (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect {|
         rendering...
         (change! (prev ()) (cur 1)) |}];
  Handle.show handle;
  [%expect {| rendering... |}];
  Handle.show handle;
  [%expect {| rendering... |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {|
         rendering...
         (change! (prev (1)) (cur 2)) |}];
  Handle.show handle;
  [%expect {| rendering... |}];
  Handle.show handle;
  [%expect {| rendering... |}]
;;

let%expect_test "actor" =
  let print_int_effect = printf "%d\n" |> Bonsai.Effect.of_sync_fun in
  let component =
    let%sub _, effect =
      Bonsai.actor0
        ()
        ~sexp_of_model:[%sexp_of: Int.t]
        ~equal:[%equal: Int.t]
        ~sexp_of_action:[%sexp_of: Unit.t]
        ~default_model:0
        ~recv:(fun ~schedule_event:_ v () -> v + 1, v)
    in
    return
    @@
    let%map effect = effect in
    let%bind.Bonsai.Effect i = effect () in
    print_int_effect i
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t
        type incoming = unit

        let view _ = ""
        let incoming t () = t
      end)
      component
  in
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| 0 |}];
  Handle.do_actions handle [ (); (); () ];
  Handle.show handle;
  [%expect {|
         1
         2
         3 |}]
;;

let%expect_test "lifecycle" =
  let effect action on =
    Ui_effect.print_s [%message (action : string) (on : string)] |> Value.return
  in
  let component input =
    let rendered = Bonsai.const "" in
    if%sub input
    then (
      let%sub () =
        Bonsai.Edge.lifecycle
          ~on_activate:(effect "activate" "a")
          ~on_deactivate:(effect "deactivate" "a")
          ~after_display:(effect "after-display" "a")
          ()
      in
      rendered)
    else (
      let%sub () =
        Bonsai.Edge.lifecycle
          ~on_activate:(effect "activate" "b")
          ~on_deactivate:(effect "deactivate" "b")
          ~after_display:(effect "after-display" "b")
          ()
      in
      rendered)
  in
  let var = Bonsai.Var.create true in
  let handle =
    Handle.create (Result_spec.string (module String)) (component (Bonsai.Var.value var))
  in
  Handle.show handle;
  [%expect
    {|
         ((action activate) (on a))
         ((action after-display) (on a)) |}];
  Bonsai.Var.set var false;
  Handle.show handle;
  [%expect
    {|
         ((action deactivate) (on a))
         ((action activate) (on b))
         ((action after-display) (on b)) |}];
  Bonsai.Var.set var true;
  Handle.show handle;
  [%expect
    {|
         ((action deactivate) (on b))
         ((action activate) (on a))
         ((action after-display) (on a)) |}]
;;

let%test_module "Clock.every" =
  (module struct
    let%expect_test "Clocks that trigger immediately at the beginning" =
      let print_hi = (fun () -> print_endline "hi") |> Bonsai.Effect.of_sync_fun in
      let clocks =
        [ Bonsai.Clock.every
            ~when_to_start_next_effect:`Every_multiple_of_period_blocking
            ~trigger_on_activate:true
            (Time_ns.Span.of_sec 3.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:
              `Wait_period_after_previous_effect_finishes_blocking
            ~trigger_on_activate:true
            (Time_ns.Span.of_sec 3.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:`Wait_period_after_previous_effect_starts_blocking
            ~trigger_on_activate:true
            (Time_ns.Span.of_sec 3.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
            ~trigger_on_activate:true
            (Time_ns.Span.of_sec 3.0)
            (Value.return (print_hi ()))
        ]
      in
      List.iter clocks ~f:(fun clock ->
        let handle = Handle.create (Result_spec.sexp (module Unit)) clock in
        let move_forward_and_show () =
          Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
          Handle.recompute_view_until_stable handle
        in
        Handle.recompute_view_until_stable handle;
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| hi |}])
    ;;

    let%expect_test "Clocks that wait span length before triggering at the beginning" =
      let print_hi = (fun () -> print_endline "hi") |> Bonsai.Effect.of_sync_fun in
      let clocks =
        [ Bonsai.Clock.every
            ~when_to_start_next_effect:`Every_multiple_of_period_blocking
            ~trigger_on_activate:false
            (Time_ns.Span.of_sec 3.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:
              `Wait_period_after_previous_effect_finishes_blocking
            ~trigger_on_activate:false
            (Time_ns.Span.of_sec 3.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:`Wait_period_after_previous_effect_starts_blocking
            ~trigger_on_activate:false
            (Time_ns.Span.of_sec 3.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
            ~trigger_on_activate:false
            (Time_ns.Span.of_sec 3.0)
            (Value.return (print_hi ()))
        ]
      in
      List.iter clocks ~f:(fun clock ->
        let handle = Handle.create (Result_spec.sexp (module Unit)) clock in
        let move_forward_and_show () =
          Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
          Handle.recompute_view_until_stable handle
        in
        Handle.recompute_view_until_stable handle;
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| |}];
        move_forward_and_show ();
        [%expect {| hi |}])
    ;;

    let%expect_test "Clocks that move with a span of 0" =
      let print_hi = (fun () -> print_endline "hi") |> Bonsai.Effect.of_sync_fun in
      let clocks =
        [ Bonsai.Clock.every
            ~when_to_start_next_effect:`Every_multiple_of_period_blocking
            ~trigger_on_activate:false
            (Time_ns.Span.of_sec 0.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:
              `Wait_period_after_previous_effect_finishes_blocking
            ~trigger_on_activate:false
            (Time_ns.Span.of_sec 0.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:`Wait_period_after_previous_effect_starts_blocking
            ~trigger_on_activate:false
            (Time_ns.Span.of_sec 0.0)
            (Value.return (print_hi ()))
        ; Bonsai.Clock.every
            ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
            ~trigger_on_activate:false
            (Time_ns.Span.of_sec 0.0)
            (Value.return (print_hi ()))
        ]
      in
      List.iter clocks ~f:(fun clock ->
        let handle = Handle.create (Result_spec.sexp (module Unit)) clock in
        let move_forward_and_show () =
          Handle.advance_clock_by handle (Time_ns.Span.next Time_ns.Span.zero);
          Handle.recompute_view_until_stable handle
        in
        Handle.recompute_view_until_stable handle;
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| hi |}];
        move_forward_and_show ();
        [%expect {| hi |}])
    ;;

    let create_clock_handle
      ~start
      ~svar
      ~when_to_start_next_effect
      ~trigger_on_activate
      ~span
      =
      let action =
        Value.return
          (let%bind.Effect () =
             (Effect.of_sync_fun (fun () -> print_endline "[tick] - effect started")) ()
           in
           let%bind.Effect () = (Effect.For_testing.of_svar_fun (fun () -> !svar)) () in
           Effect.of_sync_fun (fun () -> print_endline "[tock] - effect ended") ())
      in
      let clock =
        Bonsai.Clock.every
          ~when_to_start_next_effect
          ~trigger_on_activate
          (Time_ns.Span.of_sec span)
          action
      in
      Handle.create
        ~start_time:(Time_ns.of_span_since_epoch (Time_ns.Span.of_sec start))
        (Result_spec.sexp (module Unit))
        clock
    ;;

    let print_time handle =
      let clock = Handle.clock handle in
      let now =
        Bonsai.Time_source.now clock
        |> Time_ns.to_string_abs_parts ~zone:Time_float.Zone.utc
      in
      print_endline (List.last_exn now)
    ;;

    let move_forward_and_show ?(after_show = Fn.const ()) ~handle span =
      printf "before: ";
      print_time handle;
      Handle.advance_clock_by handle (Time_ns.Span.of_sec span);
      printf "after:  ";
      print_time handle;
      Handle.recompute_view_until_stable handle;
      after_show ();
      printf "after paint: ";
      print_time handle
    ;;

    let fill_and_reset_svar ~svar =
      Effect.For_testing.Svar.fill_if_empty !svar ();
      svar := Effect.For_testing.Svar.create ()
    ;;

    let advance_and_clear_svar ~handle ~svar time =
      Handle.advance_clock_by handle (Time_ns.Span.of_sec time);
      fill_and_reset_svar ~svar
    ;;

    let%expect_test "`Wait_period_after_previous_effect_finishes_blocking behavior" =
      let svar = ref (Effect.For_testing.Svar.create ()) in
      let handle =
        create_clock_handle
          ~start:7.0
          ~svar
          ~when_to_start_next_effect:`Wait_period_after_previous_effect_finishes_blocking
          ~span:3.0
          ~trigger_on_activate:false
      in
      let move_forward_and_show = move_forward_and_show ~handle in
      Handle.recompute_view_until_stable handle;
      move_forward_and_show 1.0;
      [%expect
        {|
         before: 00:00:07.000000000Z
         after:  00:00:08.000000000Z
         after paint: 00:00:08.000000000Z |}];
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        2.0;
      [%expect
        {|
         before: 00:00:08.000000000Z
         after:  00:00:10.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:10.200000000Z |}];
      (* Does not trigger at 7s + 2 * 3s. *)
      move_forward_and_show 2.8;
      [%expect
        {|
         before: 00:00:10.200000000Z
         after:  00:00:13.000000000Z
         after paint: 00:00:13.000000000Z |}];
      (* Triggers at 7s (initial) + 3s (first tick) + 0.2s (time taken by first tick) + 3s
         (time after first click)*)
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        0.2;
      [%expect
        {|
         before: 00:00:13.000000000Z
         after:  00:00:13.200000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:13.400000000Z |}];
      (* Starting next trigger without immediately finishing/filling the svar. *)
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:13.400000000Z
         after:  00:00:16.400000000Z
         [tick] - effect started
         after paint: 00:00:16.400000000Z |}];
      (* Clock does not trigger before the current action is completed. *)
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:16.400000000Z
         after:  00:00:19.400000000Z
         after paint: 00:00:19.400000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:19.400000000Z
         after:  00:00:22.400000000Z
         after paint: 00:00:22.400000000Z |}];
      fill_and_reset_svar ~svar;
      [%expect {| [tock] - effect ended |}];
      move_forward_and_show 2.9;
      [%expect
        {|
         before: 00:00:22.400000000Z
         after:  00:00:25.300000000Z
         after paint: 00:00:25.300000000Z |}];
      move_forward_and_show 0.1;
      [%expect
        {|
         before: 00:00:25.300000000Z
         after:  00:00:25.400000000Z
         [tick] - effect started
         after paint: 00:00:25.400000000Z |}]
    ;;

    let%expect_test "`Wait_period_after_previous_effect_starts_blocking behavior" =
      let svar = ref (Effect.For_testing.Svar.create ()) in
      let handle =
        create_clock_handle
          ~trigger_on_activate:false
          ~start:7.0
          ~svar
          ~when_to_start_next_effect:`Wait_period_after_previous_effect_starts_blocking
          ~span:3.0
      in
      let move_forward_and_show = move_forward_and_show ~handle in
      Handle.show handle;
      [%expect {| () |}];
      move_forward_and_show 1.0;
      [%expect
        {|
         before: 00:00:07.000000000Z
         after:  00:00:08.000000000Z
         after paint: 00:00:08.000000000Z |}];
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        2.0;
      [%expect
        {|
         before: 00:00:08.000000000Z
         after:  00:00:10.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:10.200000000Z |}];
      (* Triggers at 7s + 6.0s unlike the
         `Wait_period_after_previous_effect_finishes_blocking version of this
         which would need to wait until 7s + 6.2s. *)
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        2.8;
      [%expect
        {|
         before: 00:00:10.200000000Z
         after:  00:00:13.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:13.200000000Z |}];
      (* The next trigger will take a long time, 10 seconds! There will be a couple of
         missed [ticks] and missed [tocks]. *)
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:13.200000000Z
         after:  00:00:16.200000000Z
         [tick] - effect started
         after paint: 00:00:16.200000000Z |}];
      (* Clock does not tick in before the previous action is complete. *)
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:16.200000000Z
         after:  00:00:19.200000000Z
         after paint: 00:00:19.200000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:19.200000000Z
         after:  00:00:22.200000000Z
         after paint: 00:00:22.200000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:22.200000000Z
         after:  00:00:25.200000000Z
         after paint: 00:00:25.200000000Z |}];
      move_forward_and_show 1.0;
      [%expect
        {|
         before: 00:00:25.200000000Z
         after:  00:00:26.200000000Z
         after paint: 00:00:26.200000000Z |}];
      fill_and_reset_svar ~svar;
      [%expect {| [tock] - effect ended |}];
      (* Time moves slightly forward which results in another trigger. (hence the
         `Wait_period_after_previous_effect_starts_blocking behavior on skips. )*)
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        0.01;
      [%expect
        {|
         before: 00:00:26.200000000Z
         after:  00:00:26.210000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:26.410000000Z |}];
      (* Next expected trigger is at 7s + 19.21s + 3s, so going to 7s + 22.11s should not
         trigger. *)
      move_forward_and_show 2.7;
      [%expect
        {|
         before: 00:00:26.410000000Z
         after:  00:00:29.110000000Z
         after paint: 00:00:29.110000000Z |}];
      (* Trigger occurs at 7s + 22.21s as expected! 1*)
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        0.1;
      [%expect
        {|
         before: 00:00:29.110000000Z
         after:  00:00:29.210000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:29.410000000Z |}]
    ;;

    let%test_module "Resilience against bugs from action time being equal to span time" =
      (module struct
        (* This test is the only one that initially presented a race condition. Although
           the other kinds of clocks' implementations did not have a race condition when first implemented,
           they are still tested in this module.*)
        let%expect_test _ =
          let svar = ref (Effect.For_testing.Svar.create ()) in
          let handle =
            create_clock_handle
              ~trigger_on_activate:false
              ~start:7.0
              ~svar
              ~when_to_start_next_effect:
                `Wait_period_after_previous_effect_starts_blocking
              ~span:3.0
          in
          let move_forward_and_show = move_forward_and_show ~handle in
          Handle.show handle;
          [%expect {| () |}];
          move_forward_and_show 3.0;
          [%expect
            {|
         before: 00:00:07.000000000Z
         after:  00:00:10.000000000Z
         [tick] - effect started
         after paint: 00:00:10.000000000Z |}];
          move_forward_and_show 3.0;
          [%expect
            {|
         before: 00:00:10.000000000Z
         after:  00:00:13.000000000Z
         after paint: 00:00:13.000000000Z |}];
          fill_and_reset_svar ~svar;
          [%expect {| [tock] - effect ended |}];
          move_forward_and_show 0.000001;
          [%expect
            {|
         before: 00:00:13.000000000Z
         after:  00:00:13.000001000Z
         [tick] - effect started
         after paint: 00:00:13.000001000Z |}]
        ;;

        let%expect_test _ =
          let svar = ref (Effect.For_testing.Svar.create ()) in
          let handle =
            create_clock_handle
              ~trigger_on_activate:false
              ~start:7.0
              ~svar
              ~when_to_start_next_effect:
                `Wait_period_after_previous_effect_finishes_blocking
              ~span:3.0
          in
          let move_forward_and_show = move_forward_and_show ~handle in
          Handle.show handle;
          [%expect {| () |}];
          move_forward_and_show 3.;
          [%expect
            {|
         before: 00:00:07.000000000Z
         after:  00:00:10.000000000Z
         [tick] - effect started
         after paint: 00:00:10.000000000Z |}];
          move_forward_and_show 3.0;
          [%expect
            {|
         before: 00:00:10.000000000Z
         after:  00:00:13.000000000Z
         after paint: 00:00:13.000000000Z |}];
          fill_and_reset_svar ~svar;
          [%expect {| [tock] - effect ended |}];
          move_forward_and_show 0.000001;
          [%expect
            {|
         before: 00:00:13.000000000Z
         after:  00:00:13.000001000Z
         after paint: 00:00:13.000001000Z |}];
          move_forward_and_show 3.0;
          [%expect
            {|
         before: 00:00:13.000001000Z
         after:  00:00:16.000001000Z
         [tick] - effect started
         after paint: 00:00:16.000001000Z |}]
        ;;

        let%expect_test _ =
          let svar = ref (Effect.For_testing.Svar.create ()) in
          let handle =
            create_clock_handle
              ~trigger_on_activate:true
              ~start:7.0
              ~svar
              ~when_to_start_next_effect:
                `Wait_period_after_previous_effect_finishes_blocking
              ~span:3.0
          in
          let move_forward_and_show = move_forward_and_show ~handle in
          Handle.show handle;
          Handle.recompute_view handle;
          [%expect {|
         ()
         [tick] - effect started |}];
          move_forward_and_show 3.;
          [%expect
            {|
         before: 00:00:07.000000000Z
         after:  00:00:10.000000000Z
         after paint: 00:00:10.000000000Z |}];
          move_forward_and_show 3.0;
          [%expect
            {|
         before: 00:00:10.000000000Z
         after:  00:00:13.000000000Z
         after paint: 00:00:13.000000000Z |}];
          fill_and_reset_svar ~svar;
          [%expect {| [tock] - effect ended |}];
          Handle.recompute_view_until_stable handle
        ;;

        let%expect_test "Next multiple clock" =
          let svar = ref (Effect.For_testing.Svar.create ()) in
          let handle =
            create_clock_handle
              ~trigger_on_activate:false
              ~start:7.0
              ~svar
              ~when_to_start_next_effect:
                `Wait_period_after_previous_effect_starts_blocking
              ~span:3.0
          in
          let move_forward_and_show = move_forward_and_show ~handle in
          Handle.show handle;
          [%expect {| () |}];
          move_forward_and_show 3.0;
          [%expect
            {|
         before: 00:00:07.000000000Z
         after:  00:00:10.000000000Z
         [tick] - effect started
         after paint: 00:00:10.000000000Z |}];
          move_forward_and_show 3.0;
          [%expect
            {|
         before: 00:00:10.000000000Z
         after:  00:00:13.000000000Z
         after paint: 00:00:13.000000000Z |}];
          fill_and_reset_svar ~svar;
          [%expect {| [tock] - effect ended |}];
          move_forward_and_show 0.000000001;
          [%expect
            {|
         before: 00:00:13.000000000Z
         after:  00:00:13.000000001Z
         [tick] - effect started
         after paint: 00:00:13.000000001Z |}]
        ;;
      end)
    ;;

    let%expect_test "`Every_multiple_of_period_blocking clock skip behavior" =
      let svar = ref (Effect.For_testing.Svar.create ()) in
      let handle =
        create_clock_handle
          ~trigger_on_activate:false
          ~start:7.0
          ~svar
          ~when_to_start_next_effect:`Every_multiple_of_period_blocking
          ~span:3.0
      in
      let move_forward_and_show = move_forward_and_show ~handle in
      Handle.show handle;
      [%expect {| () |}];
      move_forward_and_show 1.0;
      [%expect
        {|
         before: 00:00:07.000000000Z
         after:  00:00:08.000000000Z
         after paint: 00:00:08.000000000Z |}];
      (* `Every_multiple_of_period_blocking clock triggers on every t where [(t % span) = (init_time % span)]
         Since initial time is 7s, the clock will trigger on every multiple of 3,
         but offset by 1, so on 10s, 13s, 15s independent of skips.
      *)
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        2.0;
      [%expect
        {|
         before: 00:00:08.000000000Z
         after:  00:00:10.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:10.200000000Z |}];
      move_forward_and_show 2.7;
      [%expect
        {|
         before: 00:00:10.200000000Z
         after:  00:00:12.900000000Z
         after paint: 00:00:12.900000000Z |}];
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        0.1;
      [%expect
        {|
         before: 00:00:12.900000000Z
         after:  00:00:13.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:13.200000000Z |}];
      move_forward_and_show 2.8;
      [%expect
        {|
         before: 00:00:13.200000000Z
         after:  00:00:16.000000000Z
         [tick] - effect started
         after paint: 00:00:16.000000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:16.000000000Z
         after:  00:00:19.000000000Z
         after paint: 00:00:19.000000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:19.000000000Z
         after:  00:00:22.000000000Z
         after paint: 00:00:22.000000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:22.000000000Z
         after:  00:00:25.000000000Z
         after paint: 00:00:25.000000000Z |}];
      move_forward_and_show 1.0;
      [%expect
        {|
         before: 00:00:25.000000000Z
         after:  00:00:26.000000000Z
         after paint: 00:00:26.000000000Z |}];
      fill_and_reset_svar ~svar;
      [%expect {| [tock] - effect ended |}];
      move_forward_and_show 0.1;
      [%expect
        {|
         before: 00:00:26.000000000Z
         after:  00:00:26.100000000Z
         after paint: 00:00:26.100000000Z |}];
      move_forward_and_show 1.8;
      [%expect
        {|
         before: 00:00:26.100000000Z
         after:  00:00:27.900000000Z
         after paint: 00:00:27.900000000Z |}];
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        0.1;
      [%expect
        {|
         before: 00:00:27.900000000Z
         after:  00:00:28.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:28.200000000Z |}]
    ;;

    let%expect_test "`Every_multiple_of_period_non_blocking clock skip behavior" =
      let svar = ref (Effect.For_testing.Svar.create ()) in
      let handle =
        create_clock_handle
          ~trigger_on_activate:false
          ~start:7.0
          ~svar
          ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
          ~span:3.0
      in
      let move_forward_and_show = move_forward_and_show ~handle in
      Handle.show handle;
      [%expect {| () |}];
      move_forward_and_show 1.0;
      [%expect
        {|
         before: 00:00:07.000000000Z
         after:  00:00:08.000000000Z
         after paint: 00:00:08.000000000Z |}];
      (* `Every_multiple_of_period_blocking clock triggers on every t where [(t % span) = (init_time % span)]
         Since initial time is 7s, the clock will trigger on every multiple of 3,
         but offset by 1, so on 10s, 13s, 15s independent of skips.
      *)
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        2.0;
      [%expect
        {|
         before: 00:00:08.000000000Z
         after:  00:00:10.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:10.200000000Z |}];
      move_forward_and_show 2.7;
      [%expect
        {|
         before: 00:00:10.200000000Z
         after:  00:00:12.900000000Z
         after paint: 00:00:12.900000000Z |}];
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        0.1;
      [%expect
        {|
         before: 00:00:12.900000000Z
         after:  00:00:13.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:13.200000000Z |}];
      move_forward_and_show 2.8;
      [%expect
        {|
         before: 00:00:13.200000000Z
         after:  00:00:16.000000000Z
         [tick] - effect started
         after paint: 00:00:16.000000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:16.000000000Z
         after:  00:00:19.000000000Z
         [tick] - effect started
         after paint: 00:00:19.000000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:19.000000000Z
         after:  00:00:22.000000000Z
         [tick] - effect started
         after paint: 00:00:22.000000000Z |}];
      move_forward_and_show 3.0;
      [%expect
        {|
         before: 00:00:22.000000000Z
         after:  00:00:25.000000000Z
         [tick] - effect started
         after paint: 00:00:25.000000000Z |}];
      move_forward_and_show 1.0;
      [%expect
        {|
         before: 00:00:25.000000000Z
         after:  00:00:26.000000000Z
         after paint: 00:00:26.000000000Z |}];
      fill_and_reset_svar ~svar;
      [%expect
        {|
         [tock] - effect ended
         [tock] - effect ended
         [tock] - effect ended
         [tock] - effect ended |}];
      move_forward_and_show 0.1;
      [%expect
        {|
         before: 00:00:26.000000000Z
         after:  00:00:26.100000000Z
         after paint: 00:00:26.100000000Z |}];
      move_forward_and_show 1.8;
      [%expect
        {|
         before: 00:00:26.100000000Z
         after:  00:00:27.900000000Z
         after paint: 00:00:27.900000000Z |}];
      move_forward_and_show
        ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.2)
        0.1;
      [%expect
        {|
         before: 00:00:27.900000000Z
         after:  00:00:28.000000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:28.200000000Z |}]
    ;;

    let%test_module "Resilience against inactive clocks" =
      (module struct
        let%expect_test _ =
          let print_sanitized_dropped_action_if_needed =
            Effect.of_sync_fun (fun () ->
              let out = [%expect.output] in
              if String.is_empty out
              then ()
              else print_endline "[Whoops! An action was dropped!]")
          in
          List.iter
            [ `Wait_period_after_previous_effect_starts_blocking
            ; `Wait_period_after_previous_effect_finishes_blocking
            ; `Every_multiple_of_period_blocking
            ; `Every_multiple_of_period_non_blocking
            ]
            ~f:(fun when_to_start_next_effect ->
            let component =
              let%sub state, set_state =
                Bonsai.state
                  true
                  ~sexp_of_model:[%sexp_of: Bool.t]
                  ~equal:[%equal: Bool.t]
              in
              match%sub state with
              | true ->
                let%sub () =
                  Bonsai.Clock.every
                    ~when_to_start_next_effect
                    ~trigger_on_activate:false
                    (Time_ns.Span.of_sec 3.0)
                    (let%map set_state = set_state in
                     let%bind.Effect () =
                       (Effect.of_sync_fun (fun () ->
                          print_endline "[tick tock] - (state := false)"))
                         ()
                     in
                     let%bind.Effect () = set_state false in
                     print_sanitized_dropped_action_if_needed ())
                in
                Bonsai.const true
              | false ->
                let%sub () =
                  Bonsai.Edge.after_display
                    (let%map set_state = set_state in
                     let%bind.Effect () =
                       (Effect.of_sync_fun (fun () -> print_endline "(state := true)")) ()
                     in
                     let%bind.Effect () = set_state true in
                     print_sanitized_dropped_action_if_needed ())
                in
                Bonsai.const false
            in
            let start = Time_ns.of_span_since_epoch (Time_ns.Span.of_min 1.0) in
            let handle =
              Handle.create (Result_spec.sexp (module Bool)) ~start_time:start component
            in
            let move_forward_and_show = move_forward_and_show ~handle in
            Handle.show handle;
            [%expect {| true |}];
            move_forward_and_show 3.0;
            [%expect
              {|
         [Whoops! An action was dropped!]
         after paint: 00:01:03.000000000Z |}];
            Handle.show handle;
            [%expect {| true |}];
            move_forward_and_show 3.0;
            [%expect
              {|
         [Whoops! An action was dropped!]
         after paint: 00:01:06.000000000Z |}];
            Handle.show handle;
            [%expect {| true |}];
            move_forward_and_show 3.0;
            [%expect
              {|
         [Whoops! An action was dropped!]
         after paint: 00:01:09.000000000Z |}];
            Handle.show handle;
            [%expect {| true |}];
            move_forward_and_show 3.0;
            [%expect
              {|
         [Whoops! An action was dropped!]
         after paint: 00:01:12.000000000Z |}];
            Handle.show handle;
            [%expect {| true |}])
        ;;
      end)
    ;;

    let%test_module "Super small timespans on clock" =
      (module struct
        let%expect_test _ =
          List.iter
            [ `Every_multiple_of_period_blocking
            ; `Wait_period_after_previous_effect_finishes_blocking
            ; `Wait_period_after_previous_effect_starts_blocking
            ; `Every_multiple_of_period_non_blocking
            ]
            ~f:(fun when_to_start_next_effect ->
            let svar = ref (Effect.For_testing.Svar.create ()) in
            let handle =
              create_clock_handle
                ~trigger_on_activate:false
                ~start:0.0
                ~svar
                ~when_to_start_next_effect
                ~span:0.01
            in
            let move_forward_and_show = move_forward_and_show ~handle in
            Handle.show handle;
            [%expect {| () |}];
            move_forward_and_show
              ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.)
              0.01;
            [%expect
              {|
         before: 00:00:00.000000000Z
         after:  00:00:00.010000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:00.010000000Z |}];
            move_forward_and_show
              ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.)
              0.01;
            [%expect
              {|
         before: 00:00:00.010000000Z
         after:  00:00:00.020000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:00.020000000Z |}];
            move_forward_and_show
              ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.)
              0.01;
            [%expect
              {|
         before: 00:00:00.020000000Z
         after:  00:00:00.030000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:00.030000000Z |}];
            move_forward_and_show
              ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.)
              0.01;
            [%expect
              {|
         before: 00:00:00.030000000Z
         after:  00:00:00.040000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:00.040000000Z |}])
        ;;

        let%expect_test _ =
          List.iter
            [ `Every_multiple_of_period_blocking
            ; `Wait_period_after_previous_effect_finishes_blocking
            ; `Wait_period_after_previous_effect_starts_blocking
            ; `Every_multiple_of_period_non_blocking
            ]
            ~f:(fun when_to_start_next_effect ->
            let svar = ref (Effect.For_testing.Svar.create ()) in
            let handle =
              create_clock_handle
                ~trigger_on_activate:false
                ~start:0.0
                ~svar
                ~when_to_start_next_effect
                ~span:0.01
            in
            let move_forward_and_show ?(after_show = Fn.const ()) ~handle span =
              printf "before: ";
              print_time handle;
              Handle.advance_clock_by handle (Time_ns.Span.of_sec span);
              printf "after:  ";
              print_time handle;
              Handle.show handle;
              (* Advancing the clock by one second (many time the clock's time span) before recomputing.  *)
              Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
              Handle.recompute_view handle;
              after_show ();
              printf "after paint: ";
              print_time handle
            in
            let move_forward_and_show = move_forward_and_show ~handle in
            Handle.show handle;
            [%expect {| () |}];
            move_forward_and_show
              ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.)
              0.01;
            [%expect
              {|
         before: 00:00:00.000000000Z
         after:  00:00:00.010000000Z
         ()
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:01.010000000Z |}];
            move_forward_and_show
              ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.)
              0.01;
            [%expect
              {|
         before: 00:00:01.010000000Z
         after:  00:00:01.020000000Z
         ()
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:02.020000000Z |}];
            move_forward_and_show
              ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.)
              0.01;
            [%expect
              {|
         before: 00:00:02.020000000Z
         after:  00:00:02.030000000Z
         ()
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:03.030000000Z |}];
            move_forward_and_show
              ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.)
              0.01;
            [%expect
              {|
         before: 00:00:03.030000000Z
         after:  00:00:03.040000000Z
         ()
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:04.040000000Z |}])
        ;;

        let%expect_test "`Wait_period_after_previous_effect_finishes_blocking skip \
                         behavior"
          =
          let svar = ref (Effect.For_testing.Svar.create ()) in
          let handle =
            create_clock_handle
              ~trigger_on_activate:false
              ~start:7.0
              ~svar
              ~when_to_start_next_effect:
                `Wait_period_after_previous_effect_finishes_blocking
              ~span:0.01
          in
          let move_forward_and_show = move_forward_and_show ~handle in
          Handle.show handle;
          [%expect {| () |}];
          move_forward_and_show 0.005;
          [%expect
            {|
         before: 00:00:07.000000000Z
         after:  00:00:07.005000000Z
         after paint: 00:00:07.005000000Z |}];
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.005;
          [%expect
            {|
         before: 00:00:07.005000000Z
         after:  00:00:07.010000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:07.012000000Z |}];
          (* Does not trigger at 7s + 2 * 0.01. *)
          move_forward_and_show 0.008;
          [%expect
            {|
         before: 00:00:07.012000000Z
         after:  00:00:07.020000000Z
         after paint: 00:00:07.020000000Z |}];
          (* Triggers at 7s (initial) + 0.01s (first tick) + 0.002s (time taken by first tick) + 0.001s
             (time after first click)*)
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.002;
          [%expect
            {|
         before: 00:00:07.020000000Z
         after:  00:00:07.022000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:07.024000000Z |}]
        ;;

        let%expect_test "`Wait_period_after_previous_effect_starts_blocking skip behavior"
          =
          let svar = ref (Effect.For_testing.Svar.create ()) in
          let handle =
            create_clock_handle
              ~trigger_on_activate:false
              ~start:7.0
              ~svar
              ~when_to_start_next_effect:
                `Wait_period_after_previous_effect_starts_blocking
              ~span:0.01
          in
          let move_forward_and_show = move_forward_and_show ~handle in
          Handle.show handle;
          [%expect {| () |}];
          move_forward_and_show 0.005;
          [%expect
            {|
         before: 00:00:07.000000000Z
         after:  00:00:07.005000000Z
         after paint: 00:00:07.005000000Z |}];
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.005;
          [%expect
            {|
         before: 00:00:07.005000000Z
         after:  00:00:07.010000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:07.012000000Z |}];
          (* Triggers at 7s + 2 * 0.01s unlike the "minimum" version of this which would need to wait
             until 7s + 2 * 0.01s + 0.002s. *)
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.008;
          [%expect
            {|
         before: 00:00:07.012000000Z
         after:  00:00:07.020000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:07.022000000Z |}];
          (* The next trigger will take a long time, 10 seconds! There will be a couple of
             missed [ticks] and missed [tocks]. *)
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 10.)
            0.008;
          [%expect
            {|
         before: 00:00:07.022000000Z
         after:  00:00:07.030000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:17.030000000Z |}];
          (* Time moves slightly forward which results in another trigger. (hence the
             `Wait_period_after_previous_effect_starts_blocking behavior on skips. )*)
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.00001;
          [%expect
            {|
         before: 00:00:17.030000000Z
         after:  00:00:17.030010000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:17.032010000Z |}];
          move_forward_and_show 0.007;
          [%expect
            {|
         before: 00:00:17.032010000Z
         after:  00:00:17.039010000Z
         after paint: 00:00:17.039010000Z |}];
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.001;
          [%expect
            {|
         before: 00:00:17.039010000Z
         after:  00:00:17.040010000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:17.042010000Z |}]
        ;;

        let%expect_test "`Every_multiple_of_period_blocking behavior" =
          let svar = ref (Effect.For_testing.Svar.create ()) in
          let handle =
            create_clock_handle
              ~trigger_on_activate:false
              ~start:7.0
              ~svar
              ~when_to_start_next_effect:`Every_multiple_of_period_blocking
              ~span:0.01
          in
          let move_forward_and_show = move_forward_and_show ~handle in
          Handle.show handle;
          [%expect {| () |}];
          move_forward_and_show 0.005;
          [%expect
            {|
         before: 00:00:07.000000000Z
         after:  00:00:07.005000000Z
         after paint: 00:00:07.005000000Z |}];
          (* Clock triggers on every t where [(t % span) = (init_time % span)]
             Since initial time is 7s, the clock will trigger on every multiple of 3,
             but offset by 1, so on 10s, 13s, 16s independent of skips.  *)
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.005;
          [%expect
            {|
         before: 00:00:07.005000000Z
         after:  00:00:07.010000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:07.012000000Z |}];
          move_forward_and_show 0.007;
          [%expect
            {|
         before: 00:00:07.012000000Z
         after:  00:00:07.019000000Z
         after paint: 00:00:07.019000000Z |}];
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.001;
          [%expect
            {|
         before: 00:00:07.019000000Z
         after:  00:00:07.020000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:07.022000000Z |}];
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 10.0)
            0.008;
          [%expect
            {|
         before: 00:00:07.022000000Z
         after:  00:00:07.030000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:17.030000000Z |}];
          move_forward_and_show 0.001;
          [%expect
            {|
         before: 00:00:17.030000000Z
         after:  00:00:17.031000000Z
         after paint: 00:00:17.031000000Z |}];
          move_forward_and_show 0.008;
          [%expect
            {|
         before: 00:00:17.031000000Z
         after:  00:00:17.039000000Z
         after paint: 00:00:17.039000000Z |}];
          move_forward_and_show
            ~after_show:(fun () -> advance_and_clear_svar ~handle ~svar 0.002)
            0.001;
          [%expect
            {|
         before: 00:00:17.039000000Z
         after:  00:00:17.040000000Z
         [tick] - effect started
         [tock] - effect ended
         after paint: 00:00:17.042000000Z |}]
        ;;
      end)
    ;;

    let%expect_test {| [every] continues to trigger effects even when the action takes a long time |}
      =
      let match_var = Bonsai.Var.create true in
      let component =
        let%sub (), inject =
          let%sub sleep = Bonsai.Clock.sleep in
          Bonsai.state_machine1
            ~sexp_of_model:[%sexp_of: Unit.t]
            ~equal:[%equal: Unit.t]
            ~sexp_of_action:[%sexp_of: Unit.t]
            ~default_model:()
            ~apply_action:(fun context sleep () () ->
              match sleep with
              | Active sleep ->
                Bonsai.Apply_action_context.schedule_event
                  context
                  (let%bind.Effect () = sleep (Time_ns.Span.of_sec 5.0) in
                   Effect.of_sync_fun print_endline "did action")
              | Inactive -> print_endline "inactive")
            sleep
        in
        match%sub Bonsai.Var.value match_var with
        | true ->
          Bonsai.Clock.every
            ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
            ~trigger_on_activate:true
            (Time_ns.Span.of_sec 3.0)
            (let%map inject = inject in
             inject ())
        | false -> Bonsai.const ()
      in
      let handle = Handle.create (Result_spec.sexp (module Unit)) component in
      Handle.recompute_view handle;
      [%expect {| |}];
      Handle.recompute_view handle;
      [%expect {| |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
      Handle.recompute_view handle;
      [%expect {| did action |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
      Handle.recompute_view handle;
      [%expect {| |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
      Handle.recompute_view handle;
      [%expect {| did action |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
      Handle.recompute_view handle;
      [%expect {| |}]
    ;;
  end)
;;

let%expect_test "wait_after_display" =
  let component =
    let effect name =
      let%sub wait_after_display = Bonsai.Edge.wait_after_display in
      let%arr wait_after_display = wait_after_display in
      let%bind.Effect () = wait_after_display in
      Effect.print_s [%message "after display" (name : string)]
    in
    let%sub a = effect "a" in
    let%sub b = effect "b" in
    return (Value.both a b)
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t * unit Effect.t
        type incoming = bool

        let view _t = "view"
        let incoming (a, b) which = if which then a else b
      end)
      component
  in
  Handle.do_actions handle [ true; true; true ];
  Handle.show handle;
  (* BUG: we expect that the "after display" effects are triggered _after_ the call to
     [Handle.show]. i.e. the desired output of this test is:
     {[
       [%expect
         {|
          view
          ("after display" (name a))
          ("after display" (name a))
          ("after display" (name a)) |}]
     ]}
  *)
  [%expect
    {|
    view
    ("after display" (name a))
    ("after display" (name a))
    ("after display" (name a)) |}];
  Handle.show handle;
  [%expect {| view |}];
  Handle.do_actions handle [ true; false ];
  Handle.show handle;
  [%expect {|
    view
    ("after display" (name a))
    ("after display" (name b)) |}];
  Handle.do_actions handle [ false; true ];
  Handle.show handle;
  [%expect {|
    view
    ("after display" (name b))
    ("after display" (name a)) |}];
  Handle.do_actions handle [ false; false ];
  Handle.show handle;
  [%expect {|
    view
    ("after display" (name b))
    ("after display" (name b)) |}];
  Handle.do_actions handle [ false; true; false; true; false; true; false; false; false ];
  Handle.show handle;
  [%expect
    {|
    view
    ("after display" (name b))
    ("after display" (name a))
    ("after display" (name b))
    ("after display" (name a))
    ("after display" (name b))
    ("after display" (name a))
    ("after display" (name b))
    ("after display" (name b))
    ("after display" (name b)) |}]
;;

let%expect_test "wait_after_display twice in a row" =
  let component =
    let%sub wait_after_display = Bonsai.Edge.wait_after_display in
    let%arr wait_after_display = wait_after_display in
    let%bind.Effect () = wait_after_display in
    let%bind.Effect () = wait_after_display in
    Effect.print_s [%message "after display"]
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t
        type incoming = unit

        let view _t = "view"
        let incoming a () = a
      end)
      component
  in
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| view |}];
  Handle.show handle;
  [%expect {|
    view
    "after display" |}];
  Handle.show handle;
  [%expect {| view |}]
;;

let%expect_test "wait_after_display works with [recompute_view_until_stable]" =
  let component =
    let%sub wait_after_display = Bonsai.Edge.wait_after_display in
    let%arr wait_after_display = wait_after_display in
    let%bind.Effect () = wait_after_display in
    let%bind.Effect () = wait_after_display in
    let%bind.Effect () = wait_after_display in
    let%bind.Effect () = wait_after_display in
    let%bind.Effect () = wait_after_display in
    let%bind.Effect () = wait_after_display in
    let%bind.Effect () = wait_after_display in
    let%bind.Effect () = wait_after_display in
    Effect.print_s [%message "after display"]
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t
        type incoming = unit

        let view _t = "view"
        let incoming a () = a
      end)
      component
  in
  Handle.do_actions handle [ () ];
  Handle.recompute_view_until_stable handle;
  [%expect {| "after display" |}];
  Handle.show handle;
  [%expect {| view |}]
;;

let%expect_test "sleep" =
  let component =
    let%sub sleep = Bonsai.Clock.sleep in
    let%arr sleep = sleep in
    fun seconds ->
      let%bind.Effect () = sleep (Time_ns.Span.of_sec seconds) in
      Effect.print_s [%message "after sleep" (seconds : float)]
  in
  let handle =
    Handle.create
      (module struct
        type t = float -> unit Effect.t
        type incoming = float

        let view _t = "view"
        let incoming f i = f i
      end)
      component
  in
  Handle.do_actions handle [ 0.0; 1.0; 2.0; 3.0 ];
  Handle.show handle;
  [%expect {|
    ("after sleep" (seconds 0))
    view |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {|
    ("after sleep" (seconds 1))
    view |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {|
    ("after sleep" (seconds 2))
    view |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {|
    ("after sleep" (seconds 3))
    view |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| view |}];
  Handle.do_actions handle [ 3.0; 2.0; 1.0; 0.0 ];
  Handle.show handle;
  [%expect {|
    ("after sleep" (seconds 0))
    view |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {|
    ("after sleep" (seconds 1))
    view |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {|
    ("after sleep" (seconds 2))
    view |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {|
    ("after sleep" (seconds 3))
    view |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| view |}]
;;

let%expect_test "sleep twice in a row" =
  let component =
    let%sub sleep = Bonsai.Clock.sleep in
    let%arr sleep = sleep in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    Effect.print_s [%message "slept"]
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t
        type incoming = unit

        let view _t = "view"
        let incoming a () = a
      end)
      component
  in
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| view |}];
  Handle.show handle;
  [%expect {|
    slept
    view |}]
;;

let%expect_test "recompute_view_until_stable does not notice sleep effects" =
  let component =
    let%sub sleep = Bonsai.Clock.sleep in
    let%arr sleep = sleep in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    let%bind.Effect () = sleep Time_ns.Span.zero in
    Effect.print_s [%message "slept"]
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t
        type incoming = unit

        let view _t = "view"
        let incoming a () = a
      end)
      component
  in
  Handle.do_actions handle [ () ];
  Handle.recompute_view_until_stable handle;
  [%expect {| |}];
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect {|
    slept
    view |}];
  Handle.show handle;
  [%expect {|
    view |}]
;;

let%expect_test "sleep works even when switching between inactive and active" =
  let active_var = Bonsai.Var.create true in
  let component =
    match%sub Bonsai.Var.value active_var with
    | true ->
      let%sub sleep = Bonsai.Clock.sleep in
      let%arr sleep = sleep in
      fun seconds ->
        let%bind.Effect () = sleep (Time_ns.Span.of_sec seconds) in
        Effect.print_s [%message "after sleep" (seconds : float)]
    | false ->
      Bonsai.const (fun seconds -> Effect.print_s [%message "inactive" (seconds : float)])
  in
  let handle =
    Handle.create
      (module struct
        type t = float -> unit Effect.t
        type incoming = float

        let view _t = ""
        let incoming f i = f i
      end)
      component
  in
  Handle.do_actions handle [ 0.0; 1.0; 2.0; 3.0 ];
  Bonsai.Var.set active_var false;
  Handle.show handle;
  [%expect {| ("after sleep" (seconds 0)) |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| ("after sleep" (seconds 1)) |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| ("after sleep" (seconds 2)) |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| ("after sleep" (seconds 3)) |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| |}];
  Handle.do_actions handle [ 3.0 ];
  Handle.show handle;
  [%expect {| (inactive (seconds 3)) |}];
  Bonsai.Var.set active_var true;
  Handle.do_actions handle [ 3.0; 2.0; 1.0; 0.0 ];
  Handle.show handle;
  [%expect
    {|
    (inactive (seconds 3))
    (inactive (seconds 2))
    (inactive (seconds 1))
    (inactive (seconds 0)) |}];
  Handle.do_actions handle [ 3.0; 2.0; 1.0; 0.0 ];
  Handle.show handle;
  [%expect {| ("after sleep" (seconds 0)) |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| ("after sleep" (seconds 1)) |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| ("after sleep" (seconds 2)) |}];
  Bonsai.Var.set active_var false;
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| ("after sleep" (seconds 3)) |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect {| |}]
;;

let edge_poll_shared ~get_expect_output =
  let effect_tracker = Query_response_tracker.create () in
  let effect = Bonsai.Effect.For_testing.of_query_response_tracker effect_tracker in
  let var = Bonsai.Var.create "hello" in
  let component =
    Bonsai.Edge.Poll.effect_on_change
      ~sexp_of_input:[%sexp_of: String.t]
      ~sexp_of_result:[%sexp_of: String.t]
      ~equal_input:[%equal: String.t]
      ~equal_result:[%equal: String.t]
      Bonsai.Edge.Poll.Starting.empty
      (Bonsai.Var.value var)
      ~effect:(Value.return effect)
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = string option [@@deriving sexp]
         end))
      component
  in
  let trigger_display () =
    (* Polling is driven by [on_display] callbacks, which is triggered by
       [Handle.show] *)
    Handle.show handle;
    let pending = Query_response_tracker.queries_pending_response effect_tracker in
    let output = Sexp.of_string (get_expect_output ()) in
    print_s [%message (pending : string list) (output : Sexp.t)]
  in
  var, effect_tracker, trigger_display
;;

let%expect_test "Edge.poll in order" =
  let get_expect_output () = [%expect.output] in
  let var, effect_tracker, trigger_display = edge_poll_shared ~get_expect_output in
  trigger_display ();
  [%expect {|
         ((pending ())
          (output  ())) |}];
  trigger_display ();
  [%expect {|
         ((pending (hello)) (output ())) |}];
  Bonsai.Var.set var "world";
  trigger_display ();
  [%expect {|
         ((pending (hello)) (output ())) |}];
  trigger_display ();
  [%expect {|
         ((pending (world hello)) (output ())) |}];
  Query_response_tracker.maybe_respond effect_tracker ~f:(fun s ->
    Respond (String.uppercase s));
  trigger_display ();
  [%expect {| ((pending ()) (output (WORLD))) |}]
;;

(* When completing the requests out-of-order, the last-fired effect still
   wins *)
let%expect_test "Edge.poll out of order" =
  let get_expect_output () = [%expect.output] in
  let var, effect_tracker, trigger_display = edge_poll_shared ~get_expect_output in
  trigger_display ();
  [%expect {|
         ((pending ())
          (output  ())) |}];
  trigger_display ();
  [%expect {|
         ((pending (hello)) (output ())) |}];
  Bonsai.Var.set var "world";
  trigger_display ();
  [%expect {|
         ((pending (hello)) (output ())) |}];
  trigger_display ();
  [%expect {|
         ((pending (world hello)) (output ())) |}];
  Query_response_tracker.maybe_respond effect_tracker ~f:(function
    | "world" as s -> Respond (String.uppercase s)
    | _ -> No_response_yet);
  trigger_display ();
  [%expect {|
         ((pending (hello))
          (output  (WORLD))) |}];
  Query_response_tracker.maybe_respond effect_tracker ~f:(function
    | "hello" as s -> Respond (String.uppercase s)
    | _ -> No_response_yet);
  trigger_display ();
  [%expect {|
         ((pending ()) (output (WORLD))) |}]
;;

let%expect_test "Clock.now" =
  let component = Bonsai.Clock.now in
  let handle =
    Handle.create (Result_spec.sexp (module Time_ns.Alternate_sexp)) component
  in
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 0.5);
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00.5Z" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 0.7);
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:01.2Z" |}]
;;

let%expect_test "Clock.now" =
  let component =
    let%sub get_time = Bonsai.Clock.get_current_time in
    Bonsai.Edge.after_display
      (let%map get_time = get_time in
       let%bind.Effect now = get_time in
       Effect.print_s [%sexp (now : Time_ns.Alternate_sexp.t)])
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  Handle.recompute_view handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 0.5);
  Handle.recompute_view handle;
  [%expect {| "1970-01-01 00:00:00.5Z" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 0.7);
  Handle.recompute_view handle;
  [%expect {| "1970-01-01 00:00:01.2Z" |}]
;;

let%expect_test "Clock.approx_now" =
  let component = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.0) in
  let handle =
    Handle.create (Result_spec.sexp (module Time_ns.Alternate_sexp)) component
  in
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 0.5);
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 0.7);
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:01.2Z" |}]
;;

(* $MDX part-begin=chain-computation *)
let chain_computation =
  let%sub a = Bonsai.const "x" in
  let%sub b, set_b =
    Bonsai.state " " ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%sub c, set_c =
    Bonsai.state " " ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%sub d, set_d =
    Bonsai.state " " ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%sub () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      a
      ~callback:set_b
  in
  let%sub () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      b
      ~callback:set_c
  in
  let%sub () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      c
      ~callback:set_d
  in
  return (Value.map4 a b c d ~f:(sprintf "a:%s b:%s c:%s d:%s"))
;;

(* $MDX part-end *)

(* $MDX part-begin=chained-on-change *)
let%expect_test "chained on_change" =
  let handle = Handle.create (Result_spec.string (module String)) chain_computation in
  Handle.show handle;
  [%expect {| a:x b:  c:  d: |}];
  Handle.show handle;
  [%expect {| a:x b:x c:  d: |}];
  Handle.show handle;
  [%expect {| a:x b:x c:x d: |}];
  Handle.show handle;
  [%expect {| a:x b:x c:x d:x |}];
  Handle.show handle;
  [%expect {| a:x b:x c:x d:x |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=chained-on-change-recompute *)
let%expect_test "chained on_change with recompute_view_until_stable" =
  let handle = Handle.create (Result_spec.string (module String)) chain_computation in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {| a:x b:x c:x d:x |}]
;;

(* $MDX part-end *)

let%expect_test "infinite chain!" =
  let computation =
    let%sub state, set_state =
      Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
    in
    let callback =
      let%map set_state = set_state in
      fun new_state -> set_state (new_state + 1)
    in
    let%sub () =
      Bonsai.Edge.on_change
        ~sexp_of_model:[%sexp_of: Int.t]
        ~equal:[%equal: Int.t]
        state
        ~callback
    in
    Bonsai.const ()
  in
  let handle = Handle.create (Result_spec.string (module Unit)) computation in
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    Handle.recompute_view_until_stable handle);
  [%expect {| (Failure "view not stable after 100 recomputations") |}]
;;

let%expect_test "computation.all_map" =
  let component =
    let%map.Computation map =
      [ 1, Bonsai.const "a"; 2, Bonsai.const "b" ]
      |> Int.Map.of_alist_exn
      |> Computation.all_map
    in
    [%sexp_of: string Int.Map.t] map
  in
  let handle = Handle.create (Result_spec.string (module Sexp)) component in
  Handle.show handle;
  [%expect {| ((1 a)(2 b)) |}]
;;

let%expect_test "dynamic lookup" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let component =
    Bonsai.Dynamic_scope.set
      id
      (Value.return "hello")
      ~inside:(Bonsai.Dynamic_scope.lookup id)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hello |}]
;;

let%expect_test "dynamic lookup fails" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let component = Bonsai.Dynamic_scope.lookup id in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| no |}]
;;

let%expect_test "eval inside one, use inside another" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let component =
    let%sub a =
      Bonsai.Dynamic_scope.set
        id
        (Value.return "hello")
        ~inside:(Bonsai.Dynamic_scope.lookup id)
    in
    let%sub b =
      Bonsai.Dynamic_scope.set id (Value.return "world") ~inside:(Bonsai.read a)
    in
    return b
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hello |}]
;;

let%expect_test "sub outside, use inside" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let component =
    let%sub find = Bonsai.Dynamic_scope.lookup id in
    Bonsai.Dynamic_scope.set id (Value.return "hello") ~inside:(return find)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| no |}]
;;

let%expect_test "use resetter" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let component =
    Bonsai.Dynamic_scope.set' id (Value.return "hello") ~f:(fun { revert } ->
      revert (Bonsai.Dynamic_scope.lookup id))
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| no |}]
;;

let%expect_test "nested resetter" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let component =
    Bonsai.Dynamic_scope.set
      id
      (Value.return "hello")
      ~inside:
        (Bonsai.Dynamic_scope.set' id (Value.return "world") ~f:(fun { revert } ->
           revert (Bonsai.Dynamic_scope.lookup id)))
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hello |}]
;;

let%expect_test "resetter only impacts the id you target" =
  let id_a = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no-a" () in
  let id_b = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no-b" () in
  let component =
    Bonsai.Dynamic_scope.set' id_a (Value.return "hello") ~f:(fun { revert } ->
      Bonsai.Dynamic_scope.set
        id_b
        (Value.return "world")
        ~inside:
          (revert
           @@
           let%sub a = Bonsai.Dynamic_scope.lookup id_a in
           let%sub b = Bonsai.Dynamic_scope.lookup id_b in
           return (Value.map2 a b ~f:(fun a b -> a ^ " " ^ b))))
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| no-a world |}]
;;

module M = struct
  type t =
    { a : string
    ; b : int
    }
  [@@deriving sexp_of, fields ~getters ~fields]
end

let%expect_test "derived value" =
  let id =
    Bonsai.Dynamic_scope.create
      ~sexp_of:M.sexp_of_t
      ~name:"my-id"
      ~fallback:{ a = "hi"; b = 5 }
      ()
  in
  let a = Bonsai.Dynamic_scope.derived id ~get:M.a ~set:(Field.fset M.Fields.a) in
  let component =
    Bonsai.Dynamic_scope.set
      a
      (Value.return "hello")
      ~inside:(Bonsai.Dynamic_scope.lookup id)
  in
  let handle = Handle.create (Result_spec.sexp (module M)) component in
  Handle.show handle;
  [%expect {| ((a hello) (b 5)) |}]
;;

let%expect_test "derived value revert" =
  let id =
    Bonsai.Dynamic_scope.create
      ~sexp_of:M.sexp_of_t
      ~name:"my-id"
      ~fallback:{ a = "hi"; b = 5 }
      ()
  in
  let a = Bonsai.Dynamic_scope.derived id ~get:M.a ~set:(Field.fset M.Fields.a) in
  let component =
    Bonsai.Dynamic_scope.set' a (Value.return "hello") ~f:(fun { revert } ->
      revert (Bonsai.Dynamic_scope.lookup id))
  in
  let handle = Handle.create (Result_spec.sexp (module M)) component in
  Handle.show handle;
  [%expect {| ((a hi) (b 5)) |}]
;;

let%expect_test "derived value nested revert inner" =
  let id =
    Bonsai.Dynamic_scope.create
      ~sexp_of:M.sexp_of_t
      ~name:"my-id"
      ~fallback:{ a = "hi"; b = 5 }
      ()
  in
  let a = Bonsai.Dynamic_scope.derived id ~get:M.a ~set:(Field.fset M.Fields.a) in
  let component =
    Bonsai.Dynamic_scope.set
      a
      (Value.return "hello")
      ~inside:
        (Bonsai.Dynamic_scope.set' a (Value.return "world") ~f:(fun { revert } ->
           revert (Bonsai.Dynamic_scope.lookup id)))
  in
  let handle = Handle.create (Result_spec.sexp (module M)) component in
  Handle.show handle;
  [%expect {| ((a hello) (b 5)) |}]
;;

let%expect_test "derived value nested revert outer" =
  let id =
    Bonsai.Dynamic_scope.create
      ~sexp_of:M.sexp_of_t
      ~name:"my-id"
      ~fallback:{ a = "hi"; b = 5 }
      ()
  in
  let a = Bonsai.Dynamic_scope.derived id ~get:M.a ~set:(Field.fset M.Fields.a) in
  let b = Bonsai.Dynamic_scope.derived id ~get:M.b ~set:(Field.fset M.Fields.b) in
  let component =
    Bonsai.Dynamic_scope.set' a (Value.return "hello") ~f:(fun { revert } ->
      Bonsai.Dynamic_scope.set
        b
        (Value.return 1000)
        ~inside:(revert (Bonsai.Dynamic_scope.lookup id)))
  in
  let handle = Handle.create (Result_spec.sexp (module M)) component in
  Handle.show handle;
  [%expect {| ((a hi) (b 1000)) |}]
;;

let%expect_test "exactly once" =
  let component =
    Bonsai_extra.exactly_once
      (Bonsai.Value.return (Ui_effect.print_s [%message "hello!"]))
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  Handle.show handle;
  [%expect {|
         ()
         hello! |}];
  Handle.show handle;
  [%expect {| () |}]
;;

let%expect_test "exactly once with value" =
  let component =
    Bonsai_extra.exactly_once_with_value
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      (Bonsai.Value.return
         (let%bind.Ui_effect () = Ui_effect.print_s [%message "hello!"] in
          Ui_effect.return "done"))
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = string option [@@deriving sexp, equal]
         end))
      component
  in
  Handle.show handle;
  [%expect {|
         ()
         hello! |}];
  Handle.show handle;
  [%expect {| (done) |}]
;;

let%expect_test "yoink" =
  let component =
    let%sub state, set_state =
      Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
    in
    let%sub get_state = Bonsai.yoink state in
    Bonsai_extra.exactly_once
      (let%map get_state = get_state
       and set_state = set_state in
       let%bind.Bonsai.Effect () = set_state 1 in
       let%bind.Bonsai.Effect s =
         match%bind.Effect get_state with
         | Active s -> Effect.return s
         | Inactive -> Effect.never
       in
       Ui_effect.print_s [%message (s : int)])
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  Handle.show handle;
  [%expect {| () |}];
  Handle.show handle;
  [%expect {|
         (s 1)
         () |}]
;;

let%expect_test "bonk" =
  let component =
    let%sub (), inject_message =
      Bonsai.state_machine0
        ~default_model:()
        ~apply_action:(fun _context () message -> print_endline message)
        ()
    in
    let%sub bonk = Bonsai_extra.bonk in
    let%arr inject_message = inject_message
    and bonk = bonk in
    ( inject_message "immediate"
    , bonk (inject_message "bonked")
    , bonk (bonk (inject_message "double bonked")) )
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Effect.t * unit Effect.t * unit Effect.t

        type incoming =
          [ `Now
          | `Bonked_once
          | `Bonked_twice
          ]

        let view _ = ""

        let incoming (now, bonked_once, bonked_twice) = function
          | `Now -> now
          | `Bonked_once -> bonked_once
          | `Bonked_twice -> bonked_twice
        ;;
      end)
      component
  in
  Handle.show handle;
  Handle.do_actions
    handle
    [ `Now
    ; `Bonked_once
    ; `Bonked_twice
    ; `Now
    ; `Now
    ; `Bonked_twice
    ; `Bonked_once
    ; `Bonked_twice
    ; `Bonked_once
    ; `Now
    ];
  Handle.show handle;
  [%expect
    {|
    immediate
    immediate
    immediate
    immediate
    bonked
    bonked
    bonked
    double bonked
    double bonked
    double bonked |}]
;;

let%expect_test "bonk sorts a list" =
  let component =
    let%sub (items, inject_item), reset =
      Bonsai.with_model_resetter
        (Bonsai.state_machine0
           ~default_model:[]
           ~apply_action:(fun _context l i -> l @ [ i ])
           ())
    in
    let%sub bonk = Bonsai_extra.bonk in
    let%arr inject_item = inject_item
    and bonk = bonk
    and reset = reset
    and items = items in
    ( items
    , fun l ->
        let%bind.Effect () = reset in
        List.map l ~f:(fun n -> Fn.apply_n_times ~n bonk (inject_item n)) |> Effect.Many )
  in
  let handle =
    Handle.create
      (module struct
        type t = int list * (int list -> unit Effect.t)
        type incoming = int list

        let view (l, _) = Sexp.to_string_hum [%sexp (l : int list)]
        let incoming (_, f) = f
      end)
      component
  in
  Handle.show handle;
  [%expect {| () |}];
  Handle.do_actions handle [ [ 6; 4; 1; 3; 2; 3; 5 ] ];
  Handle.show handle;
  [%expect {|
    (1 2 3 3 4 5 6) |}];
  Handle.do_actions handle [ [ 7; 1; 8; 2; 3; 1; 2; 4 ] ];
  Handle.show handle;
  [%expect {| (1 1 2 2 3 4 7 8) |}]
;;

let%expect_test "freeze" =
  let var = Bonsai.Var.create "hello" in
  let component =
    Bonsai.freeze
      ~sexp_of_model:[%sexp_of: String.t]
      (Bonsai.Var.value var)
      ~equal:[%equal: String.t]
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| hello |}];
  Bonsai.Var.set var "world";
  Handle.show handle;
  [%expect {| hello |}]
;;

let%expect_test "effect-lazy" =
  let message = Bonsai.Var.create "hello" in
  let on = Bonsai.Var.create true in
  let component =
    let%sub on_deactivate =
      let%arr message = Bonsai.Var.value message in
      let a =
        print_endline "computing a...";
        Effect.print_s [%sexp "a", (message : string)]
      in
      let b =
        Effect.lazy_
          (lazy
            (print_endline "computing b...";
             Effect.print_s [%sexp "b", (message : string)]))
      in
      Effect.Many [ a; b ]
    in
    if%sub Bonsai.Var.value on
    then Bonsai.Edge.lifecycle ~on_deactivate ()
    else Bonsai.const ()
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  Handle.show handle;
  Bonsai.Var.set message "there";
  Handle.show handle;
  Bonsai.Var.set message "world";
  Handle.show handle;
  [%expect
    {|
         computing a...
         ()
         computing a...
         ()
         computing a...
         () |}];
  Bonsai.Var.set on false;
  Handle.show handle;
  [%expect
    {|
         ()
         (a world)
         computing b...
         (b world) |}]
;;

let%expect_test "id_gen" =
  let module Id = Bonsai_extra.Id_gen (Int) () in
  let component =
    let%sub next = Id.component in
    Bonsai.Edge.after_display
      (let%map next = next in
       let%bind.Bonsai.Effect id = next in
       Ui_effect.print_s [%sexp (id : Id.t)])
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  Handle.recompute_view handle;
  [%expect {|
         0
         1
         2
         3 |}]
;;

let%expect_test "with_self_effect" =
  let module Result_spec = struct
    type action =
      | Set of int
      | Print

    type t = string * (action -> unit Effect.t) [@@deriving sexp]
    type incoming = action

    let equal = phys_equal
    let view (result, _) = result
    let incoming (_, self_effect) = self_effect
  end
  in
  let component =
    Bonsai_extra.with_self_effect
      ()
      ~sexp_of_model:[%sexp_of: Result_spec.t]
      ~equal:[%equal: Result_spec.t]
      ~f:(fun input ->
      let%sub state =
        Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
      in
      let%arr number, set_number = state
      and input = input in
      let effect action =
        match action with
        | Result_spec.Print ->
          (match%bind.Effect input with
           | Active (computed, (_ : Result_spec.action -> unit Effect.t)) ->
             Effect.print_s [%message "Active" (computed : string)]
           | Inactive -> Effect.print_s [%message "Inactive"])
        | Set i -> set_number i
      in
      let computed = sprintf "the value: [%d]" number in
      computed, effect)
  in
  let handle = Handle.create (module Result_spec) component in
  Handle.show handle;
  [%expect {| the value: [0] |}];
  Handle.do_actions handle [ Print ];
  [%expect {||}];
  Handle.show handle;
  [%expect {|
    (Active (computed "the value: [0]"))
    the value: [0] |}];
  Handle.do_actions handle [ Set 1 ];
  Handle.show handle;
  [%expect {| the value: [1] |}];
  Handle.do_actions handle [ Set 5; Print; Set 6; Print ];
  Handle.show handle;
  [%expect
    {|
    (Active (computed "the value: [5]"))
    (Active (computed "the value: [6]"))
    the value: [6] |}]
;;

let%expect_test "state_machine_dynamic_model" =
  let component =
    Bonsai_extra.state_machine0_dynamic_model
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      (module String)
      ~model:
        (`Computed
          (Bonsai.Value.return (function
            | None -> "not set "
            | Some s -> sprintf "set %s" s)))
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) _model action -> action)
  in
  let handle =
    Handle.create
      (module struct
        type t = string * (string -> unit Effect.t)
        type incoming = string

        let view (s, _) = s
        let incoming (_, s) = s
      end)
      component
  in
  Handle.show handle;
  [%expect {| not set |}];
  Handle.do_actions handle [ "here" ];
  Handle.show handle;
  [%expect {| set here |}]
;;

let%expect_test "portal" =
  let var = Bonsai.Var.create (Sexp.Atom "hello") in
  let component =
    Bonsai_extra.with_inject_fixed_point (fun inject ->
      let%sub () =
        Bonsai.Edge.on_change
          ~sexp_of_model:[%sexp_of: Sexp.t]
          ~equal:[%equal: Sexp.t]
          (Bonsai.Var.value var)
          ~callback:inject
      in
      Bonsai.const ((), Ui_effect.print_s))
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  (* this is only necessary because I use on_change, which uses after-display.
     In an action-handler, the actions would be scheduled on the same frame. *)
  Handle.recompute_view_until_stable handle;
  [%expect {| hello |}];
  Bonsai.Var.set var (Sexp.Atom "world");
  Handle.recompute_view_until_stable handle;
  [%expect {| world |}]
;;

let%expect_test "portal 2" =
  let component =
    Bonsai_extra.with_inject_fixed_point (fun inject_fix ->
      let%sub state1, inject1 =
        Bonsai.state_machine1
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:0
          ~apply_action:(fun context inject model action ->
            match inject with
            | Active inject ->
              Bonsai.Apply_action_context.schedule_event context (inject (model + action));
              action
            | Inactive ->
              print_endline "inactive";
              model)
          inject_fix
      in
      let%sub (), inject2 =
        Bonsai.state_machine1
          ~sexp_of_model:[%sexp_of: Unit.t]
          ~equal:[%equal: Unit.t]
          ~sexp_of_action:[%sexp_of: Int.t]
          ~default_model:()
          ~apply_action:(fun context state1 _model action ->
            Bonsai.Apply_action_context.schedule_event
              context
              (Ui_effect.print_s
                 [%message (state1 : int Bonsai.Computation_status.t) (action : int)]);
            ())
          state1
      in
      return @@ Bonsai.Value.both inject1 inject2)
  in
  let handle =
    Handle.create
      (module struct
        type t = int -> unit Effect.t
        type incoming = int

        let view _ = ""
        let incoming = Fn.id
      end)
      component
  in
  Handle.show handle;
  [%expect {||}];
  Handle.do_actions handle [ 1 ];
  Handle.recompute_view handle;
  [%expect {| ((state1 (Active 1)) (action 1)) |}];
  Handle.do_actions handle [ 5 ];
  Handle.recompute_view handle;
  [%expect {| ((state1 (Active 5)) (action 6)) |}];
  Handle.do_actions handle [ 10 ];
  Handle.recompute_view handle;
  [%expect {| ((state1 (Active 10)) (action 15)) |}]
;;

let%expect_test "pipe" =
  let component =
    let%sub push_and_pop = Bonsai_extra.pipe (module String) in
    return
    @@
    let%map push, pop = push_and_pop in
    let pop s =
      let%bind.Bonsai.Effect a = pop in
      Ui_effect.print_s [%sexp "pop", (s : string), (a : string)]
    in
    push, pop
  in
  let handle =
    Handle.create
      (module struct
        type t = (string -> unit Effect.t) * (string -> unit Effect.t)

        type incoming =
          [ `Push of string
          | `Pop of string
          ]

        let view _ = ""

        let incoming (push, pop) = function
          | `Push s -> push s
          | `Pop s -> pop s
        ;;
      end)
      component
  in
  Handle.do_actions handle [ `Push "hello"; `Pop "a" ];
  Handle.recompute_view handle;
  [%expect {| (pop a hello) |}];
  Handle.do_actions handle [ `Push "world" ];
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.do_actions handle [ `Pop "b" ];
  Handle.recompute_view handle;
  [%expect {| (pop b world) |}];
  Handle.do_actions handle [ `Pop "c" ];
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.do_actions handle [ `Push "foo" ];
  Handle.recompute_view handle;
  [%expect {| (pop c foo) |}];
  Handle.do_actions
    handle
    [ `Push "hello"; `Push "world"; `Push "foo"; `Pop "a"; `Pop "b"; `Pop "c" ];
  Handle.recompute_view handle;
  [%expect {|
         (pop a hello)
         (pop b world)
         (pop c foo) |}]
;;

let%expect_test "multi-thunk" =
  let module Id = Core.Unique_id.Int () in
  let id =
    Bonsai.Expert.thunk (fun () ->
      print_endline "pulling id!";
      Id.create ())
  in
  let component =
    let%map.Computation a = id
    and b = id in
    sprintf "%s %s" (Id.to_string a) (Id.to_string b)
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {|
    pulling id!
    pulling id!
    "1 0" |}]
;;

let%expect_test "evaluation of pure values under a match%sub" =
  let depending_on = Bonsai.Var.create 0 in
  let determines_use = Bonsai.Var.create false in
  let component =
    let%sub used_somewhere =
      match%sub opaque_const_value true with
      | true ->
        let%sub () =
          Bonsai.Edge.lifecycle
            ()
            ~on_activate:(Value.return (Effect.print_s [%message "activating!"]))
        in
        let%arr depending_on = Bonsai.Var.value depending_on in
        print_s [%message "doing work" (depending_on : int)];
        depending_on
      | false -> assert false
    in
    match%sub Bonsai.Var.value determines_use with
    | true -> return used_somewhere
    | false -> Bonsai.const (-1)
  in
  let handle = Handle.create (Result_spec.sexp (module Int)) component in
  (* Even though [determines_use] is false in this and other cases, we do work
     unnecessarily. *)
  Handle.show handle;
  [%expect {|
    ("doing work" (depending_on 0))
    -1
    activating! |}];
  Bonsai.Var.set determines_use true;
  Handle.show handle;
  [%expect {| 0 |}];
  Bonsai.Var.set determines_use false;
  Handle.show handle;
  [%expect {| -1 |}];
  Bonsai.Var.set depending_on 1;
  Handle.show handle;
  [%expect {|
    ("doing work" (depending_on 1))
    -1 |}];
  Bonsai.Var.set depending_on 2;
  Handle.show handle;
  [%expect {|
    ("doing work" (depending_on 2))
    -1 |}]
;;

let%expect_test "evaluation of pure values under an assoc" =
  let depending_on = Bonsai.Var.create 0 in
  let determines_use = Bonsai.Var.create false in
  let component =
    let%sub used_somewhere =
      Bonsai.assoc
        (module Int)
        (opaque_const_value (Int.Map.of_alist_exn [ 1, () ]))
        ~f:(fun _key data ->
          let%sub () =
            Bonsai.Edge.lifecycle
              ()
              ~on_activate:(Value.return (Effect.print_s [%message "activating!"]))
          in
          let%arr depending_on = Bonsai.Var.value depending_on
          and () = data in
          print_s [%message "doing work" (depending_on : int)];
          depending_on)
    in
    match%sub Bonsai.Var.value determines_use with
    | true -> return used_somewhere
    | false -> Bonsai.const Int.Map.empty
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int Map.M(Int).t [@@deriving sexp_of]
         end))
      component
  in
  Handle.show handle;
  [%expect {|
    ("doing work" (depending_on 0))
    ()
    activating! |}];
  Bonsai.Var.set determines_use true;
  Handle.show handle;
  [%expect {|
    ((1 0)) |}];
  Bonsai.Var.set determines_use false;
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set depending_on 1;
  Handle.show handle;
  [%expect {|
    ("doing work" (depending_on 1))
    () |}];
  Bonsai.Var.set depending_on 2;
  Handle.show handle;
  [%expect {|
    ("doing work" (depending_on 2))
    () |}]
;;

let%expect_test "evaluation of pure values as an input to an assoc (with a state in the \
                 assoc)"
  =
  let depending_on = Bonsai.Var.create 0 in
  let determines_use = Bonsai.Var.create false in
  let component =
    let%sub input_map =
      let%arr depending_on = Bonsai.Var.value depending_on in
      print_endline "doing work";
      Int.Map.of_alist_exn [ depending_on, () ]
    in
    let%sub used_somewhere =
      let%sub intermediate =
        Bonsai.assoc
          (module Int)
          input_map
          ~f:(fun _key _data ->
            let%sub _ = Bonsai.state () in
            Bonsai.const ())
      in
      Bonsai.Map.cutoff intermediate ~equal:phys_equal
    in
    match%sub Bonsai.Var.value determines_use with
    | true -> return used_somewhere
    | false -> Bonsai.const Int.Map.empty
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Map.M(Int).t

        include Result_spec.No_incoming

        let view map = [%sexp (map : unit Map.M(Int).t)] |> Sexp.to_string
      end)
      component
  in
  Handle.show handle;
  [%expect {|
    doing work
    () |}];
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set depending_on 1;
  Handle.show handle;
  [%expect {|
    doing work
    () |}];
  Bonsai.Var.set depending_on 2;
  Handle.show handle;
  [%expect {|
    doing work
    () |}]
;;

let%expect_test "evaluation of pure values as an input to an assoc (without a state in \
                 the assoc)"
  =
  let depending_on = Bonsai.Var.create 0 in
  let determines_use = Bonsai.Var.create false in
  let component =
    let%sub input_map =
      let%arr depending_on = Bonsai.Var.value depending_on in
      print_endline "doing work";
      Int.Map.of_alist_exn [ depending_on, () ]
    in
    let%sub used_somewhere =
      let%sub intermediate =
        Bonsai.assoc (module Int) input_map ~f:(fun _key _data -> Bonsai.const ())
      in
      Bonsai.Map.cutoff intermediate ~equal:phys_equal
    in
    match%sub Bonsai.Var.value determines_use with
    | true -> return used_somewhere
    | false -> Bonsai.const Int.Map.empty
  in
  let handle =
    Handle.create
      (module struct
        type t = unit Map.M(Int).t

        include Result_spec.No_incoming

        let view map = [%sexp (map : unit Map.M(Int).t)] |> Sexp.to_string
      end)
      component
  in
  Handle.show handle;
  [%expect {| () |}];
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set depending_on 1;
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set depending_on 2;
  Handle.show handle;
  [%expect {| () |}]
;;

let%expect_test "scope_model" =
  let var = Bonsai.Var.create true in
  let component =
    Bonsai.scope_model
      (module Bool)
      ~on:(Bonsai.Var.value var)
      (Bonsai.state
         "default"
         ~sexp_of_model:[%sexp_of: String.t]
         ~equal:[%equal: String.t])
  in
  let handle =
    Handle.create
      (module struct
        type t = string * (string -> unit Effect.t)
        type incoming = string

        let view (s, _) = s
        let incoming (_, s) = s
      end)
      component
  in
  Handle.show handle;
  [%expect {| default |}];
  Handle.do_actions handle [ "a" ];
  Handle.show handle;
  [%expect {| a |}];
  Bonsai.Var.set var false;
  Handle.show handle;
  [%expect {| default |}];
  Handle.do_actions handle [ "b" ];
  Handle.show handle;
  [%expect {| b |}];
  Bonsai.Var.set var true;
  Handle.show handle;
  [%expect {| a |}]
;;

let%expect_test "thunk-storage" =
  let module Id = Core.Unique_id.Int () in
  let var = Bonsai.Var.create true in
  let id =
    Bonsai.Expert.thunk (fun () ->
      print_endline "pulling id!";
      Id.create ())
  in
  let component =
    if%sub Bonsai.Var.value var
    then (
      let%map.Computation id = id in
      Id.to_string id)
    else Bonsai.const ""
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {|
         pulling id!
         0 |}];
  Bonsai.Var.set var false;
  Handle.show handle;
  [%expect {| "" |}];
  Bonsai.Var.set var true;
  Handle.show handle;
  [%expect {| 0 |}]
;;

let%expect_test "action dropped in match%sub" =
  let component =
    let%sub x, set_x =
      Bonsai.state true ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t]
    in
    match%sub x with
    | true ->
      let%sub (), inject =
        Bonsai.state_machine1
          ~sexp_of_model:[%sexp_of: Unit.t]
          ~equal:[%equal: Unit.t]
          ~sexp_of_action:[%sexp_of: Unit.t]
          ~default_model:()
          ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) input () () ->
            match input with
            | Active () -> print_endline "active"
            | Inactive -> print_endline "inactive")
          (opaque_const_value ())
      in
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map inject = inject
           and set_x = set_x in
           let%bind.Effect () = set_x false in
           (* This call to [inject] below successfully schedules the effect,
              but the effect never gets run because the effect that
              just got executed switched which branch of the [match%sub] was
              active, thus making it impossible to run the [apply_action]
              function of the [state_machine1]. A similar component that uses
              [state_machine0] would not have this problem. *)
           inject ())
        ()
    | false -> Bonsai.const ()
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  Handle.show handle;
  [%expect {| () |}];
  Handle.show handle;
  [%expect {|
    inactive
    () |}]
;;

let%test_module "mirror" =
  (module struct
    let prepare_test ~store ~interactive =
      let store = Bonsai.Var.create store in
      let interactive = Bonsai.Var.create interactive in
      let store_set =
        (fun value ->
          printf "store set to \"%s\"" value;
          Bonsai.Var.set store value)
        |> Ui_effect.of_sync_fun
      in
      let interactive_set =
        (fun value ->
          printf "interactive set to \"%s\"" value;
          Bonsai.Var.set interactive value)
        |> Ui_effect.of_sync_fun
      in
      let component =
        let%sub () =
          Bonsai_extra.mirror
            ()
            ~sexp_of_model:[%sexp_of: String.t]
            ~equal:[%equal: String.t]
            ~store_set:(Bonsai.Value.return store_set)
            ~interactive_set:(Bonsai.Value.return interactive_set)
            ~store_value:(Bonsai.Var.value store)
            ~interactive_value:(Bonsai.Var.value interactive)
        in
        return
          (let%map store = Bonsai.Var.value store
           and interactive = Bonsai.Var.value interactive in
           sprintf "store: %s, interactive: %s" store interactive)
      in
      let handle = Handle.create (Result_spec.string (module String)) component in
      handle, store, interactive
    ;;

    let%expect_test "starts stable" =
      let handle, _store, _interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}]
    ;;

    let%expect_test "starts unstable" =
      let handle, _store, _interactive = prepare_test ~store:"a" ~interactive:"b" in
      Handle.show handle;
      [%expect {|
         store: a, interactive: b
         interactive set to "a" |}];
      Handle.show handle;
      [%expect {| store: a, interactive: a |}]
    ;;

    let%expect_test "starts stable and then interactive changes" =
      let handle, _store, interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Bonsai.Var.set interactive "b";
      Handle.show handle;
      [%expect {|
         store: a, interactive: b
         store set to "b" |}];
      Handle.show handle;
      [%expect {| store: b, interactive: b |}]
    ;;

    let%expect_test "starts stable and then store changes" =
      let handle, store, _interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Bonsai.Var.set store "b";
      Handle.show handle;
      [%expect {|
         store: b, interactive: a
         interactive set to "b" |}];
      Handle.show handle;
      [%expect {| store: b, interactive: b |}]
    ;;

    let%expect_test "starts stable and then both change at the same time" =
      let handle, store, interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Bonsai.Var.set store "b";
      Bonsai.Var.set interactive "c";
      Handle.show handle;
      [%expect {|
         store: b, interactive: c
         store set to "c" |}];
      Handle.show handle;
      [%expect {| store: c, interactive: c |}]
    ;;
  end)
;;

let%test_module "mirror'" =
  (module struct
    let prepare_test ~store ~interactive =
      let store = Bonsai.Var.create store in
      let interactive = Bonsai.Var.create interactive in
      let store_set =
        (fun value ->
          printf "store set to \"%s\"" value;
          Bonsai.Var.set store (Some value))
        |> Ui_effect.of_sync_fun
      in
      let interactive_set =
        (fun value ->
          printf "interactive set to \"%s\"" value;
          Bonsai.Var.set interactive (Some value))
        |> Ui_effect.of_sync_fun
      in
      let component =
        let%sub () =
          Bonsai_extra.mirror'
            ()
            ~sexp_of_model:[%sexp_of: String.t]
            ~equal:[%equal: String.t]
            ~store_set:(Bonsai.Value.return store_set)
            ~interactive_set:(Bonsai.Value.return interactive_set)
            ~store_value:(Bonsai.Var.value store)
            ~interactive_value:(Bonsai.Var.value interactive)
        in
        let%arr store = Bonsai.Var.value store
        and interactive = Bonsai.Var.value interactive in
        sprintf
          "store: %s, interactive: %s"
          (Option.value store ~default:"<none>")
          (Option.value interactive ~default:"<none>")
      in
      let handle = Handle.create (Result_spec.string (module String)) component in
      handle, store, interactive
    ;;

    let%expect_test "starts both none" =
      let handle, _store, _interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}]
    ;;

    let%expect_test "starts interactive some" =
      let handle, _store, _interactive =
        prepare_test ~store:None ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {|
        store: <none>, interactive: hi
        store set to "hi" |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts store some" =
      let handle, _store, _interactive =
        prepare_test ~store:(Some "hi") ~interactive:None
      in
      Handle.show handle;
      [%expect
        {|
        store: hi, interactive: <none>
        interactive set to "hi" |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some (same value)" =
      let handle, _store, _interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some (different values)" =
      let handle, _store, _interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hello")
      in
      Handle.show handle;
      [%expect
        {|
        store: hi, interactive: hello
        interactive set to "hi" |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, store set " =
      let handle, store, _interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Bonsai.Var.set store (Some "hi");
      Handle.show handle;
      [%expect
        {|
        store: hi, interactive: <none>
        interactive set to "hi" |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, interactive set " =
      let handle, _store, interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Bonsai.Var.set interactive (Some "hi");
      Handle.show handle;
      [%expect {|
        store: <none>, interactive: hi
        store set to "hi" |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, both set to same value" =
      let handle, store, interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Bonsai.Var.set interactive (Some "hi");
      Bonsai.Var.set store (Some "hi");
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, both set to different values" =
      let handle, store, interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Bonsai.Var.set interactive (Some "hi");
      Bonsai.Var.set store (Some "hello");
      Handle.show handle;
      [%expect {|
        store: hello, interactive: hi
        store set to "hi" |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some, both set to different values" =
      let handle, store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set interactive (Some "abc");
      Bonsai.Var.set store (Some "def");
      Handle.show handle;
      [%expect {|
        store: def, interactive: abc
        store set to "abc" |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;

    let%expect_test "starts both some (same value), store reset to none" =
      let handle, store, _interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set store None;
      Handle.show handle;
      (* The noneness isn't propagated to interactive *)
      [%expect {| store: <none>, interactive: hi |}]
    ;;

    let%expect_test "starts both some (same value), interactive reset to none" =
      let handle, _store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set interactive None;
      Handle.show handle;
      (* The noneness isn't propagated to the store *)
      [%expect {| store: hi, interactive: <none> |}]
    ;;

    let%expect_test "starts both some (same value), both set to none" =
      let handle, store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set store None;
      Bonsai.Var.set interactive None;
      Handle.show handle;
      (* The noneness isn't propagated to the store *)
      [%expect {| store: <none>, interactive: <none> |}]
    ;;

    let%expect_test "starts both some (same value), interactive set to none, both swap" =
      let handle, store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set store None;
      Handle.show handle;
      [%expect {| store: <none>, interactive: hi |}];
      Bonsai.Var.set store (Some "abc");
      Bonsai.Var.set interactive None;
      Handle.show handle;
      [%expect
        {|
        store: abc, interactive: <none>
        interactive set to "abc" |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;

    let%expect_test "starts both some (same value), store set to none, both swap" =
      let handle, store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set interactive None;
      Handle.show handle;
      [%expect {| store: hi, interactive: <none> |}];
      Bonsai.Var.set interactive (Some "abc");
      Bonsai.Var.set store None;
      Handle.show handle;
      [%expect {|
        store: <none>, interactive: abc
        store set to "abc" |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;
  end)
;;

let%expect_test "let%arr cutoff destruction" =
  let var = Bonsai.Var.create (0, 0) in
  let value = Bonsai.Var.value var in
  let component =
    let%arr a, _ = value in
    print_endline "performing work!";
    a
  in
  let handle = Handle.create (Result_spec.string (module Int)) component in
  Handle.show handle;
  [%expect {|
    performing work!
    0 |}];
  Bonsai.Var.set var (0, 1);
  Handle.show handle;
  (* No work is performed! *)
  [%expect {| 0 |}];
  Bonsai.Var.set var (1, 1);
  Handle.show handle;
  [%expect {|
    performing work!
    1 |}]
;;

let%test_module "regression" =
  (module struct
    (* The regression in question is caused by calling [Value.both] on a dynamic
       [Value.Map] and a constant one. Instead of returning a [Value.Both] node, we'd
       return a [Value.Fast_map], where the constant value is added to the tuple inside
       the folded mapping function.  However, when the mapping function that we're folding
       into is used for getting better cutoff behavior, this "optimization" actually
       undoes it by introducing a fresly-allocated tuple which will not cutoff correctly
       anymore. *)
    module State = struct
      type t =
        { a : int
        ; b : int
        ; c : int
        }
      [@@deriving fields ~getters]
    end

    let%expect_test "" =
      let state_var = Bonsai.Var.create { State.a = 2; b = 3; c = 4 } in
      let state = Bonsai.Var.value state_var in
      let a = Value.map state ~f:State.a in
      let component b =
        let%arr a = a
        and b = b in
        printf "Recomputing ; a = %d\n" a;
        a + b
      in
      let c = component (Value.map state ~f:State.b) in
      let handle = Handle.create (Result_spec.string (module Int)) c in
      Handle.show handle;
      [%expect {|
    Recomputing ; a = 2
    5 |}];
      Bonsai.Var.update state_var ~f:(fun state -> { state with c = 4 });
      Handle.show handle;
      [%expect {|
    5 |}]
    ;;

    let%expect_test "" =
      let state_var = Bonsai.Var.create { State.a = 2; b = 3; c = 4 } in
      let state = Bonsai.Var.value state_var in
      let a = Value.map state ~f:State.a in
      let component b =
        let%arr a = a
        and b = b in
        printf "Recomputing ; a = %d\n" a;
        a + b
      in
      let c = component (Value.return 3) in
      let handle = Handle.create (Result_spec.string (module Int)) c in
      Handle.show handle;
      [%expect {|
    Recomputing ; a = 2
    5 |}];
      Bonsai.Var.update state_var ~f:(fun state -> { state with c = 4 });
      Handle.show handle;
      [%expect {| 5 |}]
    ;;
  end)
;;

let%expect_test "value_with_override" =
  let default_var = Bonsai.Var.create "First Model Value" in
  let value = Bonsai.Var.value default_var in
  let component =
    Bonsai_extra.value_with_override
      ~sexp_of_model:[%sexp_of: String.t]
      value
      ~equal:[%equal: String.t]
  in
  let handle =
    Handle.create
      (module struct
        type t = string * (string -> unit Effect.t)
        type incoming = string

        let view (s, _) = s
        let incoming (_, s) = s
      end)
      component
  in
  Handle.show handle;
  [%expect {| First Model Value |}];
  Bonsai.Var.set default_var "Second Model Value";
  Handle.show handle;
  [%expect {| Second Model Value |}];
  Handle.do_actions handle [ "First Override" ];
  Handle.show handle;
  [%expect {| First Override |}];
  Bonsai.Var.set default_var "Third Model Value";
  Handle.show handle;
  (* Changes to the variable don't matter, now that we have an override. *)
  [%expect {| First Override |}];
  Handle.do_actions handle [ "Second Override" ];
  Handle.show handle;
  [%expect {| Second Override |}]
;;

let%expect_test "value_with_override in resetter" =
  let default_var = Bonsai.Var.create "First Model Value" in
  let handle =
    let value = Bonsai.Var.value default_var in
    let component =
      Bonsai.with_model_resetter
        (Bonsai_extra.value_with_override
           ~sexp_of_model:[%sexp_of: String.t]
           ~equal:[%equal: String.t]
           value)
    in
    Handle.create
      (module struct
        type t = (string * (string -> unit Effect.t)) * unit Effect.t

        type incoming =
          [ `Override of string
          | `Reset
          ]

        let view ((s, _), _) = s

        let incoming ((_, override), reset) action =
          match action with
          | `Override s -> override s
          | `Reset -> reset
        ;;
      end)
      component
  in
  Handle.show handle;
  [%expect {| First Model Value |}];
  Bonsai.Var.set default_var "Second Model Value";
  Handle.show handle;
  [%expect {| Second Model Value |}];
  Handle.do_actions handle [ `Override "First Override" ];
  Handle.show handle;
  [%expect {| First Override |}];
  Bonsai.Var.set default_var "Third Model Value";
  Handle.show handle;
  (* Changes to the variable don't matter, now that we have an override. *)
  [%expect {| First Override |}];
  Handle.do_actions handle [ `Reset ];
  Handle.show handle;
  (* Now, the change to the variable becomes visible. *)
  [%expect {| Third Model Value |}];
  (* But we can still override *)
  Handle.do_actions handle [ `Override "Second Override" ];
  Handle.show handle;
  [%expect {| Second Override |}]
;;

let%expect_test "ordering behavior of skeleton traversal" =
  (* NOTE: This test just showcases current traversal order behavior in case it
     were to change/matter in the future. *)
  let all_values =
    [ Value.return ()
    ; Bonsai.Var.value (Bonsai.Var.create ())
    ; Value.cutoff (Value.return ()) ~equal:phys_equal
    ; Value.map (Value.both (Value.return ()) (Value.return ())) ~f:(fun ((), ()) -> ())
    ]
    |> List.reduce_exn ~f:(Value.map2 ~f:(fun () () -> ()))
  in
  let c =
    let%sub v = return all_values in
    let%sub c1 =
      Bonsai.state_machine1
        ~sexp_of_model:[%sexp_of: Unit.t]
        ~equal:[%equal: Unit.t]
        ~sexp_of_action:[%sexp_of: Unit.t]
        ~default_model:()
        v
        ~apply_action:
          (fun
            (_ : _ Bonsai.Apply_action_context.t)
            (_ : unit Bonsai.Computation_status.t)
            ()
            ()
            -> ())
    in
    let%sub c2 =
      Bonsai.state () ~sexp_of_model:[%sexp_of: Unit.t] ~equal:[%equal: Unit.t]
    in
    let%sub c3 =
      Bonsai.assoc
        (module Unit)
        (Value.return Unit.Map.empty)
        ~f:(fun _ _ -> Bonsai.const ())
    in
    let%sub c4 =
      match%sub v with
      | () -> Bonsai.const ()
    in
    let%arr () = v
    and (), _ = c1
    and (), _ = c2
    and _ = c3
    and () = c4 in
    ()
  in
  let skeleton =
    Bonsai.Private.Skeleton.Computation.of_computation
      (Bonsai.Private.reveal_computation c)
  in
  let pre_order_printer =
    object
      inherit Bonsai.Private.Skeleton.Traverse.map as super

      method! value value =
        printf "value - ";
        print_s [%message "" ~_:(Lazy.force value.node_path : Bonsai.Private.Node_path.t)];
        super#value value

      method! computation computation =
        printf "computation - ";
        print_s
          [%message "" ~_:(Lazy.force computation.node_path : Bonsai.Private.Node_path.t)];
        super#computation computation
    end
  in
  pre_order_printer#computation skeleton
  |> (ignore : Bonsai.Private.Skeleton.Computation.t -> unit);
  [%expect
    {|
    computation - _1
    computation - 1_1
    value - 1_2
    value - 1-1_1
    value - 1-1-1_1
    value - 1-1-1-1_1
    value - 1-1-1-2_1
    value - 1-1-2_1
    value - 1-1-2-1_1
    value - 1-2_1
    value - 1-2-1_1
    value - 1-2-1-1_1
    value - 1-2-1-2_1
    computation - 2_1
    computation - 2-1_1
    value - 2-1_2
    computation - 2-2_1
    computation - 2-2-1_1
    computation - 2-2-2_1
    computation - 2-2-2-1_1
    value - 2-2-2-1-1_1
    computation - 2-2-2-1-2_1
    value - 2-2-2-1-2_2
    computation - 2-2-2-2_1
    computation - 2-2-2-2-1_1
    computation - 2-2-2-2-1-1_1
    value - 2-2-2-2-1-1_2
    value - 2-2-2-2-1-1-1_1
    computation - 2-2-2-2-1-2_1
    value - 2-2-2-2-1-2_2
    computation - 2-2-2-2-2_1
    value - 2-2-2-2-2_2
    value - 2-2-2-2-2-1_1
    value - 2-2-2-2-2-1-1_1
    value - 2-2-2-2-2-1-2_1
    value - 2-2-2-2-2-1-2-1_1
    value - 2-2-2-2-2-1-2-1-1_1
    value - 2-2-2-2-2-1-2-2_1
    value - 2-2-2-2-2-1-2-2-1_1
    value - 2-2-2-2-2-1-2-2-1-1_1
    value - 2-2-2-2-2-1-2-2-2_1
    value - 2-2-2-2-2-1-2-2-2-1_1
    value - 2-2-2-2-2-1-2-2-2-1-1_1
    value - 2-2-2-2-2-1-2-2-2-2_1 |}]
;;

let%expect_test "on_activate lifecycle events are run the second frame after the \
                 component becomes active"
  =
  let input_var = Bonsai.Var.create () in
  let active_var = Bonsai.Var.create true in
  let component =
    let%sub (), inject =
      Bonsai.state_machine1
        ~sexp_of_model:[%sexp_of: Unit.t]
        ~equal:[%equal: Unit.t]
        ~sexp_of_action:[%sexp_of: Unit.t]
        ~default_model:()
        ~apply_action:
          (fun
            (_ : _ Bonsai.Apply_action_context.t)
            (_ : unit Bonsai.Computation_status.t)
            ()
            ()
            -> print_endline "on_activate")
        (Bonsai.Var.value input_var)
    in
    let%sub on_activate =
      let%arr inject = inject in
      inject ()
    in
    Bonsai.Edge.lifecycle ~on_activate ()
  in
  let handle =
    Handle.create
      (Result_spec.sexp (module Unit))
      (if%sub Bonsai.Var.value active_var then component else component)
  in
  (* The on_activate does not run in the first frame; rather, it is enqueued in the effect
     handler *)
  Handle.recompute_view handle;
  [%expect {| |}];
  (* Indeed, it does run the second frame *)
  Handle.recompute_view handle;
  [%expect {| on_activate |}];
  (* Flip the var to switch the active branch *)
  Bonsai.Var.set active_var false;
  (* Once again, it's enqueued on the first frame, not run *)
  Handle.recompute_view handle;
  [%expect {| |}];
  (* But now, if the active branch flips, the on_activate action is dropped! *)
  Bonsai.Var.set active_var true;
  Handle.recompute_view handle;
  [%expect {|
    on_activate |}]
;;

let%expect_test "State machine actions that are scheduled while running the actions for \
                 a frame are run on the same frame"
  =
  let component =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: Unit.t]
      ~equal:[%equal: Unit.t]
      ~sexp_of_action:[%sexp_of: Int.t]
      ~default_model:()
      ~apply_action:(fun context () n ->
      print_s [%message (n : int)];
      if n <= 0
      then ()
      else
        Bonsai.Apply_action_context.schedule_event
          context
          (Bonsai.Apply_action_context.inject context (n - 1)))
  in
  let handle =
    Handle.create
      (module struct
        type t = unit * (int -> unit Effect.t)
        type incoming = int

        let view _ = ""
        let incoming ((), inject) n = inject n
      end)
      component
  in
  (* Schedules the action, but does not run it yet *)
  Handle.do_actions handle [ 10 ];
  [%expect {| |}];
  (* Runs the action, which schedules more actions that all get run in the same frame *)
  Handle.recompute_view handle;
  [%expect
    {|
    (n 10)
    (n 9)
    (n 8)
    (n 7)
    (n 6)
    (n 5)
    (n 4)
    (n 3)
    (n 2)
    (n 1)
    (n 0) |}]
;;

let%expect_test "Bonsai.previous_value" =
  let input_var = Bonsai.Var.create 0 in
  let active_var = Bonsai.Var.create true in
  let component =
    match%sub Bonsai.Var.value active_var with
    | true ->
      Bonsai.previous_value
        ~sexp_of_model:[%sexp_of: Int.t]
        ~equal:[%equal: Int.t]
        (Bonsai.Var.value input_var)
    | false -> Bonsai.const None
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int option [@@deriving sexp, equal]
         end))
      component
  in
  Handle.show handle;
  [%expect {| () |}];
  Handle.show handle;
  [%expect {| (0) |}];
  Bonsai.Var.set input_var 1;
  Handle.show handle;
  [%expect {| (0) |}];
  Bonsai.Var.set input_var 2;
  Handle.show handle;
  [%expect {| (1) |}];
  Handle.show handle;
  [%expect {| (2) |}];
  Bonsai.Var.set active_var false;
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set input_var 3;
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set active_var true;
  Handle.show handle;
  [%expect {| (2) |}];
  Handle.show handle;
  [%expect {| (3) |}]
;;

let%expect_test "most_recent_some" =
  let var = Bonsai.Var.create 1 in
  let active = Bonsai.Var.create true in
  let component =
    match%sub Bonsai.Var.value active with
    | true ->
      Bonsai.most_recent_some
        ~sexp_of_model:[%sexp_of: Int.t]
        ~equal:[%equal: Int.t]
        (Bonsai.Var.value var)
        ~f:(fun x -> if x mod 2 = 0 then Some x else None)
    | false -> Bonsai.const None
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int option [@@deriving sexp]
         end))
      component
  in
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {| (2) |}];
  Handle.show handle;
  [%expect {| (2) |}];
  Bonsai.Var.set active false;
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set active true;
  Handle.show handle;
  [%expect {| (2) |}];
  Bonsai.Var.set active false;
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set var 6;
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set active true;
  Handle.show handle;
  [%expect {| (6) |}]
;;
