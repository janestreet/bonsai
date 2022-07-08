open! Core
open! Import
open Bonsai.For_open
open Bonsai.Let_syntax
module Query_response_tracker = Bonsai.Effect.For_testing.Query_response_tracker
open Proc

let sexp_of_packed_computation : type a. a Bonsai.Private.Computation.packed -> Sexp.t =
  fun (Bonsai.Private.Computation.T { t; _ }) ->
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

let%expect_test "on_display" =
  let component =
    let%sub state, set_state = Bonsai.state (module Int) ~default_model:0 in
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
    let%sub state, set_state = Bonsai.state_opt (module Int) in
    let%sub update =
      match%sub state with
      | None ->
        return
        @@ let%map set_state = set_state
        and input = input in
        Some (set_state (Some input))
      | Some state ->
        return
        @@ let%map state = state
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
      (Value.return (Int.Map.of_alist_exn [ -1, (); 1, () ]))
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
          (module Int)
          (module Int)
          ~default_model:0
          ~apply_action:(fun ~inject:_ ~schedule_event:_ () _model new_model -> new_model)
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
  let result = Handle.result handle in
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
  [%expect
    {|
    ("an action inside of Bonsai.assoc_on has been dropped because the computation is no longer active"
     (io_key 2) (model_key 0) (action 4))
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
  component |> Bonsai.Private.reveal_computation |> sexp_of_packed_computation |> print_s;
  [%expect {|
    (Assoc_simpl (map Incr)) |}]
;;

let%expect_test "simple-assoc works with paths" =
  let component =
    Bonsai.assoc
      (module String)
      (Value.return (String.Map.of_alist_exn [ "hello", (); "world", () ]))
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
  component |> Bonsai.Private.reveal_computation |> sexp_of_packed_computation |> print_s;
  [%expect {|
    (Assoc_simpl (map (Constant (id 0)))) |}]
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
      (module Int)
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ (result, _) model () ->
        String.length result + model)
      ~f:(fun model inject ->
        return
        @@ let%map model = model
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

let%expect_test "let%sub patterns" =
  let component =
    let%sub a, _b = Bonsai.const ("hello world", 5) in
    return a
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hello world |}]
;;

let%expect_test "let%sub unit rhs optimization" =
  let component =
    let%sub a = opaque_const 5 in
    let%sub b = opaque_const 6 in
    return
      (let%map a = a
       and b = b in
       a + b)
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via 2)
      (into (
        Sub
        (from (Return (value Incr)))
        (via 5)
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid 2))
                      (Named (uid 5)))))))))))
        (statefulness Stateless_from)))
      (statefulness Stateless_from)) |}]
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
  [%expect {|
    (Return (value Lazy)) |}]
;;

let%expect_test "assoc simplifies its inner computation, if possible" =
  let value = Value.return String.Map.empty in
  let component =
    Bonsai.assoc
      (module String)
      value
      ~f:(fun key data -> Bonsai.read (Value.both key data))
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect {|
    (Assoc_simpl (map (Constant (id 0)))) |}]
;;

let%expect_test "assoc with sub simplifies its inner computation, if possible" =
  let value = Bonsai.Value.return String.Map.empty in
  let component =
    Bonsai.assoc
      (module String)
      value
      ~f:(fun key data ->
        let%sub key = Bonsai.read key in
        Bonsai.read (Bonsai.Value.both key data))
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect {|
    (Assoc_simpl (map (Constant (id 0)))) |}]
;;

let%expect_test "assoc with sub simplifies its inner computation, if possible" =
  let value = Bonsai.Value.return String.Map.empty in
  let component =
    Bonsai.assoc
      (module String)
      value
      ~f:(fun key data ->
        let%sub key = Bonsai.read key in
        Bonsai.read (Bonsai.Value.both key data))
  in
  print_s Bonsai.Private.(sexp_of_packed_computation (reveal_computation component));
  [%expect {|
    (Assoc_simpl (map (Constant (id 0)))) |}]
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
          Bonsai.lazy_ (lazy (f ~t:v ~depth)))
    in
    return
    @@ let%map label = label
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

let%expect_test "dynamic action sent to non-existent assoc element" =
  let var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
  let component =
    Bonsai.assoc
      (module Int)
      (Bonsai.Var.value var)
      ~f:(fun _key _data ->
        Bonsai.state_machine1
          (module Int)
          (module Int)
          ~default_model:0
          ~apply_action:(fun ~inject:_ ~schedule_event:_ () _model new_model -> new_model)
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
  let result = Handle.result handle in
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
  [%expect
    {|
    ("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
     (key 2) (action 4))
    ((1 0)) |}];
  Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, (); 2, () ]);
  Handle.show handle;
  [%expect {| ((1 0) (2 3)) |}]
;;

let%expect_test "ping-pong between apply-static and dynamic" =
  let component =
    let%sub m, i, _ =
      Bonsai.Expert.state_machine01
        (module Int)
        (module Unit)
        (module Unit)
        ~default_model:9
        ~apply_dynamic:
          (fun ~inject_dynamic:_ ~inject_static ~schedule_event () model () ->
             if model <= 0
             then model
             else (
               printf "%d: Ping!\n" model;
               schedule_event (inject_static ());
               model - 1))
        ~apply_static:(fun ~inject_dynamic ~inject_static:_ ~schedule_event model () ->
          printf "%d: Pong!\n" model;
          schedule_event (inject_dynamic ());
          model - 1)
        (Value.return ())
    in
    let%arr m = m
    and i = i in
    m, i
  in
  let handle =
    Handle.create
      (module struct
        type t = int * (unit -> unit Effect.t)
        type incoming = unit

        let incoming (_, i) = i
        let view (x, _) = Int.to_string x
      end)
      component
  in
  Handle.do_actions handle [ () ];
  Handle.flush handle;
  [%expect
    {|
    9: Ping!
    8: Pong!
    7: Ping!
    6: Pong!
    5: Ping!
    4: Pong!
    3: Ping!
    2: Pong!
    1: Ping!
    0: Pong! |}]
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
        let result = Handle.result handle in
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

    let%expect_test "race inactive-delivery" =
      (fun _ ->
         Bonsai.Expert.race
           (module Int)
           (module Int)
           ~default_model:0
           ~apply_action:(fun ~inject:_ ~schedule_event:_ input _model new_model ->
             (match input with
              | Inactive -> print_endline "static action"
              | Active () -> print_endline "dynamic action");
             new_model)
           (Value.return ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Leaf01 (input (Constant (id 0))) name))
          (via 5)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 5)))))))
            (via 8)
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid 5)))))))
              (via 11)
              (into (
                Sub
                (from (Return (value (Mapn (inputs (Named (uid 5)))))))
                (via 14)
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          Mapn (
                            inputs (
                              (Named (uid 8))
                              (Mapn (
                                inputs (
                                  (Named (uid 14))
                                  (Named (uid 11)))))))))))))
                (statefulness Stateless_from)))
              (statefulness Stateless_from)))
            (statefulness Stateless_from)))
          (statefulness Stateless_into))
        ((1 0) (2 0))
        dynamic action
        ((1 0) (2 3))
        ((1 0))
        ("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
         (key 2) (action 4))
        static action
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ====
        -5,2 +5,2
        -|("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
        -| (key 2) (action 4))
        +|("an action inside of Bonsai.assoc_on has been dropped because the computation is no longer active"
        +| (io_key 2) (model_key 2) (action 4)) |}]
    ;;

    let%expect_test "race inactive-delivery (but an active input)" =
      (fun input ->
         Bonsai.Expert.race
           (module Int)
           (module Int)
           ~default_model:0
           ~apply_action:(fun ~inject:_ ~schedule_event:_ input _model new_model ->
             (match input with
              | Inactive -> print_endline "static action"
              | Active () -> print_endline "dynamic action");
             new_model)
           input)
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Leaf01 (input (Constant (id 0))) name))
          (via 5)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 5)))))))
            (via 8)
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid 5)))))))
              (via 11)
              (into (
                Sub
                (from (Return (value (Mapn (inputs (Named (uid 5)))))))
                (via 14)
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          Mapn (
                            inputs (
                              (Named (uid 8))
                              (Mapn (
                                inputs (
                                  (Named (uid 14))
                                  (Named (uid 11)))))))))))))
                (statefulness Stateless_from)))
              (statefulness Stateless_from)))
            (statefulness Stateless_from)))
          (statefulness Stateless_into))
        ((1 0) (2 0))
        dynamic action
        ((1 0) (2 3))
        ((1 0))
        ("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
         (key 2) (action 4))
        static action
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ====
        -5,2 +5,2
        -|("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
        -| (key 2) (action 4))
        +|("an action inside of Bonsai.assoc_on has been dropped because the computation is no longer active"
        +| (io_key 2) (model_key 2) (action 4)) |}]
    ;;

    let%expect_test "dynamic action inactive-delivery" =
      (fun _ ->
         Bonsai.state_machine1
           (module Int)
           (module Int)
           ~default_model:0
           ~apply_action:(fun ~inject:_ ~schedule_event:_ () _model new_model -> new_model)
           (opaque_const_value ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Leaf1 (input Incr) name)
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
         (key 2) (action 4))
        ((1 0))
        ((1 0) (2 3))

        ==== Diff between assoc and assoc_on: ====
        -4,2 +4,2
        -|("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
        -| (key 2) (action 4))
        +|("an action inside of Bonsai.assoc_on has been dropped because the computation is no longer active"
        +| (io_key 2) (model_key 2) (action 4)) |}]
    ;;

    let%expect_test "actor1 inactive-delivery" =
      (fun _ ->
         Bonsai.actor1
           (module Int)
           (module Int)
           ~default_model:0
           ~recv:(fun ~schedule_event:_ () _model new_model -> new_model, ())
           (opaque_const_value ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Leaf1 (input Incr) name))
          (via 4)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 4)))))))
            (via 7)
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid 4)))))))
              (via 10)
              (into (
                Sub
                (from (Return (value (Mapn (inputs (Named (uid 10)))))))
                (via 13)
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          Mapn (
                            inputs (
                              (Named (uid 7))
                              (Named (uid 13))))))))))
                (statefulness Stateless_from)))
              (statefulness Stateless_from)))
            (statefulness Stateless_from)))
          (statefulness Stateless_into))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
         (key 2) (action 4))
        ((1 0))
        ((1 0) (2 3))

        ==== Diff between assoc and assoc_on: ====
        -4,2 +4,2
        -|("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
        -| (key 2) (action 4))
        +|("an action inside of Bonsai.assoc_on has been dropped because the computation is no longer active"
        +| (io_key 2) (model_key 2) (action 4)) |}]
    ;;

    let%expect_test "actor0 inactive-delivery" =
      (fun _ ->
         Bonsai.actor0
           (module Int)
           (module Int)
           ~default_model:0
           ~recv:(fun ~schedule_event:_ _model new_model -> new_model, ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Leaf0 name))
          (via 0)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 0)))))))
            (via 3)
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid 0)))))))
              (via 6)
              (into (
                Sub
                (from (Return (value (Mapn (inputs (Named (uid 6)))))))
                (via 9)
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          Mapn (
                            inputs (
                              (Named (uid 3))
                              (Named (uid 9))))))))))
                (statefulness Stateless_from)))
              (statefulness Stateless_from)))
            (statefulness Stateless_from)))
          (statefulness Stateless_into))
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
           (module Int)
           (module Int)
           ~default_model:0
           ~recv:(fun ~schedule_event:_ () _model new_model -> new_model, ())
           (Value.return ()))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Leaf0 name))
          (via 0)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 0)))))))
            (via 3)
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid 0)))))))
              (via 6)
              (into (
                Sub
                (from (Return (value (Mapn (inputs (Named (uid 6)))))))
                (via 9)
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          Mapn (
                            inputs (
                              (Named (uid 3))
                              (Named (uid 9))))))))))
                (statefulness Stateless_from)))
              (statefulness Stateless_from)))
            (statefulness Stateless_from)))
          (statefulness Stateless_into))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "dynamic action stat_machine01 inactive-delivery" =
      (fun _ ->
         let%sub m, i, _ =
           Bonsai.Expert.state_machine01
             (module Int)
             (module Int)
             (module Nothing)
             ~default_model:0
             ~apply_dynamic:
               (fun ~inject_dynamic:_
                 ~inject_static:_
                 ~schedule_event:_
                 ()
                 _model
                 new_model -> new_model)
             ~apply_static:
               (fun ~inject_dynamic:_ ~inject_static:_ ~schedule_event:_ _model ->
                  Nothing.unreachable_code)
             (Value.return ())
         in
         let%arr m = m
         and i = i in
         m, i)
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Leaf01 (input (Constant (id 0))) name))
          (via 5)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 5)))))))
            (via 8)
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid 5)))))))
              (via 11)
              (into (
                Return (
                  value (
                    Mapn (
                      inputs (
                        Mapn (
                          inputs (
                            (Named (uid 8))
                            (Named (uid 11))))))))))
              (statefulness Stateless_from)))
            (statefulness Stateless_from)))
          (statefulness Stateless_into))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
         (key 2) (action 4))
        ((1 0))
        ((1 0) (2 3))

        ==== Diff between assoc and assoc_on: ====
        -4,2 +4,2
        -|("an action inside of Bonsai.assoc has been dropped because the computation is no longer active"
        -| (key 2) (action 4))
        +|("an action inside of Bonsai.assoc_on has been dropped because the computation is no longer active"
        +| (io_key 2) (model_key 2) (action 4)) |}]
    ;;

    let%expect_test "static action stat_machine01 inactive-delivery" =
      (fun _ ->
         let%sub m, _, i =
           Bonsai.Expert.state_machine01
             (module Int)
             (module Nothing)
             (module Int)
             ~default_model:0
             ~apply_dynamic:
               (fun ~inject_dynamic:_ ~inject_static:_ ~schedule_event:_ () _model ->
                  Nothing.unreachable_code)
             ~apply_static:
               (fun ~inject_dynamic:_ ~inject_static:_ ~schedule_event:_ _model new_model ->
                  new_model)
             (Value.return ())
         in
         let%arr m = m
         and i = i in
         m, i)
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Leaf01 (input (Constant (id 0))) name))
          (via 5)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 5)))))))
            (via 8)
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid 5)))))))
              (via 11)
              (into (
                Return (
                  value (
                    Mapn (
                      inputs (
                        Mapn (
                          inputs (
                            (Named (uid 8))
                            (Named (uid 11))))))))))
              (statefulness Stateless_from)))
            (statefulness Stateless_from)))
          (statefulness Stateless_into))
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static action inactive-delivery" =
      (fun _ -> Bonsai.state (module Int) ~default_model:0)
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Leaf0 name)
        ((1 0) (2 0))
        ((1 0) (2 3))
        ((1 0))
        ((1 0))
        ((1 0) (2 4))

        ==== Diff between assoc and assoc_on: ==== |}]
    ;;

    let%expect_test "static inside of a lazy" =
      (fun _ -> Bonsai.lazy_ (lazy (Bonsai.state (module Int) ~default_model:0)))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Lazy t)
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
           (module Unit)
           ~default_model:()
           ~apply_action:(fun ~inject:_ ~schedule_event:_ _ () () -> ())
           ~f:(fun _model _inject -> Bonsai.state (module Int) ~default_model:0))
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Wrap
          (model_id  0)
          (inject_id 2)
          (inner (Leaf0 name)))
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
         | () -> Bonsai.state (module Int) ~default_model:0)
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (Return (value Incr)))
          (via 2)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 2)))))))
            (via 5)
            (into (Leaf0 name))
            (statefulness Stateless_from)))
          (statefulness Stateless_from))
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
           Bonsai.with_model_resetter (Bonsai.state (module Int) ~default_model:0)
         in
         return r)
      |> test_delivery_to_inactive_component;
      [%expect
        {|
        (Sub
          (from (With_model_resetter (t (Leaf0 name))))
          (via 0)
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Named (uid 0)))))))
            (via 3)
            (into (
              Sub
              (from (Return (value (Mapn (inputs (Named (uid 0)))))))
              (via 6)
              (into (Return (value (Named (uid 3)))))
              (statefulness Stateless_from)))
            (statefulness Stateless_from)))
          (statefulness Stateless_into))
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
        then Bonsai.with_model_resetter (Bonsai.state (module Int) ~default_model:0)
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
      let (_, set_value), reset = Handle.result handle in
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
              (module Int)
              (module Int)
              ~default_model:0
              ~apply_action:(fun ~inject:_ ~schedule_event:_ () _model new_model ->
                new_model)
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
      let result = Handle.result handle in
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
           (map       Incr)
           (io_key_id 7)
           (data_id   8)
           (by (Leaf1 (input Incr) name)))
         ((1 0) (2 0))
         ((1 3) (2 3))
         ((1 3))
         ("an action inside of Bonsai.assoc_on has been dropped because the computation is no longer active"
          (io_key 2) (model_key ()) (action 4))
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
            let%sub v = Bonsai.state (module String) ~default_model:"hello" in
            return
            @@ let%map state, set_state = v in
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
      [%expect {|
         () |}];
      Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, (); 2, () ]);
      Handle.show_model handle;
      [%expect {|
         () |}];
      (* use the setter to re-establish the default *)
      Handle.do_actions handle [ 1, Set "test" ];
      Handle.show_model handle;
      [%expect {| ((1 test)) |}];
      Handle.do_actions handle [ 1, Set "hello" ];
      Handle.show_model handle;
      [%expect {|
         () |}]
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
    let (T { t = computation; model; apply_static = _; static_action; dynamic_action }) =
      reveal_computation computation
    in
    let T = Type_equal.Id.same_witness_exn Meta.Action.nothing static_action in
    let T = Type_equal.Id.same_witness_exn Meta.Action.nothing dynamic_action in
    let snapshot =
      eval
        ~environment:Environment.empty
        ~path:Path.empty
        ~clock:Ui_incr.clock
        ~inject_dynamic:Nothing.unreachable_code
        ~inject_static:Nothing.unreachable_code
        ~model:(Ui_incr.return model.default)
        computation
    in
    Snapshot.result snapshot |> Ui_incr.pack
  in
  let filename = Stdlib.Filename.temp_file "incr" "out" in
  Ui_incr.Packed.save_dot_to_file filename [ packed ];
  let dot_contents = In_channel.read_all filename in
  require
    [%here]
    ~if_false_then_print_s:(lazy [%message "No Map7 node found"])
    (String.is_substring dot_contents ~substring:"Map7");
  [%expect {| |}]
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

let%expect_test "on_display for updating a state (using on_change)" =
  let callback =
    Value.return (fun prev cur ->
      Ui_effect.print_s [%message "change!" (prev : int option) (cur : int)])
  in
  let component input = Bonsai.Edge.on_change' (module Int) ~callback input in
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
        (module Int)
        (module Unit)
        ~default_model:0
        ~recv:(fun ~schedule_event:_ v () -> v + 1, v)
    in
    return
    @@ let%map effect = effect in
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
      let clock_param =
        Ui_incr.Clock.create
          ~start:(Time_ns.of_span_since_epoch (Time_ns.Span.of_sec start))
          ()
      in
      Handle.create ~clock:clock_param (Result_spec.sexp (module Unit)) clock
    ;;

    let print_time handle =
      let clock = Handle.clock handle in
      let now =
        Ui_incr.Clock.now clock |> Time_ns.to_string_abs_parts ~zone:Time_float.Zone.utc
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
                  Bonsai.state (module Bool) ~default_model:true
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
                         (Effect.of_sync_fun (fun () -> print_endline "(state := true)"))
                           ()
                       in
                       let%bind.Effect () = set_state true in
                       print_sanitized_dropped_action_if_needed ())
                  in
                  Bonsai.const false
              in
              let start = Time_ns.of_span_since_epoch (Time_ns.Span.of_min 1.0) in
              let clock = Ui_incr.Clock.create ~start () in
              let handle =
                Handle.create (Result_spec.sexp (module Bool)) ~clock component
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
  end)
;;

let edge_poll_shared ~get_expect_output =
  let effect_tracker = Query_response_tracker.create () in
  let effect = Bonsai.Effect.For_testing.of_query_response_tracker effect_tracker in
  let var = Bonsai.Var.create "hello" in
  let component =
    Bonsai.Edge.Poll.effect_on_change
      (module String)
      (module String)
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
  let clock = Ui_incr.Clock.create ~start:Time_ns.epoch () in
  let component = Bonsai.Clock.now in
  let handle =
    Handle.create ~clock (Result_spec.sexp (module Time_ns.Alternate_sexp)) component
  in
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  Ui_incr.Clock.advance_clock_by clock (Time_ns.Span.of_sec 0.5);
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00.5Z" |}];
  Ui_incr.Clock.advance_clock_by clock (Time_ns.Span.of_sec 0.7);
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:01.2Z" |}]
;;

let%expect_test "Clock.now" =
  let clock = Ui_incr.Clock.create ~start:Time_ns.epoch () in
  let component =
    let%sub get_time = Bonsai.Clock.get_current_time in
    Bonsai.Edge.after_display
      (let%map get_time = get_time in
       let%bind.Effect now = get_time in
       Effect.print_s [%sexp (now : Time_ns.Alternate_sexp.t)])
  in
  let handle = Handle.create ~clock (Result_spec.sexp (module Unit)) component in
  Handle.recompute_view handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  Ui_incr.Clock.advance_clock_by clock (Time_ns.Span.of_sec 0.5);
  Handle.recompute_view handle;
  [%expect {| "1970-01-01 00:00:00.5Z" |}];
  Ui_incr.Clock.advance_clock_by clock (Time_ns.Span.of_sec 0.7);
  Handle.recompute_view handle;
  [%expect {| "1970-01-01 00:00:01.2Z" |}]
;;

let%expect_test "Clock.approx_now" =
  let clock = Ui_incr.Clock.create ~start:Time_ns.epoch () in
  let component = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.0) in
  let handle =
    Handle.create ~clock (Result_spec.sexp (module Time_ns.Alternate_sexp)) component
  in
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  Ui_incr.Clock.advance_clock_by clock (Time_ns.Span.of_sec 0.5);
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:00Z" |}];
  Ui_incr.Clock.advance_clock_by clock (Time_ns.Span.of_sec 0.7);
  Handle.show handle;
  [%expect {| "1970-01-01 00:00:01.2Z" |}]
;;

(* $MDX part-begin=chain-computation *)
let chain_computation =
  let%sub a = Bonsai.const "x" in
  let%sub b, set_b = Bonsai.state (module String) ~default_model:" " in
  let%sub c, set_c = Bonsai.state (module String) ~default_model:" " in
  let%sub d, set_d = Bonsai.state (module String) ~default_model:" " in
  let%sub () = Bonsai.Edge.on_change (module String) a ~callback:set_b in
  let%sub () = Bonsai.Edge.on_change (module String) b ~callback:set_c in
  let%sub () = Bonsai.Edge.on_change (module String) c ~callback:set_d in
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
    let%sub state, set_state = Bonsai.state (module Int) ~default_model:0 in
    let callback =
      let%map set_state = set_state in
      fun new_state -> set_state (new_state + 1)
    in
    let%sub () = Bonsai.Edge.on_change (module Int) state ~callback in
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
           @@ let%sub a = Bonsai.Dynamic_scope.lookup id_a in
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
  [@@deriving sexp_of, fields]
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
      (module String)
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
    let%sub state, set_state = Bonsai.state (module Int) ~default_model:0 in
    let%sub get_state = Bonsai_extra.yoink state in
    Bonsai_extra.exactly_once
      (let%map get_state = get_state
       and set_state = set_state in
       let%bind.Bonsai.Effect () = set_state 1 in
       let%bind.Bonsai.Effect s = get_state in
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

let%expect_test "freeze" =
  let var = Bonsai.Var.create "hello" in
  let component = Bonsai.freeze (module String) (Bonsai.Var.value var) in
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

let%expect_test "state_machine_dynamic_model" =
  let component =
    Bonsai_extra.state_machine0_dynamic_model
      (module String)
      (module String)
      ~model:
        (`Computed
           (Bonsai.Value.return (function
              | None -> "not set "
              | Some s -> sprintf "set %s" s)))
      ~apply_action:(fun ~inject:_ ~schedule_event:_ _model action -> action)
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
        Bonsai.Edge.on_change (module Sexp) (Bonsai.Var.value var) ~callback:inject
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
          (module Int)
          (module Int)
          ~default_model:0
          ~apply_action:(fun ~inject:_ ~schedule_event inject model action ->
            schedule_event (inject (model + action));
            action)
          inject_fix
      in
      let%sub (), inject2 =
        Bonsai.state_machine1
          (module Unit)
          (module Int)
          ~default_model:()
          ~apply_action:(fun ~inject:_ ~schedule_event state1 _model action ->
            schedule_event (Ui_effect.print_s [%message (state1 : int) (action : int)]);
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
  Handle.flush handle;
  [%expect {| ((state1 1) (action 1)) |}];
  Handle.do_actions handle [ 5 ];
  Handle.flush handle;
  [%expect {| ((state1 5) (action 6)) |}];
  Handle.do_actions handle [ 10 ];
  Handle.flush handle;
  [%expect {| ((state1 10) (action 15)) |}]
;;

let%expect_test "pipe" =
  let component =
    let%sub push_and_pop = Bonsai_extra.pipe (module String) in
    return
    @@ let%map push, pop = push_and_pop in
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
  Handle.flush handle;
  [%expect {| (pop a hello) |}];
  Handle.do_actions handle [ `Push "world" ];
  Handle.flush handle;
  [%expect {| |}];
  Handle.do_actions handle [ `Pop "b" ];
  Handle.flush handle;
  [%expect {| (pop b world) |}];
  Handle.do_actions handle [ `Pop "c" ];
  Handle.flush handle;
  [%expect {| |}];
  Handle.do_actions handle [ `Push "foo" ];
  Handle.flush handle;
  [%expect {| (pop c foo) |}];
  Handle.do_actions
    handle
    [ `Push "hello"; `Push "world"; `Push "foo"; `Pop "a"; `Pop "b"; `Pop "c" ];
  Handle.flush handle;
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

let%expect_test "scope_model" =
  let var = Bonsai.Var.create true in
  let component =
    Bonsai_extra.scope_model
      (module Bool)
      ~on:(Bonsai.Var.value var)
      (Bonsai.state (module String) ~default_model:"default")
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
    let%sub x, set_x = Bonsai.state (module Bool) ~default_model:true in
    match%sub x with
    | true ->
      let%sub (), inject =
        Bonsai.state_machine1
          (module Unit)
          (module Unit)
          ~default_model:()
          ~apply_action:(fun ~inject:_ ~schedule_event:_ () () () ->
            print_s [%message "injected"])
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
  [%expect
    {|
         ("an action inside of Bonsai.switch as been dropped because the computation is no longer active"
          (index 1) (action ()))
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
            (module String)
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
      [@@deriving fields]
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
