open! Core
open! Import
module Bonsai_lib = Bonsai
open Bonsai_lib
open Proc
open Bonsai.Let_syntax
module Query_response_tracker = Bonsai.Effect.For_testing.Query_response_tracker

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
    let%sub state, set_state = Bonsai.state [%here] (module Int) ~default_model:0 in
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
    let%sub state, set_state = Bonsai.state_opt [%here] (module Int) in
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
    let%sub () = Bonsai.const () in
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
    ((-1 (Subst_from (Assoc -1) Subst_into (Enum 0)))
     (1 (Subst_from (Assoc 1) Subst_into (Enum 1)))) |}]
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
  print_s Bonsai_lib.Private.(Computation.sexp_of_packed (reveal_computation component));
  [%expect {| (Assoc_simpl ((map constant))) |}]
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

let%expect_test "let%sub unit rhs optimizaiton" =
  let component =
    let%sub a = Bonsai.const 5 in
    let%sub b = Bonsai.const 6 in
    return
      (let%map a = a
       and b = b in
       a + b)
  in
  print_s Bonsai_lib.Private.(Computation.sexp_of_packed (reveal_computation component));
  [%expect
    {|
    (Subst_stateless (
      (from (Return constant))
      (via lib/bonsai/src/proc.ml:13:63)
      (into (
        Subst_stateless (
          (from (Return constant))
          (via lib/bonsai/src/proc.ml:13:63)
          (into (
            Return (
              map (
                t (
                  both
                  (t1 (named lib/bonsai/src/proc.ml:13:63))
                  (t2 (named lib/bonsai/src/proc.ml:13:63)))))))
          (here None))))
      (here None))) |}]
;;

let%expect_test "assoc simplifies its inner computation, if possible" =
  let value = Value.return String.Map.empty in
  let component =
    Bonsai.assoc
      (module String)
      value
      ~f:(fun key data -> Bonsai.read (Value.both key data))
  in
  print_s Bonsai_lib.Private.(Computation.sexp_of_packed (reveal_computation component));
  [%expect {|
    (Assoc_simpl ((map constant))) |}]
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
  print_s Bonsai_lib.Private.(Computation.sexp_of_packed (reveal_computation component));
  [%expect {|
    (Assoc_simpl ((map constant))) |}]
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
  print_s Bonsai_lib.Private.(Computation.sexp_of_packed (reveal_computation component));
  [%expect {|
    (Assoc_simpl ((map constant))) |}]
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

let%expect_test "action sent to non-existent assoc element" =
  let var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, () ]) in
  let component =
    Bonsai.assoc
      (module Int)
      (Bonsai.Var.value var)
      ~f:(fun _key _data -> Bonsai.state [%here] (module Int) ~default_model:0)
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
  [%expect {|
        ((1 0) (2 0)) |}];
  let result = Handle.result handle in
  let set_two what =
    result
    |> Fn.flip Map.find_exn 2
    |> Tuple2.get2
    |> Fn.flip ( @@ ) what
    |> Ui_effect.Expert.handle
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
    ("an action inside of Bonsai.assoc as been dropped because the computation is no longer active"
     (key 2) (action 4))
    ((1 0)) |}]
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
            let%sub v = Bonsai.state [%here] (module String) ~default_model:"hello" in
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
        (()
         ()) |}];
      Bonsai.Var.set var (Int.Map.of_alist_exn [ 1, (); 2, () ]);
      Handle.show_model handle;
      [%expect {|
        (()
         ()) |}];
      (* use the setter to re-establish the default *)
      Handle.do_actions handle [ 1, Set "test" ];
      Handle.show_model handle;
      [%expect {| (((1 (test ()))) ()) |}];
      Handle.do_actions handle [ 1, Set "hello" ];
      Handle.show_model handle;
      [%expect {|
        (()
         ()) |}]
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
  let value =
    let%map () = Value.return ()
    and () = Value.return ()
    and () = Value.return ()
    and () = Value.return ()
    and () = Value.return ()
    and () = Value.return ()
    and () = Value.return () in
    ()
  in
  let packed =
    let open Bonsai.Private in
    value |> reveal_value |> Value.eval Environment.empty |> Ui_incr.pack
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
  let component input = Bonsai.Edge.on_change' [%here] (module Int) ~callback input in
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
        [%here]
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
  [%expect {|
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

let%expect_test "Clock.every" =
  let print_hi = (fun () -> print_endline "hi") |> Bonsai.Effect.of_sync_fun in
  let component =
    let%sub () =
      Bonsai.Clock.every [%here] (Time_ns.Span.of_sec 3.0) (Value.return (print_hi ()))
    in
    Bonsai.const ()
  in
  let handle = Handle.create (Result_spec.sexp (module Unit)) component in
  let move_forward_and_show () =
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    Handle.show handle
  in
  Handle.show handle;
  [%expect {|
    ()
    hi |}];
  move_forward_and_show ();
  [%expect {| () |}];
  move_forward_and_show ();
  [%expect {| () |}];
  move_forward_and_show ();
  [%expect {|
     ()
     hi |}]
;;

let edge_poll_shared ~get_expect_output =
  let effect_tracker = Query_response_tracker.create () in
  let effect = Bonsai.Effect.For_testing.of_query_response_tracker effect_tracker in
  let var = Bonsai.Var.create "hello" in
  let component =
    Bonsai.Edge.Poll.effect_on_change
      [%here]
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
  let%sub b, set_b = Bonsai.state [%here] (module String) ~default_model:" " in
  let%sub c, set_c = Bonsai.state [%here] (module String) ~default_model:" " in
  let%sub d, set_d = Bonsai.state [%here] (module String) ~default_model:" " in
  let%sub () = Bonsai.Edge.on_change [%here] (module String) a ~callback:set_b in
  let%sub () = Bonsai.Edge.on_change [%here] (module String) b ~callback:set_c in
  let%sub () = Bonsai.Edge.on_change [%here] (module String) c ~callback:set_d in
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
    let%sub state, set_state = Bonsai.state [%here] (module Int) ~default_model:0 in
    let callback =
      let%map set_state = set_state in
      fun new_state -> set_state (new_state + 1)
    in
    let%sub () = Bonsai.Edge.on_change [%here] (module Int) state ~callback in
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
    Bonsai.Dynamic_scope.set id (Value.return "hello") ~f:(fun _ ->
      Bonsai.Dynamic_scope.lookup id)
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
      Bonsai.Dynamic_scope.set id (Value.return "hello") ~f:(fun _ ->
        Bonsai.Dynamic_scope.lookup id)
    in
    let%sub b =
      Bonsai.Dynamic_scope.set id (Value.return "world") ~f:(fun _ -> Bonsai.read a)
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
    Bonsai.Dynamic_scope.set id (Value.return "hello") ~f:(fun _ -> return find)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| no |}]
;;

let%expect_test "use resetter" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let component =
    Bonsai.Dynamic_scope.set id (Value.return "hello") ~f:(fun { revert } ->
      revert (Bonsai.Dynamic_scope.lookup id))
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| no |}]
;;

let%expect_test "nested resetter" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let component =
    Bonsai.Dynamic_scope.set id (Value.return "hello") ~f:(fun _ ->
      Bonsai.Dynamic_scope.set id (Value.return "world") ~f:(fun { revert } ->
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
    Bonsai.Dynamic_scope.set id_a (Value.return "hello") ~f:(fun { revert } ->
      Bonsai.Dynamic_scope.set id_b (Value.return "world") ~f:(fun _ ->
        revert
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
    Bonsai.Dynamic_scope.set a (Value.return "hello") ~f:(fun _ ->
      Bonsai.Dynamic_scope.lookup id)
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
    Bonsai.Dynamic_scope.set a (Value.return "hello") ~f:(fun { revert } ->
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
    Bonsai.Dynamic_scope.set a (Value.return "hello") ~f:(fun _ ->
      Bonsai.Dynamic_scope.set a (Value.return "world") ~f:(fun { revert } ->
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
    Bonsai.Dynamic_scope.set a (Value.return "hello") ~f:(fun { revert } ->
      Bonsai.Dynamic_scope.set b (Value.return 1000) ~f:(fun _ ->
        revert (Bonsai.Dynamic_scope.lookup id)))
  in
  let handle = Handle.create (Result_spec.sexp (module M)) component in
  Handle.show handle;
  [%expect {| ((a hi) (b 1000)) |}]
;;

let%expect_test "exactly once" =
  let component =
    Bonsai_extra.exactly_once
      [%here]
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
      [%here]
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
    let%sub state, set_state = Bonsai.state [%here] (module Int) ~default_model:0 in
    let%sub get_state = Bonsai_extra.yoink state in
    Bonsai_extra.exactly_once
      [%here]
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
  let component = Bonsai_extra.freeze [%here] (module String) (Bonsai.Var.value var) in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| hello |}];
  Bonsai.Var.set var "world";
  Handle.show handle;
  [%expect {| hello |}]
;;

let%expect_test "id_gen" =
  let module Id = Bonsai_extra.Id_gen (Int) () in
  let component =
    let%sub next = Id.component [%here] in
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
      [%here]
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
        Bonsai.Edge.on_change
          [%here]
          (module Sexp)
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
          [%here]
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
          [%here]
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
    let%sub push_and_pop = Bonsai_extra.pipe [%here] (module String) in
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
    Bonsai_extra.thunk (fun () ->
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
    "0 1" |}]
;;

let%expect_test "scope_model" =
  let var = Bonsai.Var.create true in
  let component =
    Bonsai_extra.scope_model
      (module Bool)
      ~on:(Bonsai.Var.value var)
      (Bonsai.state [%here] (module String) ~default_model:"default")
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
    Bonsai_extra.thunk (fun () ->
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

module _ = struct
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
          [%here]
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
end
