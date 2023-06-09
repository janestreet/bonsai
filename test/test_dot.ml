open! Core
open! Import
module Bonsai_lib = Bonsai
open Bonsai_lib
open Bonsai.Let_syntax

let line_number_regex = Re.Str.regexp ":[0-9]+:[0-9]+"
let to_dot c = c |> Bonsai.Debug.to_dot |> Re.Str.global_replace line_number_regex ""

let print_graph c =
  let content = to_dot c in
  let out = Stdlib.open_out "/tmp/foo" in
  Stdlib.output_string out content;
  Stdlib.flush out;
  assert (Stdlib.Sys.command "graph-easy /tmp/foo --from graphviz --as boxart" = 0)
;;

let%test_module ("regression" [@tags "no-js"]) =
  (module struct
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
      let _this_is_normal =
        let c = component (Value.map state ~f:State.b) in
        print_graph c;
        [%expect
          {|
          ┌──────┐
          │ incr │
          └──────┘
            │
            │
            ▼
          ┌──────┐
          │ mapn │
          └──────┘
            │
            │
            ▼
          ┌──────┐
          │ mapn │ ◀┐
          └──────┘  │
            │       │
            │       │
            ▼       │
          ┌──────┐  │
          │ read │  │
          └──────┘  │
          ┌──────┐  │
          │ incr │  │
          └──────┘  │
            │       │
            │       │
            ▼       │
          ┌──────┐  │
          │ mapn │ ─┘
          └──────┘ |}]
      in
      let _this_is_weird =
        let c = component (Value.return 3) in
        print_graph c;
        [%expect
          {|
          ┌──────┐
          │ incr │
          └──────┘
            │
            │
            ▼
          ┌──────┐
          │ mapn │
          └──────┘
            │
            │
            ▼
          ┌──────┐
          │ mapn │
          └──────┘
            │
            │
            ▼
          ┌──────┐
          │ mapn │
          └──────┘
            │
            │
            ▼
          ┌──────┐
          │ read │
          └──────┘ |}]
      in
      ()
    ;;
  end)
;;

let%expect_test ("map7 dot file" [@tags "no-js"]) =
  let c =
    Bonsai.read
      (let%map () = opaque_const_value ()
       and () = opaque_const_value ()
       and () = opaque_const_value ()
       and () = opaque_const_value ()
       and () = opaque_const_value ()
       and () = opaque_const_value ()
       and () = opaque_const_value () in
       ())
  in
  print_graph c;
  [%expect
    {|
                 ┌──────┐┌──────┐
                 │ incr ││ incr │
                 └──────┘└──────┘
                   │       │
                   │       │
                   ▼       ▼
    ┌──────┐     ┌──────────────┐     ┌──────┐
    │ incr │ ──▶ │              │ ◀── │ incr │
    └──────┘     │     mapn     │     └──────┘
    ┌──────┐     │              │     ┌──────┐
    │ incr │ ──▶ │              │ ◀── │ incr │
    └──────┘     └──────────────┘     └──────┘
                   │       ▲
                   │       │
                   ▼       │
                 ┌──────┐┌──────┐
                 │ read ││ incr │
                 └──────┘└──────┘ |}]
;;

let%expect_test ("map7 dot file constant folding" [@tags "no-js"]) =
  let c =
    Bonsai.read
      (let%map () = Value.return ()
       and () = Value.return ()
       and () = Value.return ()
       and () = Value.return ()
       and () = Value.return ()
       and () = Value.return ()
       and () = Value.return () in
       ())
  in
  print_graph c;
  [%expect
    {|
    ┌───────┐
    │ const │
    └───────┘
      │
      │
      ▼
    ┌───────┐
    │ read  │
    └───────┘ |}]
;;

let%expect_test ("map-10 dot file" [@tags "no-js"]) =
  let c =
    Bonsai.read
      (let%map _1 : unit Value.t = opaque_const_value ()
       and _2 : unit Value.t = opaque_const_value ()
       and _3 : unit Value.t = opaque_const_value ()
       and _4 : unit Value.t = opaque_const_value ()
       and _5 : unit Value.t = opaque_const_value ()
       and _6 : unit Value.t = opaque_const_value ()
       and _7 : unit Value.t = opaque_const_value ()
       and _8 : unit Value.t = opaque_const_value ()
       and _9 : unit Value.t = opaque_const_value ()
       and _10 : unit Value.t = opaque_const_value () in
       ())
  in
  print_graph c;
  [%expect
    {|
                 ┌──────┐     ┌──────┐
                 │ incr │     │ incr │
                 └──────┘     └──────┘
                   │            │
                   │            │
                   ▼            ▼
    ┌──────┐     ┌───────────────────┐     ┌──────┐
    │ incr │ ──▶ │                   │ ◀── │ incr │
    └──────┘     │       mapn        │     └──────┘
    ┌──────┐     │                   │     ┌──────┐
    │ incr │ ──▶ │                   │ ◀── │ incr │
    └──────┘     └───────────────────┘     └──────┘
                   │       ▲
                   │       │
                   ▼       │
                 ┌──────┐  │
                 │ read │  │
                 └──────┘  │
                 ┌──────┐  │
                 │ incr │  │
                 └──────┘  │
                   │       │
                   │       │
                   ▼       │
    ┌──────┐     ┌───────────────────┐
    │ incr │ ──▶ │       mapn        │
    └──────┘     └───────────────────┘
                   ▲            ▲
                   │            │
                   │            │
                 ┌──────┐     ┌──────┐
                 │ incr │     │ incr │
                 └──────┘     └──────┘ |}]
;;

let%expect_test ("map-10 dot file constant folding optimization" [@tags "no-js"]) =
  let c =
    Bonsai.read
      (let%map _1 : unit Value.t = Value.return ()
       and _2 : unit Value.t = Value.return ()
       and _3 : unit Value.t = Value.return ()
       and _4 : unit Value.t = Value.return ()
       and _5 : unit Value.t = Value.return ()
       and _6 : unit Value.t = Value.return ()
       and _7 : unit Value.t = Value.return ()
       and _8 : unit Value.t = Value.return ()
       and _9 : unit Value.t = Value.return ()
       and _10 : unit Value.t = Value.return () in
       ())
  in
  print_graph c;
  [%expect
    {|
    ┌───────┐
    │ const │
    └───────┘
      │
      │
      ▼
    ┌───────┐
    │ read  │
    └───────┘ |}]
;;

let%expect_test ("subst dot constant folding" [@tags "no-js"]) =
  let c =
    let%sub a = Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t] in
    let%sub b = opaque_const () in
    let%sub c = return (Value.both a b) in
    return (Value.both a c)
  in
  print_graph c;
  [%expect
    {|
       ┌─────────────────┐
       │      incr       │
       └─────────────────┘
         │
         │
         ▼
       ┌─────────────────┐
       │      read       │
       └─────────────────┘
         │
         │
         │
       ┌─────────────────┐
       │      subst      │
       └─────────────────┘
         │
         │
         ▼
       ┌─────────────────┐
       │      mapn       │ ◀┐
       └─────────────────┘  │
         │                  │
         │                  │
         ▼                  │
       ┌─────────────────┐  │
       │      read       │  │
       └─────────────────┘  │
         │                  │
         │                  │
         │                  │
       ┌─────────────────┐  │
       │      subst      │  │
       └─────────────────┘  │
         │                  │
         │                  │
         ▼                  │
       ┌─────────────────┐  │
    ┌▶ │      mapn       │  │
    │  └─────────────────┘  │
    │    │                  │
    │    │                  │
    │    ▼                  │
    │  ┌─────────────────┐  │
    │  │      read       │  │
    │  └─────────────────┘  │
    │  ┌─────────────────┐  │
    │  │ {state machine} │  │
    │  └─────────────────┘  │
    │    │                  │
    │    │                  │
    │    │                  │
    │  ┌─────────────────┐  │
    └─ │      subst      │ ─┘
       └─────────────────┘ |}]
;;

let%expect_test ("subst dot" [@tags "no-js"]) =
  let c =
    let%sub a = Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t] in
    let%sub b = opaque_const () in
    let%sub c = return (Value.both a b) in
    return (Value.both a c)
  in
  print_graph c;
  [%expect
    {|
       ┌─────────────────┐
       │      incr       │
       └─────────────────┘
         │
         │
         ▼
       ┌─────────────────┐
       │      read       │
       └─────────────────┘
         │
         │
         │
       ┌─────────────────┐
       │      subst      │
       └─────────────────┘
         │
         │
         ▼
       ┌─────────────────┐
       │      mapn       │ ◀┐
       └─────────────────┘  │
         │                  │
         │                  │
         ▼                  │
       ┌─────────────────┐  │
       │      read       │  │
       └─────────────────┘  │
         │                  │
         │                  │
         │                  │
       ┌─────────────────┐  │
       │      subst      │  │
       └─────────────────┘  │
         │                  │
         │                  │
         ▼                  │
       ┌─────────────────┐  │
    ┌▶ │      mapn       │  │
    │  └─────────────────┘  │
    │    │                  │
    │    │                  │
    │    ▼                  │
    │  ┌─────────────────┐  │
    │  │      read       │  │
    │  └─────────────────┘  │
    │  ┌─────────────────┐  │
    │  │ {state machine} │  │
    │  └─────────────────┘  │
    │    │                  │
    │    │                  │
    │    │                  │
    │  ┌─────────────────┐  │
    └─ │      subst      │ ─┘
       └─────────────────┘ |}]
;;

let%expect_test "model_resetter doesn't have a dash in the label name" =
  let c = Bonsai.with_model_resetter (Bonsai.const ()) in
  print_endline (to_dot c);
  [%expect
    {|
    digraph {
    with_model_resetter_0 [ style=filled, shape = "Mrecord", label = "with_model_resetter"; fillcolor = "#86E3CE"; ]
    named_1 [ style=filled, shape = "circle", label = ""; fillcolor = "#000000"; width=.1, height=.1]
    with_model_resetter_0 -> named_1 [dir=none];
    read_2 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
    mapn_3 [ style=filled, shape = "oval", label = "mapn"; fillcolor = "#FFDD94"; ]
    named_1 -> mapn_3;
    mapn_3 -> read_2;
    read_2 -> with_model_resetter_0;
    } |}]
;;

let%expect_test ("dynamic scope" [@tags "no-js"]) =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let c =
    Bonsai.Dynamic_scope.set
      id
      (Value.return "hello")
      ~inside:
        (let%sub _ = Bonsai.Dynamic_scope.lookup id in
         let%sub _ = Bonsai.Dynamic_scope.lookup id in
         Bonsai.const ())
  in
  print_graph c;
  [%expect
    {|
                  ┌─────────┐
                  │  const  │
                  └─────────┘
                    │
                    │
                    ▼
                  ┌─────────┐
                  │ dyn_set │
                  └─────────┘
                    │
                    │
                    │
    ┌───────┐     ┌─────────┐
    │ fetch │ ◀── │ named_2 │
    └───────┘     └─────────┘
      │             │
      │             │
      │             ▼
    ┌───────┐     ┌─────────┐
    │ subst │     │  fetch  │
    └───────┘     └─────────┘
      │             │
      │             │
      ▼             │
    ┌───────┐     ┌─────────┐
    │ mapn  │     │  subst  │
    └───────┘     └─────────┘
      │             │
      │             │
      ▼             ▼
    ┌───────┐     ┌─────────┐
    │ read  │     │  mapn   │
    └───────┘     └─────────┘
      │             │
      │             │
      │             ▼
    ┌───────┐     ┌─────────┐
    │ subst │     │  read   │
    └───────┘     └─────────┘
                    │
                    │
                    │
                  ┌─────────┐
                  │  subst  │
                  └─────────┘
                  ┌─────────┐
                  │  const  │
                  └─────────┘
                    │
                    │
                    ▼
                  ┌─────────┐
                  │  read   │
                  └─────────┘ |}]
;;

let%expect_test ("dynamic scope (with reverter)" [@tags "no-js"]) =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let c =
    Bonsai.Dynamic_scope.set' id (Value.return "hello") ~f:(fun _ ->
      let%sub _ = Bonsai.Dynamic_scope.lookup id in
      let%sub _ = Bonsai.Dynamic_scope.lookup id in
      Bonsai.const ())
  in
  print_graph c;
  [%expect
    {|
                  ┌─────────┐
                  │  const  │
                  └─────────┘
                    │
                    │
                    ▼
                  ┌─────────┐
                  │  read   │
                  └─────────┘
                  ┌─────────┐
                  │  const  │
                  └─────────┘
                    │
                    │
                    ▼
                  ┌─────────┐
                  │ dyn_set │
                  └─────────┘
                    │
                    │
                    │
    ┌───────┐     ┌─────────┐     ┌───────┐
    │ fetch │ ◀── │ named_2 │ ──▶ │ fetch │
    └───────┘     └─────────┘     └───────┘
      │             │               │
      │             │               │
      │             ▼               │
    ┌───────┐     ┌─────────┐     ┌───────┐
    │ subst │     │  fetch  │     │ subst │
    └───────┘     └─────────┘     └───────┘
                    │               │
                    │               │
                    │               ▼
                  ┌─────────┐     ┌───────┐
                  │  subst  │     │ mapn  │
                  └─────────┘     └───────┘
                    │               │
                    │               │
                    ▼               ▼
                  ┌─────────┐     ┌───────┐
                  │  mapn   │     │ read  │
                  └─────────┘     └───────┘
                    │               │
                    │               │
                    ▼               │
                  ┌─────────┐     ┌───────┐
                  │  read   │     │ subst │
                  └─────────┘     └───────┘
                    │
                    │
                    │
                  ┌─────────┐
                  │  subst  │
                  └─────────┘ |}]
;;

let%expect_test ("arrow-syntax" [@tags "no-js"]) =
  let component =
    let%sub a = opaque_const "hi" in
    let%sub b = opaque_const 5 in
    let%arr a = a
    and b = b in
    sprintf "%s %d" a b
  in
  print_graph component;
  [%expect
    {|
    ┌───────┐
    │ incr  │
    └───────┘
      │
      │
      ▼
    ┌───────┐
    │ read  │
    └───────┘
      │
      │
      │
    ┌───────┐
    │ subst │
    └───────┘
      │
      │
      ▼
    ┌───────┐
    │ mapn  │ ◀┐
    └───────┘  │
      │        │
      │        │
      ▼        │
    ┌───────┐  │
    │ read  │  │
    └───────┘  │
    ┌───────┐  │
    │ incr  │  │
    └───────┘  │
      │        │
      │        │
      ▼        │
    ┌───────┐  │
    │ read  │  │
    └───────┘  │
      │        │
      │        │
      │        │
    ┌───────┐  │
    │ subst │ ─┘
    └───────┘ |}]
;;

let%expect_test ("both-constant-opt" [@tags "no-js"]) =
  print_graph
    (let%arr a = Bonsai.Value.return 1
     and b = Bonsai.Value.return 1 in
     sprintf "%d %d" a b);
  [%expect
    {|
    ┌───────┐
    │ const │
    └───────┘
      │
      │
      ▼
    ┌───────┐
    │ read  │
    └───────┘ |}];
  print_graph
    (let%arr a = opaque_const_value 1
     and b = Bonsai.Value.return 1 in
     sprintf "%d %d" a b);
  [%expect
    {|
    ┌──────┐
    │ incr │
    └──────┘
      │
      │
      ▼
    ┌──────┐
    │ mapn │
    └──────┘
      │
      │
      ▼
    ┌──────┐
    │ mapn │
    └──────┘
      │
      │
      ▼
    ┌──────┐
    │ read │
    └──────┘ |}];
  print_graph
    (let%arr a = Bonsai.Value.return 1
     and b = opaque_const_value 1 in
     sprintf "%d %d" a b);
  [%expect
    {|
    ┌──────┐
    │ incr │
    └──────┘
      │
      │
      ▼
    ┌──────┐
    │ mapn │
    └──────┘
      │
      │
      ▼
    ┌──────┐
    │ mapn │
    └──────┘
      │
      │
      ▼
    ┌──────┐
    │ read │
    └──────┘ |}]
;;
