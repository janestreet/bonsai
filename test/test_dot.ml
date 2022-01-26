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

let%expect_test ("map7 dot file" [@tags "no-js"]) =
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
                  ┌───────┐┌───────┐
                  │ const ││ const │
                  └───────┘└───────┘
                    │        │
                    │        │
                    ▼        ▼
    ┌───────┐     ┌────────────────┐     ┌───────┐
    │ const │ ──▶ │                │ ◀── │ const │
    └───────┘     │      map7      │     └───────┘
    ┌───────┐     │                │     ┌───────┐
    │ const │ ──▶ │                │ ◀── │ const │
    └───────┘     └────────────────┘     └───────┘
                    │        ▲
                    │        │
                    ▼        │
                  ┌───────┐┌───────┐
                  │ read  ││ const │
                  └───────┘└───────┘ |}]
;;

let%expect_test ("map-10 dot file" [@tags "no-js"]) =
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
                  ┌───────┐     ┌───────┐
                  │ const │     │ const │
                  └───────┘     └───────┘
                    │             │
                    │             │
                    ▼             ▼
    ┌───────┐     ┌─────────────────────┐     ┌───────┐
    │ const │ ──▶ │        map4         │ ◀── │ const │
    └───────┘     └─────────────────────┘     └───────┘
                    │
                    │
                    ▼
    ┌───────┐     ┌─────────────────────┐     ┌───────┐
    │ const │ ──▶ │                     │ ◀── │ const │
    └───────┘     │        map7         │     └───────┘
    ┌───────┐     │                     │     ┌───────┐
    │ const │ ──▶ │                     │ ◀── │ const │
    └───────┘     └─────────────────────┘     └───────┘
                    │        ▲    ▲
                    │        │    │
                    ▼        │    │
                  ┌───────┐  │  ┌───────┐
                  │ read  │  │  │ const │
                  └───────┘  │  └───────┘
                             │
                    ┌────────┘
                    │
                  ┌───────┐
                  │ const │
                  └───────┘ |}]
;;

let%expect_test ("subst dot" [@tags "no-js"]) =
  let c =
    let%sub a = Bonsai.state dummy_source_code_position (module Int) ~default_model:0 in
    let%sub b = Bonsai.const () in
    let%sub c = return (Value.both a b) in
    return (Value.both a c)
  in
  print_graph c;
  [%expect
    {|
       ┌─────────────────┐
       │      const      │
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
       │      both       │ ◀┐
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
    ┌▶ │      both       │  │
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
  let c = Bonsai.const () |> Bonsai.with_model_resetter in
  print_endline (to_dot c);
  [%expect
    {|
    digraph {
    with_model_resetter_0 [ style=filled, shape = "Mrecord", label = "with_model_resetter"; fillcolor = "#86E3CE"; ]
    read_1 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
    const_2 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_2 -> read_1;
    read_1 -> with_model_resetter_0;
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
    │  map  │     │  subst  │
    └───────┘     └─────────┘
      │             │
      │             │
      ▼             ▼
    ┌───────┐     ┌─────────┐
    │ read  │     │   map   │
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
                  │  subst  │     │  map  │
                  └─────────┘     └───────┘
                    │               │
                    │               │
                    ▼               ▼
                  ┌─────────┐     ┌───────┐
                  │   map   │     │ read  │
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
    let%sub a = Bonsai.const "hi" in
    let%sub b = Bonsai.const 5 in
    let%arr a = a
    and b = b in
    sprintf "%s %d" a b
  in
  print_graph component;
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
    │ map2  │ ◀┐
    └───────┘  │
      │        │
      │        │
      ▼        │
    ┌───────┐  │
    │ read  │  │
    └───────┘  │
    ┌───────┐  │
    │ const │  │
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
