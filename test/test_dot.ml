open! Core
open! Import
module Bonsai_lib = Bonsai
open Bonsai_lib
open Bonsai.Let_syntax

let%expect_test "map7 dot file" =
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
  print_endline (Bonsai.Debug.to_dot c);
  [%expect
    {|
    digraph {
    read_0 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
    map_1 [ style=filled, shape = "oval", label = "map"; fillcolor = "#FFDD94"; ]
    const_2 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_3 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_4 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_5 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_6 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_7 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_8 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    map7_9 [ style=filled, shape = "oval", label = "map7"; fillcolor = "#FFDD94"; ]
    const_8 -> map7_9;
    const_7 -> map7_9;
    const_6 -> map7_9;
    const_5 -> map7_9;
    const_4 -> map7_9;
    const_3 -> map7_9;
    const_2 -> map7_9;
    map7_9 -> map_1;
    map_1 -> read_0;
    } |}]
;;

let%expect_test "subst dot" =
  let c =
    let%sub a = Bonsai.state dummy_source_code_position (module Int) ~default_model:0 in
    let%sub b = Bonsai.const () in
    let%sub c = return (Value.both a b) in
    return (Value.both a c)
  in
  print_endline (Bonsai.Debug.to_dot c);
  [%expect
    {|
      digraph {
      named_0 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
      leaf_1 [ style=filled, shape = "Mrecord", label = "{state|file_name.ml:0:0}"; fillcolor = "#D0E6A5"; ]
      leaf_1 -> named_0 [dir=none];
      named_2 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
      const_3 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
      const_3 -> named_2 [dir=none];
      named_4 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
      map2_5 [ style=filled, shape = "oval", label = "map2"; fillcolor = "#FFDD94"; ]
      named_0 -> map2_5;
      named_2 -> map2_5;
      map2_5 -> named_4 [dir=none];
      read_6 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
      map2_7 [ style=filled, shape = "oval", label = "map2"; fillcolor = "#FFDD94"; ]
      named_0 -> map2_7;
      named_4 -> map2_7;
      map2_7 -> read_6;
      }
    |}]
;;

let%expect_test "model_resetter doesn't have a dash in the label name" =
  let c = Bonsai.const () |> Bonsai.with_model_resetter in
  print_endline (Bonsai.Debug.to_dot c);
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

let%expect_test "dynamic scope" =
  let id = Bonsai.Dynamic_scope.create ~name:"my-id" ~fallback:"no" () in
  let c =
    Bonsai.Dynamic_scope.set id (Value.return "hello") ~f:(fun _ ->
      let%sub _ = Bonsai.Dynamic_scope.lookup id in
      let%sub _ = Bonsai.Dynamic_scope.lookup id in
      Bonsai.const ())
  in
  print_endline (Bonsai.Debug.to_dot c);
  [%expect
    {|
    digraph {
    named_0 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    named_1 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    fetch_2 [ style=filled, shape = "Mrecord", label = "fetch"; fillcolor = "#86E3CE"; ]
    named_3 [ style=filled, shape = "circle", label = ""; fillcolor = "#000000"; width=.1, height=.1]
    named_3 -> fetch_2;
    fetch_2 -> named_1 [dir=none];
    read_4 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
    map_5 [ style=filled, shape = "oval", label = "map"; fillcolor = "#FFDD94"; ]
    named_1 -> map_5;
    map_5 -> read_4;
    read_4 -> named_0 [dir=none];
    dyn_set_6 [ style=filled, shape = "Mrecord", label = "dyn_set"; fillcolor = "#86E3CE"; ]
    const_7 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_7 -> dyn_set_6;
    dyn_set_6 -> named_3 [dir=none];
    named_8 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    named_9 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    fetch_10 [ style=filled, shape = "Mrecord", label = "fetch"; fillcolor = "#86E3CE"; ]
    named_3 -> fetch_10;
    fetch_10 -> named_9 [dir=none];
    read_11 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
    map_12 [ style=filled, shape = "oval", label = "map"; fillcolor = "#FFDD94"; ]
    named_9 -> map_12;
    map_12 -> read_11;
    read_11 -> named_8 [dir=none];
    named_13 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    map_14 [ style=filled, shape = "oval", label = "map"; fillcolor = "#FFDD94"; ]
    named_8 -> map_14;
    map_14 -> named_13 [dir=none];
    named_15 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    named_16 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    fetch_17 [ style=filled, shape = "Mrecord", label = "fetch"; fillcolor = "#86E3CE"; ]
    named_3 -> fetch_17;
    fetch_17 -> named_16 [dir=none];
    read_18 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
    map_19 [ style=filled, shape = "oval", label = "map"; fillcolor = "#FFDD94"; ]
    named_16 -> map_19;
    map_19 -> read_18;
    read_18 -> named_15 [dir=none];
    named_20 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    map_21 [ style=filled, shape = "oval", label = "map"; fillcolor = "#FFDD94"; ]
    named_15 -> map_21;
    map_21 -> named_20 [dir=none];
    read_22 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
    const_23 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_23 -> read_22;
    } |}]
;;

let%expect_test "arrow-syntax" =
  let component =
    let%sub a = Bonsai.const "hi" in
    let%sub b = Bonsai.const 5 in
    let%arr a = a
    and b = b in
    sprintf "%s %d" a b
  in
  print_endline (Bonsai.Debug.to_dot component);
  [%expect
    {|
    digraph {
    named_0 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    const_1 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_1 -> named_0 [dir=none];
    named_2 [ style=filled, shape = "circle", label = ""; fillcolor = "#FFFFFF"; width=.1, height=.1]
    const_3 [ style=filled, shape = "oval", label = "const"; fillcolor = "#FFDD94"; ]
    const_3 -> named_2 [dir=none];
    read_4 [ style=filled, shape = "Mrecord", label = "read"; fillcolor = "#86E3CE"; ]
    map_5 [ style=filled, shape = "Mrecord", label = "{map|lib/bonsai/test/test_dot.ml:155:4}"; fillcolor = "#FFDD94"; ]
    map2_6 [ style=filled, shape = "oval", label = "map2"; fillcolor = "#FFDD94"; ]
    named_0 -> map2_6;
    named_2 -> map2_6;
    map2_6 -> map_5;
    map_5 -> read_4;
    } |}]
;;
