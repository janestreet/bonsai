open! Core_kernel
open! Import
module Bonsai_lib = Bonsai
open Bonsai.Let_syntax

let%expect_test "map7 dot file" =
  let c =
    Bonsai.read
      (let%map () = Bonsai.Value.return ()
       and () = Bonsai.Value.return ()
       and () = Bonsai.Value.return ()
       and () = Bonsai.Value.return ()
       and () = Bonsai.Value.return ()
       and () = Bonsai.Value.return ()
       and () = Bonsai.Value.return () in
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
    let%sub c = return (Bonsai.Value.both a b) in
    return (Bonsai.Value.both a c)
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
