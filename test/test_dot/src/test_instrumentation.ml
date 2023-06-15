module Bonsai_lib = Bonsai
open! Bonsai_lib
open! Bonsai.Let_syntax
open! Core
open Bonsai_test
open Bonsai.Private.Instrumentation
module Node_path = Bonsai.Private.Node_path
module Graph_info = Bonsai.Private.Graph_info

let start_timer label = print_endline [%string "start-%{label}"]
let stop_timer label = print_endline [%string "stop-%{label}"]

(* Each test sets this ref to the location at which the test began, so that
   test output can be made resilient to adding or remove lines above the test. *)
let test_location_reference_point = ref [%here]
let test_start here = test_location_reference_point := here
let this_file = [%here].pos_fname

let add_graph_info_nodes buffer graph_info =
  Map.iteri graph_info.Graph_info.info ~f:(fun ~key ~data ->
    let node_type = data.Graph_info.Node_info.node_type in
    let node_id = [%string "\"%{Node_path.to_string key}\""] in
    Buffer.add_string
      buffer
      [%string
        "  %{node_id} [ style=filled, shape=\"oval\", label = \"%{node_type}\"; \
         fillcolor = \"#FFDD94\"; ]\n"])
;;

let graph_info_to_dot filename graph_info =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "digraph {\n";
  add_graph_info_nodes buffer graph_info;
  Map.iteri graph_info.Graph_info.dag ~f:(fun ~key:from ~data:to_ ->
    List.iter to_ ~f:(fun to_ ->
      let from = [%string "\"%{Node_path.to_string from}\""] in
      let to_ = [%string "\"%{Node_path.to_string to_}\""] in
      Buffer.add_string buffer [%string "  %{from} -> %{to_};\n"]));
  Buffer.add_string buffer "}\n";
  Out_channel.write_all filename ~data:(Buffer.contents buffer)
;;

let print_graph_info (graph_info : Graph_info.t) =
  let with_line_and_column_string id =
    let id_string = Node_path.to_string id in
    match Map.find graph_info.info id with
    | Some { node_type; here } ->
      (match here with
       | Some { Source_code_position.pos_lnum; pos_cnum; pos_bol; pos_fname } ->
         let pos_lnum, pos_fname =
           if String.equal this_file pos_fname
           then pos_lnum - !test_location_reference_point.pos_lnum, ""
           else pos_lnum, pos_fname ^ ":"
         in
         [%string
           "%{id_string} %{node_type} @ %{pos_fname}%{pos_lnum#Int}:%{pos_cnum - \
            pos_bol#Int}"]
       | None -> [%string "%{id_string} %{node_type}"])
    | _ -> id_string
  in
  print_endline "tree:";
  Map.iteri graph_info.tree ~f:(fun ~key:from ~data:to_ ->
    let from = with_line_and_column_string from in
    let to_ = with_line_and_column_string to_ in
    print_endline [%string "  %{from} -> %{to_}"]);
  print_endline "dag:";
  Map.iteri graph_info.dag ~f:(fun ~key:from ~data:to_ ->
    let from = with_line_and_column_string from in
    let to_ = List.map to_ ~f:with_line_and_column_string |> String.concat ~sep:", " in
    print_endline [%string "  %{from} -> %{to_}"])
;;

let write_computation_to_dot filename component =
  let component = Private.reveal_computation component in
  let component = Private.pre_process component in
  let graph_info = ref Graph_info.empty in
  let (_ : _ Private.Computation.t) =
    Graph_info.iter_graph_updates component ~on_update:(fun gm -> graph_info := gm)
  in
  graph_info_to_dot filename !graph_info
;;

let instrument_computation component =
  let graph_info = ref Graph_info.empty in
  let print_graph_info_on_update = ref false in
  let c =
    Graph_info.iter_graph_updates
      (Bonsai.Private.reveal_computation component)
      ~on_update:(fun gm ->
        graph_info := gm;
        if !print_graph_info_on_update then print_graph_info !graph_info)
    |> instrument_computation ~start_timer ~stop_timer
    |> Bonsai.Private.conceal_computation
  in
  print_graph_info !graph_info;
  print_graph_info_on_update := true;
  c
;;

let many_aliases () =
  test_start [%here];
  let%sub a = opaque_const 1 in
  let%sub a = return a in
  let%sub a = return a in
  (* Even with an extra subst in here to break up the chain, the optimization
     still removes all the aliases. *)
  let%sub _ = opaque_const 1 in
  let%sub a = return a in
  let%sub a = return a in
  let%sub a = return a in
  return a
;;

let%expect_test _ =
  let (_ : int Computation.t) = instrument_computation (many_aliases ()) in
  [%expect
    {|
     tree:
       1_1 return -> _1 sub @ 1:2
       1_2 incr -> 1_1 return
       2-1_1 return -> 2_1 sub @ 6:2
       2-1_2 incr -> 2-1_1 return
       2-2-1-1_1 named -> 2-2-1_2 map @ 6:2
       2-2-1_1 return -> 2-2_1 sub @ 6:2
       2-2-1_2 map @ 6:2 -> 2-2-1_1 return
       2-2-2_1 return -> 2-2_1 sub @ 6:2
       2-2-2_2 named @ 9:2 -> 2-2-2_1 return
       2-2_1 sub @ 6:2 -> 2_1 sub @ 6:2
       2_1 sub @ 6:2 -> _1 sub @ 1:2
       _1 sub @ 1:2 -> _0
     dag:
       1_1 return -> 2-2-2_2 named @ 9:2, _1 sub @ 1:2
       1_2 incr -> 1_1 return
       2-1_1 return -> 2-2-1-1_1 named, 2_1 sub @ 6:2
       2-1_2 incr -> 2-1_1 return
       2-2-1-1_1 named -> 2-2-1_2 map @ 6:2
       2-2-1_1 return -> 2-2_1 sub @ 6:2
       2-2-1_2 map @ 6:2 -> 2-2-1_1 return
       2-2-2_1 return -> 2-2_1 sub @ 6:2
       2-2-2_2 named @ 9:2 -> 2-2-2_1 return
       2-2_1 sub @ 6:2 -> 2_1 sub @ 6:2
       2_1 sub @ 6:2 -> _1 sub @ 1:2
       _1 sub @ 1:2 -> _0 |}]
;;

let many_aliases_constant_folding () =
  test_start [%here];
  let%sub a = Bonsai.const 5 in
  let%sub a = return a in
  let%sub a = return a in
  (* Even with an extra subst in here to break up the chain, the optimization
     still removes all the aliases. *)
  let%sub _ = Bonsai.const 6 in
  let%sub a = return a in
  let%sub a = return a in
  let%sub a = return a in
  return a
;;

let%expect_test _ =
  let (_ : int Computation.t) =
    instrument_computation (many_aliases_constant_folding ())
  in
  [%expect
    {|
   tree:
     1_1 return -> _1 sub @ 1:2
     1_2 constant -> 1_1 return
     2-1_1 return -> 2_1 sub @ 6:2
     2-1_2 constant -> 2-1_1 return
     2-2-1-1_1 named -> 2-2-1_2 map @ 6:2
     2-2-1_1 return -> 2-2_1 sub @ 6:2
     2-2-1_2 map @ 6:2 -> 2-2-1_1 return
     2-2-2_1 return -> 2-2_1 sub @ 6:2
     2-2-2_2 named @ 9:2 -> 2-2-2_1 return
     2-2_1 sub @ 6:2 -> 2_1 sub @ 6:2
     2_1 sub @ 6:2 -> _1 sub @ 1:2
     _1 sub @ 1:2 -> _0
   dag:
     1_1 return -> 2-2-2_2 named @ 9:2, _1 sub @ 1:2
     1_2 constant -> 1_1 return
     2-1_1 return -> 2-2-1-1_1 named, 2_1 sub @ 6:2
     2-1_2 constant -> 2-1_1 return
     2-2-1-1_1 named -> 2-2-1_2 map @ 6:2
     2-2-1_1 return -> 2-2_1 sub @ 6:2
     2-2-1_2 map @ 6:2 -> 2-2-1_1 return
     2-2-2_1 return -> 2-2_1 sub @ 6:2
     2-2-2_2 named @ 9:2 -> 2-2-2_1 return
     2-2_1 sub @ 6:2 -> 2_1 sub @ 6:2
     2_1 sub @ 6:2 -> _1 sub @ 1:2
     _1 sub @ 1:2 -> _0 |}]
;;

let subst_tree () =
  test_start [%here];
  let%sub a = opaque_const 1 in
  let%sub b = opaque_const 2 in
  let%sub c = opaque_const 3 in
  let%sub d = opaque_const 4 in
  let%sub e = opaque_const 5 in
  let%arr a = a
  and b = b
  and c = c
  and d = d
  and e = e in
  a + b + c + d + e
;;

let%expect_test _ =
  let c = instrument_computation (subst_tree ()) in
  [%expect
    {|
tree:
  1_1 return -> _1 sub @ 1:2
  1_2 incr -> 1_1 return
  2-1_1 return -> 2_1 sub @ 2:2
  2-1_2 incr -> 2-1_1 return
  2-2-1_1 return -> 2-2_1 sub @ 3:2
  2-2-1_2 incr -> 2-2-1_1 return
  2-2-2-1_1 return -> 2-2-2_1 sub @ 4:2
  2-2-2-1_2 incr -> 2-2-2-1_1 return
  2-2-2-2-1_1 return -> 2-2-2-2_1 sub @ 5:2
  2-2-2-2-1_2 incr -> 2-2-2-2-1_1 return
  2-2-2-2-2-1-1_1 named -> 2-2-2-2-2-1_1 both
  2-2-2-2-2-1-2-1_1 named -> 2-2-2-2-2-1-2_1 both
  2-2-2-2-2-1-2-2-1_1 named -> 2-2-2-2-2-1-2-2_1 both
  2-2-2-2-2-1-2-2-2-1_1 named -> 2-2-2-2-2-1-2-2-2_1 both
  2-2-2-2-2-1-2-2-2-2_1 named -> 2-2-2-2-2-1-2-2-2_1 both
  2-2-2-2-2-1-2-2-2_1 both -> 2-2-2-2-2-1-2-2_1 both
  2-2-2-2-2-1-2-2_1 both -> 2-2-2-2-2-1-2_1 both
  2-2-2-2-2-1-2_1 both -> 2-2-2-2-2-1_1 both
  2-2-2-2-2-1_1 both -> 2-2-2-2-2_2 map @ 6:2
  2-2-2-2-2_1 return -> 2-2-2-2_1 sub @ 5:2
  2-2-2-2-2_2 map @ 6:2 -> 2-2-2-2-2_1 return
  2-2-2-2_1 sub @ 5:2 -> 2-2-2_1 sub @ 4:2
  2-2-2_1 sub @ 4:2 -> 2-2_1 sub @ 3:2
  2-2_1 sub @ 3:2 -> 2_1 sub @ 2:2
  2_1 sub @ 2:2 -> _1 sub @ 1:2
  _1 sub @ 1:2 -> _0
dag:
  1_1 return -> 2-2-2-2-2-1-1_1 named, _1 sub @ 1:2
  1_2 incr -> 1_1 return
  2-1_1 return -> 2-2-2-2-2-1-2-1_1 named, 2_1 sub @ 2:2
  2-1_2 incr -> 2-1_1 return
  2-2-1_1 return -> 2-2-2-2-2-1-2-2-1_1 named, 2-2_1 sub @ 3:2
  2-2-1_2 incr -> 2-2-1_1 return
  2-2-2-1_1 return -> 2-2-2-2-2-1-2-2-2-1_1 named, 2-2-2_1 sub @ 4:2
  2-2-2-1_2 incr -> 2-2-2-1_1 return
  2-2-2-2-1_1 return -> 2-2-2-2-2-1-2-2-2-2_1 named, 2-2-2-2_1 sub @ 5:2
  2-2-2-2-1_2 incr -> 2-2-2-2-1_1 return
  2-2-2-2-2-1-1_1 named -> 2-2-2-2-2-1_1 both
  2-2-2-2-2-1-2-1_1 named -> 2-2-2-2-2-1-2_1 both
  2-2-2-2-2-1-2-2-1_1 named -> 2-2-2-2-2-1-2-2_1 both
  2-2-2-2-2-1-2-2-2-1_1 named -> 2-2-2-2-2-1-2-2-2_1 both
  2-2-2-2-2-1-2-2-2-2_1 named -> 2-2-2-2-2-1-2-2-2_1 both
  2-2-2-2-2-1-2-2-2_1 both -> 2-2-2-2-2-1-2-2_1 both
  2-2-2-2-2-1-2-2_1 both -> 2-2-2-2-2-1-2_1 both
  2-2-2-2-2-1-2_1 both -> 2-2-2-2-2-1_1 both
  2-2-2-2-2-1_1 both -> 2-2-2-2-2_2 map @ 6:2
  2-2-2-2-2_1 return -> 2-2-2-2_1 sub @ 5:2
  2-2-2-2-2_2 map @ 6:2 -> 2-2-2-2-2_1 return
  2-2-2-2_1 sub @ 5:2 -> 2-2-2_1 sub @ 4:2
  2-2-2_1 sub @ 4:2 -> 2-2_1 sub @ 3:2
  2-2_1 sub @ 3:2 -> 2_1 sub @ 2:2
  2_1 sub @ 2:2 -> _1 sub @ 1:2
  _1 sub @ 1:2 -> _0 |}];
  let handle = Handle.create (Result_spec.string (module Int)) c in
  Handle.show handle;
  [%expect {|
     start-##map 2-2-2-2-2_2
     stop-##map 2-2-2-2-2_2
     15 |}]
;;

let diamond () =
  test_start [%here];
  let%sub a = opaque_const 0 in
  let%sub b = Bonsai.pure Fn.id a in
  let%sub c = Bonsai.pure Fn.id a in
  let%arr b = b
  and c = c in
  b + c
;;

let%expect_test "diamond" =
  let (_ : int Computation.t) = instrument_computation (diamond ()) in
  [%expect
    {|
    tree:
      1_1 return -> _1 sub @ 1:2
      1_2 incr -> 1_1 return
      2-1-1_1 named -> 2-1_2 map
      2-1_1 return -> 2_1 sub @ 2:2
      2-1_2 map -> 2-1_1 return
      2-2-1-1_1 named -> 2-2-1_2 map
      2-2-1_1 return -> 2-2_1 sub @ 3:2
      2-2-1_2 map -> 2-2-1_1 return
      2-2-2-1-1_1 named -> 2-2-2-1_1 both
      2-2-2-1-2_1 named -> 2-2-2-1_1 both
      2-2-2-1_1 both -> 2-2-2_2 map @ 4:2
      2-2-2_1 return -> 2-2_1 sub @ 3:2
      2-2-2_2 map @ 4:2 -> 2-2-2_1 return
      2-2_1 sub @ 3:2 -> 2_1 sub @ 2:2
      2_1 sub @ 2:2 -> _1 sub @ 1:2
      _1 sub @ 1:2 -> _0
    dag:
      1_1 return -> 2-2-1-1_1 named, 2-1-1_1 named, _1 sub @ 1:2
      1_2 incr -> 1_1 return
      2-1-1_1 named -> 2-1_2 map
      2-1_1 return -> 2-2-2-1-1_1 named, 2_1 sub @ 2:2
      2-1_2 map -> 2-1_1 return
      2-2-1-1_1 named -> 2-2-1_2 map
      2-2-1_1 return -> 2-2-2-1-2_1 named, 2-2_1 sub @ 3:2
      2-2-1_2 map -> 2-2-1_1 return
      2-2-2-1-1_1 named -> 2-2-2-1_1 both
      2-2-2-1-2_1 named -> 2-2-2-1_1 both
      2-2-2-1_1 both -> 2-2-2_2 map @ 4:2
      2-2-2_1 return -> 2-2_1 sub @ 3:2
      2-2-2_2 map @ 4:2 -> 2-2-2_1 return
      2-2_1 sub @ 3:2 -> 2_1 sub @ 2:2
      2_1 sub @ 2:2 -> _1 sub @ 1:2
      _1 sub @ 1:2 -> _0 |}]
;;

let state () =
  test_start [%here];
  let%sub state =
    Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
  in
  return state
;;

let%expect_test "state" =
  let c = instrument_computation (state ()) in
  [%expect
    {|
     tree:
       1_1 leaf0 -> _1 sub @ 1:2
       2_1 return -> _1 sub @ 1:2
       2_2 named -> 2_1 return
       _1 sub @ 1:2 -> _0
     dag:
       1_1 leaf0 -> 2_2 named, _1 sub @ 1:2
       2_1 return -> _1 sub @ 1:2
       2_2 named -> 2_1 return
       _1 sub @ 1:2 -> _0 |}];
  let handle =
    Handle.create
      (module struct
        type t = int * (int -> unit Effect.t)
        type incoming = int

        let view (state, _) = [%string "%{state#Int}"]
        let incoming (state, set_state) add = set_state (state + add)
      end)
      c
  in
  Handle.show handle;
  [%expect {| 0 |}];
  Handle.do_actions handle [ 1; 2 ];
  Handle.show handle;
  [%expect
    {|
     start-##leaf0-apply_action 1_1
     stop-##leaf0-apply_action 1_1
     start-##leaf0-apply_action 1_1
     stop-##leaf0-apply_action 1_1
     2 |}]
;;

let dynamic_state () =
  test_start [%here];
  let%sub state =
    Bonsai.state_machine1
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) input model () ->
        match input with
        | Active () -> model + 1
        | Inactive ->
          print_endline "inactive";
          model)
      (opaque_const_value ())
  in
  return state
;;

let%expect_test "dynamic_state" =
  let c = instrument_computation (dynamic_state ()) in
  [%expect
    {|
     tree:
       1_1 leaf1 -> _1 sub @ 1:2
       1_2 incr -> 1_1 leaf1
       2_1 return -> _1 sub @ 1:2
       2_2 named -> 2_1 return
       _1 sub @ 1:2 -> _0
     dag:
       1_1 leaf1 -> 2_2 named, _1 sub @ 1:2
       1_2 incr -> 1_1 leaf1
       2_1 return -> _1 sub @ 1:2
       2_2 named -> 2_1 return
       _1 sub @ 1:2 -> _0 |}];
  let handle =
    Handle.create
      (module struct
        type t = int * (unit -> unit Effect.t)
        type incoming = unit

        let view (state, _) = [%string "%{state#Int}"]
        let incoming (_, increment) () = increment ()
      end)
      c
  in
  Handle.show handle;
  [%expect {| 0 |}];
  Handle.do_actions handle [ (); () ];
  Handle.recompute_view handle;
  [%expect
    {|
     start-##leaf1-apply_action 1_1
     stop-##leaf1-apply_action 1_1
     start-##leaf1-apply_action 1_1
     stop-##leaf1-apply_action 1_1 |}];
  Handle.show handle;
  [%expect {| 2 |}]
;;

let dynamic_scope () =
  test_start [%here];
  let dynamic_var = Bonsai.Dynamic_scope.create ~name:"a" ~fallback:0 () in
  Bonsai.Dynamic_scope.set
    dynamic_var
    (Bonsai.Value.return 10)
    ~inside:
      (let%sub a = Bonsai.Dynamic_scope.lookup dynamic_var in
       let%arr a = a in
       a)
;;

let%expect_test "dynamic scope" =
  let (_ : int Computation.t) = instrument_computation (dynamic_scope ()) in
  [%expect
    {|
     tree:
       1_1 constant -> _1 store
       2-1_1 fetch -> 2_1 sub @ 6:7
       2-2-1_1 named -> 2-2_2 map @ 7:7
       2-2_1 return -> 2_1 sub @ 6:7
       2-2_2 map @ 7:7 -> 2-2_1 return
       2_1 sub @ 6:7 -> _1 store
       _1 store -> _0
     dag:
       1_1 constant -> 2-1_1 fetch, _1 store
       2-1_1 fetch -> 2-2-1_1 named, 2_1 sub @ 6:7
       2-2-1_1 named -> 2-2_2 map @ 7:7
       2-2_1 return -> 2_1 sub @ 6:7
       2-2_2 map @ 7:7 -> 2-2_1 return
       2_1 sub @ 6:7 -> _1 store
       _1 store -> _0 |}]
;;

let cutoff ?(var = Bonsai.Var.create 0) () =
  test_start [%here];
  let value = Bonsai.Var.value var in
  return (Value.cutoff value ~equal:(fun _ _ -> false))
;;

let%expect_test "cutoff" =
  let var = Bonsai.Var.create 0 in
  let c = instrument_computation (cutoff ~var ()) in
  [%expect
    {|
     tree:
       1_1 incr -> _2 cutoff
       _1 return -> _0
       _2 cutoff -> _1 return
     dag:
       1_1 incr -> _2 cutoff
       _1 return -> _0
       _2 cutoff -> _1 return
     |}];
  let handle = Handle.create (Result_spec.string (module Int)) c in
  Handle.show handle;
  [%expect {| 0 |}];
  Bonsai.Var.set var 2;
  Handle.show handle;
  [%expect {| 2 |}];
  Bonsai.Var.set var 1;
  Handle.show handle;
  [%expect {| 1 |}]
;;

let assoc_simpl () =
  test_start [%here];
  Bonsai.assoc
    (module Int)
    (Value.return (Int.Map.of_alist_exn [ -1, (); 1, () ]))
    ~f:(fun i _ -> return i)
;;

let%expect_test "assoc_simpl" =
  let c = instrument_computation (assoc_simpl ()) in
  [%expect
    {|
     tree:
       1_1 constant -> _1 assoc
       2_1 return -> _1 assoc
       2_2 named -> 2_1 return
       _1 assoc -> _0
     dag:
       1_1 constant -> _1 assoc
       2_1 return -> 2_2 named, _1 assoc
       2_2 named -> 2_1 return
       _1 assoc -> _0 |}];
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int Int.Map.t [@@deriving sexp_of]
         end))
      c
  in
  Handle.show handle;
  [%expect {|
     ((-1 -1) (1 1)) |}]
;;

let assoc () =
  test_start [%here];
  let%sub a = opaque_const 0 in
  Bonsai.assoc
    (module Int)
    (Value.return (Int.Map.of_alist_exn [ -1, (); 1, () ]))
    ~f:(fun _ _ ->
      let%arr a = a in
      a)
;;

let%expect_test "assoc" =
  let c = instrument_computation (assoc ()) in
  [%expect
    {|
     tree:
       1_1 return -> _1 sub @ 1:2
       1_2 incr -> 1_1 return
       2-1_1 constant -> 2_1 assoc
       2-2-1_1 named -> 2-2_2 map @ 6:6
       2-2_1 return -> 2_1 assoc
       2-2_2 map @ 6:6 -> 2-2_1 return
       2_1 assoc -> _1 sub @ 1:2
       _1 sub @ 1:2 -> _0
     dag:
       1_1 return -> 2-2-1_1 named, _1 sub @ 1:2
       1_2 incr -> 1_1 return
       2-1_1 constant -> 2_1 assoc
       2-2-1_1 named -> 2-2_2 map @ 6:6
       2-2_1 return -> 2_1 assoc
       2-2_2 map @ 6:6 -> 2-2_1 return
       2_1 assoc -> _1 sub @ 1:2
       _1 sub @ 1:2 -> _0 |}];
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int Int.Map.t [@@deriving sexp_of]
         end))
      c
  in
  Handle.show handle;
  [%expect
    {|
     start-##map 2-2_2
     stop-##map 2-2_2
     start-##map 2-2_2
     stop-##map 2-2_2
     ((-1 0) (1 0)) |}]
;;

let assoc_constant_folding () =
  test_start [%here];
  let%sub a = Bonsai.const 0 in
  Bonsai.assoc
    (module Int)
    (Value.return (Int.Map.of_alist_exn [ -1, (); 1, () ]))
    ~f:(fun _ _ ->
      let%arr a = a in
      a)
;;

let%expect_test "assoc constant folding" =
  let c = instrument_computation (assoc_constant_folding ()) in
  [%expect
    {|
    tree:
      1_1 return -> _1 sub @ 1:2
      1_2 constant -> 1_1 return
      2-1_1 constant -> 2_1 assoc
      2-2-1_1 named -> 2-2_2 map @ 6:6
      2-2_1 return -> 2_1 assoc
      2-2_2 map @ 6:6 -> 2-2_1 return
      2_1 assoc -> _1 sub @ 1:2
      _1 sub @ 1:2 -> _0
    dag:
      1_1 return -> 2-2-1_1 named, _1 sub @ 1:2
      1_2 constant -> 1_1 return
      2-1_1 constant -> 2_1 assoc
      2-2-1_1 named -> 2-2_2 map @ 6:6
      2-2_1 return -> 2_1 assoc
      2-2_2 map @ 6:6 -> 2-2_1 return
      2_1 assoc -> _1 sub @ 1:2
      _1 sub @ 1:2 -> _0 |}];
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int Int.Map.t [@@deriving sexp_of]
         end))
      c
  in
  Handle.show handle;
  [%expect
    {|
     start-##map 2-2_2
     stop-##map 2-2_2
     start-##map 2-2_2
     stop-##map 2-2_2
     ((-1 0) (1 0)) |}]
;;

let nested_values ?(a = Value.return 0) () =
  test_start [%here];
  return
    (let%map a =
       let%map a =
         let%map a = a in
         a
       in
       a
     in
     a)
;;

let%expect_test "nested values" =
  let a_var = Bonsai.Var.create 0 in
  let a = Bonsai.Var.value a_var in
  let c = instrument_computation (nested_values ~a ()) in
  [%expect
    {|
     tree:
       1-1-1_1 incr -> 1-1_1 map
       1-1_1 map -> 1_1 map
       1_1 map -> _2 map
       _1 return -> _0
       _2 map -> _1 return
     dag:
       1-1-1_1 incr -> 1-1_1 map
       1-1_1 map -> 1_1 map
       1_1 map -> _2 map
       _1 return -> _0
       _2 map -> _1 return
     |}];
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = int [@@deriving sexp_of]
         end))
      c
  in
  Handle.show handle;
  [%expect
    {|
     start-##map 1-1_1
     stop-##map 1-1_1
     start-##map 1_1
     stop-##map 1_1
     start-##map _2
     stop-##map _2
     0
     |}];
  Bonsai.Var.set a_var 2;
  Handle.show handle;
  [%expect
    {|
     start-##map 1-1_1
     stop-##map 1-1_1
     start-##map 1_1
     stop-##map 1_1
     start-##map _2
     stop-##map _2
     2
     |}];
  Handle.show handle;
  [%expect {| 2 |}]
;;

let enum ?(a = Value.return 0) ?(match_ = Value.return true) () =
  test_start [%here];
  Bonsai.enum
    (module Bool)
    ~match_
    ~with_:(function
      | true ->
        let%arr a = a in
        a > 0
      | false -> return match_)
;;

let%expect_test "enum" =
  let match_var = Bonsai.Var.create true in
  let match_ = Bonsai.Var.value match_var in
  let a_var = Bonsai.Var.create 0 in
  let a = Bonsai.Var.value a_var in
  let c = instrument_computation (enum ~a ~match_ ()) in
  [%expect
    {|
     tree:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 incr -> 2_1 return
       3-1_1 incr -> 3_2 map @ 6:8
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 map @ 6:8 -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     dag:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 incr -> 2_1 return
       3-1_1 incr -> 3_2 map @ 6:8
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 map @ 6:8 -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     |}];
  let handle = Handle.create (Result_spec.sexp (module Bool)) c in
  Handle.show handle;
  [%expect
    {|
     start-##map 1_1
     stop-##map 1_1
     start-##map 3_2
     stop-##map 3_2
     false |}];
  Bonsai.Var.set match_var false;
  Handle.show handle;
  [%expect {|
     start-##map 1_1
     stop-##map 1_1
     false |}];
  Bonsai.Var.set a_var 5;
  Handle.show handle;
  [%expect {| false |}];
  Bonsai.Var.set match_var true;
  Handle.show handle;
  [%expect
    {|
     start-##map 1_1
     stop-##map 1_1
     start-##map 3_2
     stop-##map 3_2
     true |}];
  Bonsai.Var.set a_var 10;
  Handle.show handle;
  [%expect {|
     start-##map 3_2
     stop-##map 3_2
     true |}]
;;

let lazy_computation ?(match_ = Value.return true) () =
  test_start [%here];
  let a = Bonsai.Value.return 0 in
  Bonsai.enum
    (module Bool)
    ~match_
    ~with_:(function
      | true -> return a
      | false -> Bonsai.lazy_ (lazy (return a)))
;;

let%expect_test "lazy" =
  let match_var = Bonsai.Var.create true in
  let match_ = Bonsai.Var.value match_var in
  let c = instrument_computation (lazy_computation ~match_ ()) in
  [%expect
    {|
     tree:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     dag:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     |}];
  let handle = Handle.create (Result_spec.sexp (module Int)) c in
  Handle.show handle;
  [%expect {|
     start-##map 1_1
     stop-##map 1_1
     0 |}];
  Bonsai.Var.set match_var false;
  Handle.show handle;
  [%expect
    {|
     start-##map 1_1
     stop-##map 1_1
     tree:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 return -> 2_1 lazy
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     dag:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     tree:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 return -> 2_1 lazy
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     dag:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 return -> 2_1 lazy
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     tree:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 return -> 2_1 lazy
       2_3 constant -> 2_2 return
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     dag:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 return -> 2_1 lazy
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     tree:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 return -> 2_1 lazy
       2_3 constant -> 2_2 return
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     dag:
       1-1_1 incr -> 1_1 map
       1_1 map -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_1 lazy -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       2_2 return -> 2_1 lazy
       2_3 constant -> 2_2 return
       3_1 return -> _1 switch @ lib/bonsai/src/proc.ml:51:26
       3_2 constant -> 3_1 return
       _1 switch @ lib/bonsai/src/proc.ml:51:26 -> _0
     0 |}]
;;

let shared =
  let%sub () = opaque_const () in
  () |> Bonsai.Var.create |> Bonsai.Var.value |> return
;;

let name_used_twice () =
  test_start [%here];
  let%sub () = shared in
  let%sub () = shared in
  Bonsai.const ()
;;

let%expect_test "name_used_twice" =
  let (_ : unit Computation.t) = instrument_computation (name_used_twice ()) in
  [%expect
    {|
     tree:
       1-1_1 return -> 1_1 sub @ -5:2
       1-1_2 incr -> 1-1_1 return
       1-2-1-1_1 named -> 1-2-1_2 map @ -5:2
       1-2-1_1 return -> 1-2_1 sub @ -5:2
       1-2-1_2 map @ -5:2 -> 1-2-1_1 return
       1-2-2_1 return -> 1-2_1 sub @ -5:2
       1-2-2_2 incr -> 1-2-2_1 return
       1-2_1 sub @ -5:2 -> 1_1 sub @ -5:2
       1_1 sub @ -5:2 -> _1 sub @ 1:2
       2-1-1_1 named -> 2-1_2 map @ 1:2
       2-1_1 return -> 2_1 sub @ 1:2
       2-1_2 map @ 1:2 -> 2-1_1 return
       2-2-1-1_1 return -> 2-2-1_1 sub @ -5:2
       2-2-1-1_2 incr -> 2-2-1-1_1 return
       2-2-1-2-1-1_1 named -> 2-2-1-2-1_2 map @ -5:2
       2-2-1-2-1_1 return -> 2-2-1-2_1 sub @ -5:2
       2-2-1-2-1_2 map @ -5:2 -> 2-2-1-2-1_1 return
       2-2-1-2-2_1 return -> 2-2-1-2_1 sub @ -5:2
       2-2-1-2-2_2 incr -> 2-2-1-2-2_1 return
       2-2-1-2_1 sub @ -5:2 -> 2-2-1_1 sub @ -5:2
       2-2-1_1 sub @ -5:2 -> 2-2_1 sub @ 2:2
       2-2-2-1-1_1 named -> 2-2-2-1_2 map @ 2:2
       2-2-2-1_1 return -> 2-2-2_1 sub @ 2:2
       2-2-2-1_2 map @ 2:2 -> 2-2-2-1_1 return
       2-2-2-2_1 return -> 2-2-2_1 sub @ 2:2
       2-2-2-2_2 constant -> 2-2-2-2_1 return
       2-2-2_1 sub @ 2:2 -> 2-2_1 sub @ 2:2
       2-2_1 sub @ 2:2 -> 2_1 sub @ 1:2
       2_1 sub @ 1:2 -> _1 sub @ 1:2
       _1 sub @ 1:2 -> _0
     dag:
       1-1_1 return -> 1-2-1-1_1 named, 1_1 sub @ -5:2
       1-1_2 incr -> 1-1_1 return
       1-2-1-1_1 named -> 1-2-1_2 map @ -5:2
       1-2-1_1 return -> 1-2_1 sub @ -5:2
       1-2-1_2 map @ -5:2 -> 1-2-1_1 return
       1-2-2_1 return -> 1-2_1 sub @ -5:2
       1-2-2_2 incr -> 1-2-2_1 return
       1-2_1 sub @ -5:2 -> 1_1 sub @ -5:2
       1_1 sub @ -5:2 -> 2-1-1_1 named, _1 sub @ 1:2
       2-1-1_1 named -> 2-1_2 map @ 1:2
       2-1_1 return -> 2_1 sub @ 1:2
       2-1_2 map @ 1:2 -> 2-1_1 return
       2-2-1-1_1 return -> 2-2-1-2-1-1_1 named, 2-2-1_1 sub @ -5:2
       2-2-1-1_2 incr -> 2-2-1-1_1 return
       2-2-1-2-1-1_1 named -> 2-2-1-2-1_2 map @ -5:2
       2-2-1-2-1_1 return -> 2-2-1-2_1 sub @ -5:2
       2-2-1-2-1_2 map @ -5:2 -> 2-2-1-2-1_1 return
       2-2-1-2-2_1 return -> 2-2-1-2_1 sub @ -5:2
       2-2-1-2-2_2 incr -> 2-2-1-2-2_1 return
       2-2-1-2_1 sub @ -5:2 -> 2-2-1_1 sub @ -5:2
       2-2-1_1 sub @ -5:2 -> 2-2-2-1-1_1 named, 2-2_1 sub @ 2:2
       2-2-2-1-1_1 named -> 2-2-2-1_2 map @ 2:2
       2-2-2-1_1 return -> 2-2-2_1 sub @ 2:2
       2-2-2-1_2 map @ 2:2 -> 2-2-2-1_1 return
       2-2-2-2_1 return -> 2-2-2_1 sub @ 2:2
       2-2-2-2_2 constant -> 2-2-2-2_1 return
       2-2-2_1 sub @ 2:2 -> 2-2_1 sub @ 2:2
       2-2_1 sub @ 2:2 -> 2_1 sub @ 1:2
       2_1 sub @ 1:2 -> _1 sub @ 1:2
       _1 sub @ 1:2 -> _0 |}]
;;

type packed = T : (unit -> 'a Computation.t) -> packed

open Async

let command =
  Command.async
    ~summary:"Run tests for bonsai instrumentation"
    (Command.Param.return (fun () ->
       Writer.with_file "tests.md" ~f:(fun writer ->
         Writer.write_line writer "---";
         Writer.write_line writer "title: Bonsai instrumentation tests DOT output";
         Writer.write_line writer "parent: ../../../README.md";
         Writer.write_line writer "uuid: 70aac2f6-625f-3328-6029-cedd12cf8d1d";
         Writer.write_line writer "---";
         let%bind () =
           [ "subst_tree", T subst_tree
           ; "many_aliases", T many_aliases
           ; "diamond", T diamond
           ; "state", T state
           ; "dynamic_state", T dynamic_state
           ; "dynamic_scope", T dynamic_scope
           ; "cutoff", T cutoff
           ; "assoc_simpl", T assoc_simpl
           ; "assoc", T assoc
           ; "nested_values", T nested_values
           ; "enum", T enum
           ; "lazy", T lazy_computation
           ; "name_used_twice", T name_used_twice
           ]
           |> Deferred.List.iter ~how:`Sequential ~f:(fun (name, T computation) ->
             print_endline [%string "Processing %{name}"];
             write_computation_to_dot [%string "%{name}.dot"] (computation ());
             Sys_unix.command_exn [%string "dot -Tsvg %{name}.dot -o %{name}.svg"];
             let%bind () = Sys.remove [%string "%{name}.dot"] in
             Writer.write
               writer
               [%string "# %{name}\n\n![](%{name}.svg \"Graph for %{name}\")\n\n"];
             Writer.flushed writer)
         in
         let%bind () =
           (* We have to special-case a bunch of code in order to force the
              lazy component and extract the graph info to write it to a file.
              The following block of code is a slightly modified copy of the
              function above and also has a slightly modified
              [write_computation_to_dot] inlined. *)
           let name = "lazy_forced" in
           print_endline [%string "Processing %{name}"];
           let filename = [%string "%{name}.dot"] in
           let component = lazy_computation ~match_:(Value.return false) () in
           let () =
             let graph_info = ref Graph_info.empty in
             let (component : _ Private.Computation.t) =
               Graph_info.iter_graph_updates
                 (Bonsai.Private.reveal_computation component)
                 ~on_update:(fun gm -> graph_info := gm)
             in
             let handle =
               Handle.create
                 (Result_spec.sexp (module Int))
                 (Bonsai.Private.conceal_computation component)
             in
             Handle.recompute_view handle;
             graph_info_to_dot filename !graph_info
           in
           Sys_unix.command_exn [%string "dot -Tsvg %{name}.dot -o %{name}.svg"];
           let%bind () = Sys.remove [%string "%{name}.dot"] in
           Writer.write
             writer
             [%string "# %{name}\n\n![](%{name}.svg \"Graph for %{name}\")\n\n"];
           Writer.flushed writer
         in
         return ())))
    ~behave_nicely_in_pipeline:false
;;
