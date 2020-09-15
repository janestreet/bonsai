open! Core_kernel
open! Import
module Bonsai_lib = Bonsai
open Proc
open Bonsai.Let_syntax

let dummy_source_code_position =
  Source_code_position.
    { pos_fname = "file_name.ml"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
;;

let%expect_test "cutoff" =
  let var = Bonsai.Var.create 0 in
  let value = Bonsai.Var.value var in
  let component = return @@ Bonsai.Value.cutoff value ~equal:(fun a b -> a % 2 = b % 2) in
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

let%expect_test "if_" =
  let component input =
    let a = Bonsai.Value.return "hello" in
    let b = Bonsai.Value.return "world" in
    Bonsai.if_ input ~then_:(Bonsai.read a) ~else_:(Bonsai.read b)
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
    let%sub c = add (Bonsai.Value.both a b) in
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
        type t = string * (unit -> Event.t)
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

let%expect_test "match_either" =
  let var : (string, int) Either.t Bonsai.Var.t =
    Bonsai.Var.create (Either.First "hello")
  in
  let component =
    Bonsai.match_either
      (Bonsai.Var.value var)
      ~first:(fun s -> Bonsai.read (Bonsai.Value.map s ~f:(sprintf "%s world")))
      ~second:(fun i -> Bonsai.read (Bonsai.Value.map i ~f:Int.to_string))
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| hello world |}];
  Bonsai.Var.set var (Second 2);
  Handle.show handle;
  [%expect {| 2 |}]
;;

let%expect_test "assoc simplifies its inner computation, if possible" =
  let value = Bonsai.Value.return String.Map.empty in
  let component =
    Bonsai.assoc
      (module String)
      value
      ~f:(fun key data -> Bonsai.read (Bonsai.Value.both key data))
  in
  print_s Bonsai_lib.Private.(Computation.sexp_of_packed (reveal_computation component));
  [%expect {| (Assoc_simpl ((map constant))) |}]
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
  let rec f t =
    let%pattern_bind { M.label; children }, depth = t in
    let%sub children =
      Bonsai.assoc
        (module Int)
        children
        ~f:(fun _ v ->
          let recursive =
            let%map v = v
            and depth = depth in
            v, depth + 1
          in
          Bonsai.lazy_ (lazy (f recursive)))
    in
    return
    @@ let%map label = label
    and children = children
    and depth = depth in
    [%message label (depth : int) (children : Sexp.t Int.Map.t)]
  in
  let var = Bonsai.Var.create ({ M.label = "hi"; children = Int.Map.empty }, 0) in
  let value = Bonsai.Var.value var in
  let handle = Handle.create (Result_spec.sexp (module Sexp)) (f value) in
  [%expect {| |}];
  Handle.show handle;
  [%expect {| (hi (depth 0) (children ())) |}];
  Bonsai.Var.set
    var
    ( { M.label = "hi"
      ; children = Int.Map.singleton 0 { M.label = "hello"; children = Int.Map.empty }
      }
    , 0 );
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
        type t = (int * (int -> Event.t)) Int.Map.t
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
    |> Ui_event.Expert.handle
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
     (key <opaque>) (action 4))
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
          ; set_state : string -> Event.t
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
    |> Bonsai.Value.map ~f:(fun (_ : int) -> ())
    |> Bonsai.Value.map ~f:(fun () -> print_endline "triggered")
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
    let%map () = Bonsai.Value.return ()
    and () = Bonsai.Value.return ()
    and () = Bonsai.Value.return ()
    and () = Bonsai.Value.return ()
    and () = Bonsai.Value.return ()
    and () = Bonsai.Value.return ()
    and () = Bonsai.Value.return () in
    ()
  in
  let packed =
    let open Bonsai.Private in
    value |> reveal_value |> Value.eval Environment.empty |> Incr.pack
  in
  let filename = Stdlib.Filename.temp_file "incr" "out" in
  Incremental.Packed.save_dot filename [ packed ];
  let dot_contents = In_channel.read_all filename in
  require
    [%here]
    ~if_false_then_print_s:(lazy [%message "No Map7 node found"])
    (String.is_substring dot_contents ~substring:"Map7");
  [%expect {| |}]
;;

let%test_unit "constant prop doesn't happen" =
  (* Just make sure that this expression doesn't crash *)
  let (_ : int Bonsai.Computation.t) =
    Bonsai.match_either
      (Bonsai.Value.return (First 1))
      ~first:Bonsai.read
      ~second:Bonsai.read
  in
  ()
;;

module Dot = struct
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
    print_endline (Bonsai.Private.to_dot c);
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
    print_endline (Bonsai.Private.to_dot c);
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
end
