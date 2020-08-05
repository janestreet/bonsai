open! Core_kernel
open! Import
open Proc
module Bonsai = Bonsai.Proc
open Bonsai.Let_syntax

let%expect_test "cutoff" =
  let var = Bonsai.Var.create 0 in
  let value = Bonsai.Var.value var in
  let component =
    return @@ Bonsai.Value.cutoff value ~equal:(fun a b -> a % 2 = b % 2)
  in
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
      [%expect {| (((1 (hello ()))) ()) |}]
    ;;
  end)
;;
