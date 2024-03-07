open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai_test_of_bonsai_itself.Big_computation_regression_util

type handle =
  { handle : (String.Set.t, Nothing.t) Handle.t
  ; set_state : state -> unit
  }

and state =
  | No_op
  | Big_computation_is_active

let create_handle (computation : String.Set.t Computation.t) : handle =
  let open Bonsai.Let_syntax in
  let var = Bonsai.Var.create No_op in
  let computation =
    match%sub Bonsai.Var.value var with
    | No_op -> Bonsai.const String.Set.empty
    | Big_computation_is_active -> computation
  in
  let set_state state = Bonsai.Var.set var state in
  let handle = Handle.create lengths_result_spec computation in
  { handle; set_state }
;;

let%test_module "Demonstrating hypothetical crash after successful startup." =
  (* NOTE: We have not yet observed this on real apps that have migrated to cont and is
     solely a hypothetical. *)
  (module struct
    let%expect_test "Proc Syntax" =
      let { handle; set_state } = create_handle (For_proc.basic ~height:10 ~width:7) in
      Handle.show handle;
      [%expect {| () |}];
      set_state Big_computation_is_active;
      Handle.show handle;
      [%expect {| (13 15 17 19 21 23 25 27 29 31 31) |}]
    ;;

    let%expect_test "Cont" =
      let { handle; set_state } = create_handle (For_cont.basic ~height:10 ~width:7) in
      Handle.show handle;
      [%expect {| () |}];
      set_state Big_computation_is_active;
      (* NOTE: This shows that an app can crash at runtime after having started
         successfully. *)
      Handle.show handle;
      [%expect {| (13 15 17 19 21 23 25 27 29 31 31) |}]
    ;;
  end)
;;
