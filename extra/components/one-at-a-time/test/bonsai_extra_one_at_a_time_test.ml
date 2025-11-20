open! Core
open Bonsai_test
module Bonsai = Bonsai_proc
open Bonsai.For_open
module One_at_a_time = Bonsai_extra_proc.One_at_a_time

(* These test are a bit hairier then I'd like because [One_at_a_time.effect] only updates
   its state machine on [Handle.show] boundaries, which makes things take longer than I'd
   like. While we usually try to avoid computation introducing frame-delays, this seems
   sort of okay because it isn't late by a whole frame, and the lateness doesn't compound
   when this computation is chained together with itself. *)

let create_handle component =
  Handle.create
    (module struct
      type t = (int -> int One_at_a_time.Response.t Effect.t) * One_at_a_time.Status.t
      type incoming = int

      let view (_, status) = Sexp.to_string ([%sexp_of: One_at_a_time.Status.t] status)

      let incoming (f, _) i =
        let%bind.Effect result = f i in
        Effect.print_s [%message (result : int One_at_a_time.Response.t)]
      ;;
    end)
    component
;;

let%expect_test {| One_at_a_time.effect only runs one instance of an effect at a time |} =
  let qrt = Effect.For_testing.Query_response_tracker.create () in
  let respond i =
    Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun _ -> Respond i)
  in
  let component =
    One_at_a_time.effect (Value.return (Effect.For_testing.of_query_response_tracker qrt))
  in
  let handle = create_handle component in
  Handle.show handle;
  [%expect {| Idle |}];
  Handle.do_actions handle [ 0 ];
  Handle.show handle;
  [%expect {| Busy |}];
  Handle.do_actions handle [ 0 ];
  Handle.show handle;
  [%expect
    {|
    (result Busy)
    Busy
    |}];
  respond 0;
  Handle.show handle;
  [%expect
    {|
    (result (Result 0))
    Idle
    |}];
  Handle.do_actions handle [ 0 ];
  Handle.show handle;
  [%expect {| Busy |}];
  respond 1;
  Handle.do_actions handle [ 0 ];
  Handle.show handle;
  [%expect
    {|
    (result (Result 1))
    Busy
    |}];
  respond 2;
  Handle.do_actions handle [ 0; 0 ];
  Handle.show handle;
  [%expect
    {|
    (result (Result 2))
    (result Busy)
    Busy
    |}];
  respond 3;
  respond 4;
  Handle.show handle;
  [%expect
    {|
    (result (Result 3))
    Idle
    |}]
;;

let%expect_test {| Double [One_at_a_time.effect] application should be consistent between the two invocations. |}
  =
  let qrt = Effect.For_testing.Query_response_tracker.create () in
  let respond i =
    Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun () -> Respond i)
  in
  let component =
    let open Bonsai.Let_syntax in
    let%sub effect, status1 =
      One_at_a_time.effect
        (Value.return (Effect.For_testing.of_query_response_tracker qrt))
    in
    let%sub effect, status2 = One_at_a_time.effect effect in
    let%arr status1 and status2 and effect in
    effect, status1, status2
  in
  let handle =
    Handle.create
      (module struct
        type t =
          (unit -> int One_at_a_time.Response.t One_at_a_time.Response.t Ui_effect.t)
          * One_at_a_time.Status.t
          * One_at_a_time.Status.t

        type incoming = unit

        let view (_, status1, status2) =
          Sexp.to_string_hum
            [%message
              (status1 : One_at_a_time.Status.t) (status2 : One_at_a_time.Status.t)]
        ;;

        let incoming (effect, _, _) () =
          let%bind.Effect result = effect () in
          Effect.print_s
            [%message (result : int One_at_a_time.Response.t One_at_a_time.Response.t)]
        ;;
      end)
      component
  in
  Handle.show handle;
  [%expect {| ((status1 Idle) (status2 Idle)) |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| ((status1 Busy) (status2 Busy)) |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect
    {|
    (result Busy)
    ((status1 Busy) (status2 Busy))
    |}];
  respond 0;
  Handle.show handle;
  [%expect
    {|
    (result (Result (Result 0)))
    ((status1 Idle) (status2 Idle))
    |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| ((status1 Busy) (status2 Busy)) |}];
  respond 1;
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect
    {|
    (result Busy)
    (result (Result (Result 1)))
    ((status1 Idle) (status2 Idle))
    |}];
  Handle.do_actions handle [ (); () ];
  respond 2;
  Handle.show handle;
  [%expect
    {|
    (result Busy)
    ((status1 Busy) (status2 Busy))
    |}];
  respond 3;
  respond 4;
  Handle.show handle;
  [%expect
    {|
    (result (Result (Result 3)))
    ((status1 Idle) (status2 Idle))
    |}]
;;

let%expect_test {| One_at_a_time.effect releases lock after effect throws exception |} =
  let qrt = Effect.For_testing.Query_response_tracker.create () in
  let complete () =
    Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun _ -> Respond ())
  in
  let delay_effect = Effect.For_testing.of_query_response_tracker qrt in
  let fail_effect x =
    if x = 0
    then failwith "error while computing effect"
    else (
      let%bind.Effect () = delay_effect () in
      Effect.of_sync_fun
        (fun x -> if x = 1 then failwith "error while running effect" else x)
        x)
  in
  let component = One_at_a_time.effect (Value.return fail_effect) in
  let handle = create_handle component in
  Handle.show handle;
  [%expect {| Idle |}];
  Handle.do_actions handle [ 0 ];
  Handle.show handle;
  [%expect
    {|
    (result (Exn (Failure "error while computing effect")))
    Idle
    |}];
  Handle.show handle;
  [%expect {| Idle |}];
  Handle.do_actions handle [ 1 ];
  Handle.show handle;
  [%expect {| Busy |}];
  Expect_test_helpers_core.require_does_not_raise (fun () -> complete ());
  [%expect {| |}];
  Handle.show handle;
  [%expect
    {|
    (result (Exn (Failure "error while running effect")))
    Idle
    |}];
  Handle.show handle;
  [%expect {| Idle |}];
  Handle.show handle;
  [%expect {| Idle |}]
;;
