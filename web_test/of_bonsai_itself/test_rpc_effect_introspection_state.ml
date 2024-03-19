open! Core
open Bonsai_introspection_protocol
open For_testing

(* This module tests the state machine used for communicating events on rpc effect. *)

let test events =
  let state =
    List.fold events ~init:State.empty ~f:(fun state event ->
      State.apply_event state (Event.conceal event))
  in
  let columns =
    [ Ascii_table_kernel.Column.create "id" (fun (id, _) -> Rpc_id.to_string id)
    ; Ascii_table_kernel.Column.create "Response" (fun (_, { Rpc_state.status; _ }) ->
        Sexp.to_string_hum ([%sexp_of: Rpc_status.t] status))
    ]
  in
  let table =
    Ascii_table_kernel.to_string_noattr columns (Map.to_alist state) ~bars:`Unicode
  in
  print_s [%sexp (state : State.t)];
  print_endline table
;;

let%expect_test "empty state" =
  test [];
  [%expect
    {|
    ()
    ┌────┬──────────┐
    │ id │ Response │
    ├┬┬┬┬┼┬┬┬┬┬┬┬┬┬┬┤
    └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┘
    |}]
;;

let dummy_start_event ~id =
  Event.Started
    { id = Rpc_id.of_int id
    ; start_time = Time_ns.of_int_ns_since_epoch 1
    ; query = Sexp_of_provided (Sexp.Atom "<query>")
    ; path = "bonsai_path"
    ; rpc_kind = Rpc_kind.Normal { name = "rpc_name"; version = 0; interval = Dispatch }
    }
;;

let dummy_finish_event ~id ~response =
  Event.Finished { id = Rpc_id.of_int id; duration = Time_ns.Span.of_sec 1.0; response }
;;

let dummy_abort_event ~id =
  Event.Aborted { id = Rpc_id.of_int id; duration = Time_ns.Span.of_sec 1.0 }
;;

let dummy_response content =
  Ok (Or_no_sexp_of_provided.Sexp_of_provided (Sexp.Atom content))
;;

let%expect_test "adding a start event" =
  test [ dummy_start_event ~id:1 ];
  [%expect
    {|
    ((1
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>)) (status Running) (path bonsai_path))))
    ┌────┬──────────┐
    │ id │ Response │
    ├────┼──────────┤
    │ 1  │ Running  │
    └────┴──────────┘
    |}]
;;

let%expect_test "start -> successful finish event" =
  test
    [ dummy_start_event ~id:1
    ; dummy_finish_event
        ~id:1
        ~response:(Ok (Or_no_sexp_of_provided.Sexp_of_provided [%sexp "<response>"]))
    ];
  [%expect
    {|
    ((1
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>))
       (status
        (Finished (duration 1s) (response (Ok (Sexp_of_provided <response>)))))
       (path bonsai_path))))
    ┌────┬────────────────────────────────────────────────────────────────────────┐
    │ id │ Response                                                               │
    ├────┼────────────────────────────────────────────────────────────────────────┤
    │ 1  │ (Finished (duration 1s) (response (Ok (Sexp_of_provided <response>)))) │
    └────┴────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "start -> error finish event" =
  test
    [ dummy_start_event ~id:1
    ; dummy_finish_event
        ~id:1
        ~response:(error_s [%message "Beep boop an error occurred."])
    ];
  [%expect
    {|
    ((1
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>))
       (status
        (Finished (duration 1s)
         (response (Error "Beep boop an error occurred."))))
       (path bonsai_path))))
    ┌────┬────────────────────────────────────────────────────────────────────────────┐
    │ id │ Response                                                                   │
    ├────┼────────────────────────────────────────────────────────────────────────────┤
    │ 1  │ (Finished (duration 1s) (response (Error "Beep boop an error occurred."))) │
    └────┴────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "start -> abort event" =
  test [ dummy_start_event ~id:1; dummy_abort_event ~id:1 ];
  [%expect
    {|
    ((1
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>)) (status (Aborted (duration 1s)))
       (path bonsai_path))))
    ┌────┬─────────────────────────┐
    │ id │ Response                │
    ├────┼─────────────────────────┤
    │ 1  │ (Aborted (duration 1s)) │
    └────┴─────────────────────────┘
    |}]
;;

let%expect_test "Duplicated finished event" =
  (* NOTE: This test demonstrates existing behavior and may or not maybe be a bug.
     I currently believe that this _might_ be impossible. I think that 
     printing an error message in this situation is good. *)
  test
    [ dummy_start_event ~id:1
    ; dummy_finish_event ~id:1 ~response:(dummy_response "first")
    ; (* Second finish event for the same id! *)
      dummy_finish_event ~id:1 ~response:(dummy_response "second")
    ];
  [%expect
    {|
    Saw finish event for already finished event. Ignoring new event. (id 1)
    ((1
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>))
       (status (Finished (duration 1s) (response (Ok (Sexp_of_provided first)))))
       (path bonsai_path))))
    ┌────┬───────────────────────────────────────────────────────────────────┐
    │ id │ Response                                                          │
    ├────┼───────────────────────────────────────────────────────────────────┤
    │ 1  │ (Finished (duration 1s) (response (Ok (Sexp_of_provided first)))) │
    └────┴───────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Out-of-order events" =
  test
    [ dummy_start_event ~id:1
    ; dummy_start_event ~id:3
    ; dummy_start_event ~id:4
    ; dummy_abort_event ~id:3
    ; dummy_start_event ~id:2
    ; dummy_finish_event ~id:4 ~response:(dummy_response "first")
    ; dummy_finish_event ~id:2 ~response:(dummy_response "second")
    ];
  [%expect
    {|
    ((1
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>)) (status Running) (path bonsai_path)))
     (2
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>))
       (status
        (Finished (duration 1s) (response (Ok (Sexp_of_provided second)))))
       (path bonsai_path)))
     (3
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>)) (status (Aborted (duration 1s)))
       (path bonsai_path)))
     (4
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>))
       (status (Finished (duration 1s) (response (Ok (Sexp_of_provided first)))))
       (path bonsai_path))))
    ┌────┬────────────────────────────────────────────────────────────────────┐
    │ id │ Response                                                           │
    ├────┼────────────────────────────────────────────────────────────────────┤
    │ 1  │ Running                                                            │
    │ 2  │ (Finished (duration 1s) (response (Ok (Sexp_of_provided second)))) │
    │ 3  │ (Aborted (duration 1s))                                            │
    │ 4  │ (Finished (duration 1s) (response (Ok (Sexp_of_provided first))))  │
    └────┴────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Seeing a finish event before having seen a start event." =
  test
    [ dummy_start_event ~id:1
    ; dummy_finish_event
        ~id:2
        ~response:(Ok (Sexp_of_provided [%message "I should not appear"]))
    ];
  (* NOTE: If we have not previously seen the start event for an rpc previously before we
     do not do anything. This situation might happen when the dev tool panel is opened and then
     closed, although since the right behavior we want would be to [clear] the event queue
     regardless, I think ignoring this message is correct. *)
  [%expect
    {|
    ("Saw finished message for unknown rpc id, ignoring finish message."
     (id 2) (duration 1s)
     (response (Ok (Sexp_of_provided "I should not appear"))))
    ((1
      ((rpc_kind (Normal (name rpc_name) (version 0) (interval Dispatch)))
       (start_time "1970-01-01 00:00:00.000000001Z")
       (query (Sexp_of_provided <query>)) (status Running) (path bonsai_path))))
    ┌────┬──────────┐
    │ id │ Response │
    ├────┼──────────┤
    │ 1  │ Running  │
    └────┴──────────┘
    |}]
;;
