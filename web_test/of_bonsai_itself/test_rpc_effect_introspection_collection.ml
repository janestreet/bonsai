open! Core
open! Async_kernel
open Bonsai_web
open Async_rpc_kernel
open Bonsai_web_test_async
module State = Bonsai_introspection_protocol.State

(* At a high level this module contains tests that show that
   "sending" certain rpc's results in the "correct" introspection
   state being enqueued for the devtool panel to read. This happens
   for all the different kinds of rpcs available in rpc effect.

   Additionally, this also tests what happens in "race-condition"-ey
   situations. (i.e. what happens if start/stop-recording happens in
   the middle/edges of other rpc's running? What happens if there are many rpc's
   happening?/what if an rpc never finishes?) *)

(* We shadow the expect test config with startup/cleanup logic for each
   of the tests. This logic includes:

   - Resetting id counters for "nice" ids.
   - Asserting that the event queue is empty as a check that the test has seen
     all events.
   - Starting/stopping the recording so that recording is only enabled for the duration 
     of the test. *)
open struct
  module Expect_test_config = struct
    include Expect_test_config
    open Rpc_effect.For_introspection.For_testing
    open Bonsai_introspection_protocol.For_testing

    let assert_no_unpopped_events () =
      match pop_events' () with
      | [] -> ()
      | unpopped_events ->
        let unpopped_events =
          List.map ~f:Bonsai_introspection_protocol.Event.Stable.of_latest unpopped_events
        in
        raise_s
          [%message
            "test finished with unpopped events"
              (unpopped_events : Bonsai_introspection_protocol.Event.Stable.t list)]
    ;;

    let run f =
      run (fun () ->
        start_recording ();
        let%map () = f () in
        stop_recording ();
        reset_ids_for_testing ();
        assert_no_unpopped_events ())
    ;;
  end
end

let () = Async_js.init ()

module Rpcs = struct
  let reverse_rpc ~version =
    Rpc.Rpc.create
      ~name:"reverse-rpc"
      ~version
      ~bin_query:bin_string
      ~bin_response:bin_string
      ~include_in_error_count:Only_on_exn
  ;;

  let reverse_rpc_v1 = reverse_rpc ~version:1
  let reverse_rpc_v2 = reverse_rpc ~version:2
  let babel_reverse_rpc_v1 = Babel.Caller.Rpc.singleton reverse_rpc_v1
  let babel_reverse_rpc_v2 = Babel.Caller.Rpc.add babel_reverse_rpc_v1 ~rpc:reverse_rpc_v2

  module Streamable_rpc = struct
    module S = struct
      let name = "streamable-reverse-rpc"
      let version = 1

      type query = string [@@deriving bin_io]
      type response = string

      module Response = struct
        type t = string [@@deriving streamable ~version:1]
      end

      let client_pushes_back = false
    end

    include Streamable.Plain_rpc.Make (S)
  end

  module Polling_state_rpc = struct
    module Response = struct
      type t = string [@@deriving bin_io, sexp, diff]

      module Update = struct
        module Diff = struct
          type t = string [@@deriving sexp, bin_io]
        end

        type t = Diff.t list [@@deriving sexp, bin_io]
      end

      let update t diffs = Option.value ~default:t (List.last diffs)

      let diffs ~from ~to_ =
        print_s [%message "Computing diff" (from : string) (to_ : string)];
        [ to_ ]
      ;;
    end

    let rpc ~version =
      Polling_state_rpc.create
        ~name:"polling-state-rpc-reverse-rpc"
        ~version
        ~query_equal:[%equal: string]
        ~bin_query:bin_string
        (module Response)
    ;;

    let rpc_v1 = rpc ~version:1
    let rpc_v2 = rpc ~version:2
    let v1_caller = Versioned_polling_state_rpc.Client.create_caller rpc_v1
    let v2_caller = Versioned_polling_state_rpc.Client.create_caller rpc_v2
    let caller = Babel.Caller.of_list_decreasing_preference [ v2_caller; v1_caller ]
  end
end

let reverse_rpc_implementation =
  Rpc.Rpc.implement' Rpcs.reverse_rpc_v1 (fun _ query -> String.rev query)
;;

let streamable_reverse_rpc_implementation =
  Streamable.Plain_rpc.implement Rpcs.Streamable_rpc.rpc (fun _ string ->
    Deferred.Or_error.return (String.rev string))
;;

let polling_state_rpc_reverse_rpc_implementation =
  Rpc.Implementation.lift
    ~f:(fun connection -> connection, connection)
    (Polling_state_rpc.implement
       Rpcs.Polling_state_rpc.rpc_v1
       (fun _ string -> Deferred.return (String.rev string))
       ~on_client_and_server_out_of_sync:print_s)
;;

let lots_of_equal_signs = "========================="

let consume_and_apply_events ~state =
  let open Js_of_ocaml in
  let open Rpc_effect.For_introspection.For_testing in
  let events =
    pop_events ()
    |> Js.to_string
    |> Sexp.of_string
    |> [%of_sexp: Bonsai_introspection_protocol.Event.Stable.t list]
    |> List.map ~f:Bonsai_introspection_protocol.Event.Stable.to_latest
  in
  print_endline "Events";
  print_endline lots_of_equal_signs;
  List.iter events ~f:(fun event ->
    print_s
      [%sexp
        (Bonsai_introspection_protocol.For_testing.Event.reveal event
          : Bonsai_introspection_protocol.For_testing.Event.Unstable.t)];
    print_endline "");
  let new_state =
    List.fold events ~init:state ~f:Bonsai_introspection_protocol.State.apply_event
  in
  print_endline "State";
  print_endline lots_of_equal_signs;
  let () =
    match [%equal: State.t] state new_state with
    | true -> print_endline "No diff!"
    | false ->
      Expect_test_patdiff.print_patdiff_s
        ([%sexp_of: State.t] state)
        ([%sexp_of: State.t] new_state)
  in
  new_state
;;

module Dispatcher_handle = struct
  module Dispatcher_result_spec = struct
    type t = { send_rpc : query:string -> unit Effect.t }
    type incoming = Send_rpc of string

    let view _ = ""

    let incoming { send_rpc } = function
      | Send_rpc query -> send_rpc ~query
    ;;
  end

  type t =
    { handle : (Dispatcher_result_spec.t, Dispatcher_result_spec.incoming) Handle.t
    ; state : Bonsai_introspection_protocol.State.t
    }

  let component ~dispatch =
    let open Bonsai.Let_syntax in
    let%sub dispatch = dispatch in
    let%sub send_rpc =
      let%arr dispatch = dispatch in
      fun ~query ->
        let%bind.Effect () = Effect.print_s [%message "RPC started" (query : string)] in
        let%bind.Effect response = dispatch query in
        Effect.print_s [%message "RPC finished" (response : string Or_error.t)]
    in
    let%arr send_rpc = send_rpc in
    { Dispatcher_result_spec.send_rpc }
  ;;

  let create ?rpc_implementations ~dispatch () =
    let handle =
      Handle.create
        ?rpc_implementations
        ~connectors:(fun _ -> Bonsai_web.Rpc_effect.Connector.test_fallback)
        (module Dispatcher_result_spec)
        (component ~dispatch)
    in
    let state = Bonsai_introspection_protocol.State.empty in
    { handle; state }
  ;;
end

module Poller_handle = struct
  module Poller_result_spec = struct
    type t = { set_query : query:string -> unit Effect.t }
    type incoming = Set_query of string

    let view _ = ""

    let incoming { set_query } = function
      | Set_query query -> set_query ~query
    ;;
  end

  type t =
    { handle : (Poller_result_spec.t, Poller_result_spec.incoming) Handle.t
    ; state : Bonsai_introspection_protocol.State.t
    }

  let component
    ~(poller : string Value.t -> (string, string) Rpc_effect.Poll_result.t Computation.t)
    =
    let open Bonsai.Let_syntax in
    let%sub query, set_query = Bonsai.state_opt () in
    let%sub result_spec =
      let%arr set_query = set_query in
      let set_query ~query =
        let%bind.Effect () =
          Effect.of_sync_fun (fun () -> print_endline "Changing query") ()
        in
        set_query (Some query)
      in
      { Poller_result_spec.set_query }
    in
    let%sub () =
      match%sub query with
      | None -> Bonsai.const ()
      | Some query ->
        let%sub _result = poller query in
        Bonsai.const ()
    in
    return result_spec
  ;;

  let create ?rpc_implementations ~poller () =
    let handle =
      Handle.create
        ?rpc_implementations
        ~connectors:(fun _ -> Bonsai_web.Rpc_effect.Connector.test_fallback)
        (module Poller_result_spec)
        (component ~poller)
    in
    let state = Bonsai_introspection_protocol.State.empty in
    { handle; state }
  ;;
end

let%test_module "Normal Rpc.Rpc.dispatch" =
  (module struct
    let create_handle ?rpc_implementations () =
      Dispatcher_handle.create
        ?rpc_implementations
        ~dispatch:
          (Rpc_effect.Rpc.dispatcher
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.reverse_rpc_v1
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "dispatching a normal rpc - rpc does not exist error" =
      let { Dispatcher_handle.handle; state } = create_handle () in
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query capybara))
        ("RPC finished"
         (response
          (Error
           ((rpc_error (Unimplemented_rpc reverse-rpc (Version 1)))
            (connection_description <created-directly>) (rpc_name reverse-rpc)
            (rpc_version 1)))))
        |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path))

        (Finished (id 0) (duration 0s)
         (response
          (Error
           ((rpc_error (Unimplemented_rpc reverse-rpc (Version 1)))
            (connection_description <created-directly>) (rpc_name reverse-rpc)
            (rpc_version 1)))))

        State
        =========================
        -1,1 +1,19
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Normal
        +|      (name     reverse-rpc)
        +|      (version  1)
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished
        +|      (duration 0s)
        +|      (response (
        +|        Error (
        +|          (rpc_error (Unimplemented_rpc reverse-rpc (Version 1)))
        +|          (connection_description <created-directly>)
        +|          (rpc_name               reverse-rpc)
        +|          (rpc_version            1))))))
        +|    (path bonsai_path))))
        |}];
      return ()
    ;;

    let%expect_test "dispatching a normal rpc - rpc successful" =
      let { Dispatcher_handle.handle; state } =
        create_handle ~rpc_implementations:[ reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query capybara))
        ("RPC finished" (response (Ok arabypac)))
        |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,12
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Normal
        +|      (name     reverse-rpc)
        +|      (version  1)
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path))))
        |}];
      return ()
    ;;

    let%expect_test "dispatching a normal rpc - rpc does not finish immediately" =
      let response_ivar = Ivar.create () in
      let { Dispatcher_handle.handle; state } =
        create_handle
          ~rpc_implementations:
            [ Rpc.Rpc.implement Rpcs.reverse_rpc_v1 (fun _ _ -> Ivar.read response_ivar) ]
          ()
      in
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC started" (query capybara)) |}];
      let state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path))

        State
        =========================
        -1,1 +1,11
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Normal
        +|      (name     reverse-rpc)
        +|      (version  1)
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status Running)
        +|    (path   bonsai_path))))
        |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
      Ivar.fill_exn response_ivar "arabypac";
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC finished" (response (Ok arabypac))) |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Finished (id 0) (duration 10s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,11 +1,12
          ((
            0 (
              (rpc_kind (
                Normal
                (name     reverse-rpc)
                (version  1)
                (interval Dispatch)))
              (start_time "1970-01-01 00:00:00Z")
              (query (Sexp_of_provided capybara))
        -|    (status Running)
        +|    (status (
        +|      Finished (duration 10s) (response (Ok (Sexp_of_provided arabypac)))))
              (path bonsai_path))))
        |}];
      return ()
    ;;

    let%expect_test "dispatching a normal rpc - many rpc's all-at-once" =
      let { Dispatcher_handle.handle; state } =
        create_handle ~rpc_implementations:[ reverse_rpc_implementation ] ()
      in
      Handle.do_actions
        handle
        [ Send_rpc "capybara"; Send_rpc "corgi"; Send_rpc "basset hound" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query capybara))
        ("RPC started" (query corgi))
        ("RPC started" (query "basset hound"))
        ("RPC finished" (response (Ok igroc)))
        ("RPC finished" (response (Ok arabypac)))
        ("RPC finished" (response (Ok "dnuoh tessab")))
        |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path))

        (Started (id 1)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided corgi))
         (path bonsai_path))

        (Started (id 2)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z")
         (query (Sexp_of_provided "basset hound")) (path bonsai_path))

        (Finished (id 1) (duration 0s) (response (Ok (Sexp_of_provided igroc))))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        (Finished (id 2) (duration 0s)
         (response (Ok (Sexp_of_provided "dnuoh tessab"))))

        State
        =========================
        -1,1 +1,32
        -|()
        +|((0 (
        +|   (rpc_kind (
        +|     Normal
        +|     (name     reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:00Z")
        +|   (query (Sexp_of_provided capybara))
        +|   (status (
        +|     Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|   (path bonsai_path)))
        +| (1 (
        +|   (rpc_kind (
        +|     Normal
        +|     (name     reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:00Z")
        +|   (query (Sexp_of_provided corgi))
        +|   (status (Finished (duration 0s) (response (Ok (Sexp_of_provided igroc)))))
        +|   (path bonsai_path)))
        +| (2 (
        +|   (rpc_kind (
        +|     Normal
        +|     (name     reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:00Z")
        +|   (query (Sexp_of_provided "basset hound"))
        +|   (status (
        +|     Finished (duration 0s) (response (Ok (Sexp_of_provided "dnuoh tessab")))))
        +|   (path bonsai_path))))
        |}];
      return ()
    ;;

    let%expect_test {|dispatching a normal rpc - many rpc's out of order (Canterbury Park Race)|}
      =
      let capybara_ivar = Ivar.create () in
      let corgi_ivar = Ivar.create () in
      let basset_hound_ivar = Ivar.create () in
      let { Dispatcher_handle.handle; state } =
        create_handle
          ~rpc_implementations:
            [ Rpc.Rpc.implement Rpcs.reverse_rpc_v1 (fun _ s ->
                match s with
                | "corgi" ->
                  let%bind message = Ivar.read corgi_ivar in
                  failwith message
                | "basset hound" -> Ivar.read basset_hound_ivar
                | "capybara" -> Ivar.read capybara_ivar
                | s -> raise_s [%message "Unexpected test case input" ~input:(s : string)])
            ]
          ()
      in
      (* A corgi and a basset hound start a race as participants in Canterbury Park's 2024
         inagural race. *)
      Handle.do_actions handle [ Send_rpc "corgi"; Send_rpc "basset hound" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query corgi))
        ("RPC started" (query "basset hound"))
        |}];
      let state = consume_and_apply_events ~state in
      (* They are both running: *)
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided corgi))
         (path bonsai_path))

        (Started (id 1)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z")
         (query (Sexp_of_provided "basset hound")) (path bonsai_path))

        State
        =========================
        -1,1 +1,20
        -|()
        +|((0 (
        +|   (rpc_kind (
        +|     Normal
        +|     (name     reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:00Z")
        +|   (query (Sexp_of_provided corgi))
        +|   (status Running)
        +|   (path   bonsai_path)))
        +| (1 (
        +|   (rpc_kind (
        +|     Normal
        +|     (name     reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:00Z")
        +|   (query (Sexp_of_provided "basset hound"))
        +|   (status Running)
        +|   (path   bonsai_path))))
        |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
      (* A wild capybara jumps from the bleachers and joins the race. *)
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC started" (query capybara)) |}];
      let state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 2)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:10Z") (query (Sexp_of_provided capybara))
         (path bonsai_path))

        State
        =========================
        -4,17 +4,27
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided corgi))
             (status Running)
             (path   bonsai_path)))
           (1 (
             (rpc_kind (
               Normal
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided "basset hound"))
             (status Running)
        +|   (path   bonsai_path)))
        +| (2 (
        +|   (rpc_kind (
        +|     Normal
        +|     (name     reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:10Z")
        +|   (query (Sexp_of_provided capybara))
        +|   (status Running)
             (path   bonsai_path))))
        |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
      (* Basset hound crosses the finish line! *)
      Ivar.fill_exn basset_hound_ivar "dnuoh tessab";
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      let state = consume_and_apply_events ~state in
      (* Finished in a record 20s! *)
      [%expect
        {|
        ("RPC finished" (response (Ok "dnuoh tessab")))
        Events
        =========================
        (Finished (id 1) (duration 20s)
         (response (Ok (Sexp_of_provided "dnuoh tessab"))))

        State
        =========================
        -3,28 +3,31
               Normal
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided corgi))
             (status Running)
             (path   bonsai_path)))
           (1 (
             (rpc_kind (
               Normal
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided "basset hound"))
        -|   (status Running)
        +|   (status (
        +|     Finished
        +|     (duration 20s)
        +|     (response (Ok (Sexp_of_provided "dnuoh tessab")))))
             (path bonsai_path)))
           (2 (
             (rpc_kind (
               Normal
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:10Z")
             (query (Sexp_of_provided capybara))
             (status Running)
             (path   bonsai_path))))
        |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_ns 1.0);
      Ivar.fill_exn capybara_ivar "arabypac";
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      (* The capybara comes in close 2nd, picture finish, one nano-second behind. *)
      let state = consume_and_apply_events ~state in
      [%expect
        {|
        ("RPC finished" (response (Ok arabypac)))
        Events
        =========================
        (Finished (id 2) (duration 10.000000001s)
         (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -16,18 +16,21
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided "basset hound"))
             (status (
               Finished
               (duration 20s)
               (response (Ok (Sexp_of_provided "dnuoh tessab")))))
             (path bonsai_path)))
           (2 (
             (rpc_kind (
               Normal
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:10Z")
             (query (Sexp_of_provided capybara))
        -|   (status Running)
        +|   (status (
        +|     Finished
        +|     (duration 10.000000001s)
        +|     (response (Ok (Sexp_of_provided arabypac)))))
             (path bonsai_path))))
        |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
      (* Distracted by the cheers and pets of the crowd rooting for him,
         the corgi instead jumped into the bleachers. *)
      Ivar.fill_exn corgi_ivar {|Disqualified: Jumped into the bleachers.|};
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC finished"
         (response
          (Error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn
                (monitor.ml.Error
                 (Failure "Disqualified: Jumped into the bleachers."))))))
            (connection_description <created-directly>) (rpc_name reverse-rpc)
            (rpc_version 1)))))
        |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Finished (id 0) (duration 30.000000001s)
         (response
          (Error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn
                (monitor.ml.Error
                 (Failure "Disqualified: Jumped into the bleachers."))))))
            (connection_description <created-directly>) (rpc_name reverse-rpc)
            (rpc_version 1)))))

        State
        =========================
        -1,25 +1,38
          ((0 (
             (rpc_kind (
               Normal
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided corgi))
        -|   (status Running)
        +|   (status (
        +|     Finished
        +|     (duration 30.000000001s)
        +|     (response (
        +|       Error (
        +|         (rpc_error (
        +|           Uncaught_exn (
        +|             (location "server-side rpc computation")
        +|             (exn (
        +|               monitor.ml.Error (
        +|                 Failure "Disqualified: Jumped into the bleachers."))))))
        +|         (connection_description <created-directly>)
        +|         (rpc_name               reverse-rpc)
        +|         (rpc_version            1))))))
             (path bonsai_path)))
           (1 (
             (rpc_kind (
               Normal
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided "basset hound"))
             (status (
               Finished
               (duration 20s)
               (response (Ok (Sexp_of_provided "dnuoh tessab")))))
             (path bonsai_path)))
           (2 (
             (rpc_kind (
        |}];
      return ()
    ;;

    let%expect_test "rpc is sent, but recording is stopped before rpc finishes, rpc \
                     finishes and then starts recording again"
      =
      let open Rpc_effect.For_introspection.For_testing in
      let response_ivar = Ivar.create () in
      let { Dispatcher_handle.handle; state } =
        create_handle
          ~rpc_implementations:
            [ Rpc.Rpc.implement Rpcs.reverse_rpc_v1 (fun _ s ->
                match s with
                | "first-rpc" -> Ivar.read response_ivar
                | s -> return (String.rev s))
            ]
          ()
      in
      Handle.do_actions handle [ Send_rpc "first-rpc" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC started" (query first-rpc)) |}];
      let state = consume_and_apply_events ~state in
      (* first-rpc is sent and its start event was recorded. *)
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided first-rpc))
         (path bonsai_path))

        State
        =========================
        -1,1 +1,11
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Normal
        +|      (name     reverse-rpc)
        +|      (version  1)
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided first-rpc))
        +|    (status Running)
        +|    (path   bonsai_path))))
        |}];
      (* oh no! recording was stopped right before the rpc finished. tragic. *)
      stop_recording ();
      Ivar.fill_exn response_ivar "arabycap";
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC finished" (response (Ok arabycap))) |}];
      let state = consume_and_apply_events ~state in
      (* finish event was missed into the void. *)
      [%expect
        {|
        Events
        =========================
        State
        =========================
        No diff!
        |}];
      (* oh no again! A second rpc started right before recording was re-enabled. Its
         start event shall be lost to history. *)
      Handle.do_actions handle [ Send_rpc "second-rpc" ];
      start_recording ();
      Handle.do_actions handle [ Send_rpc "third-rpc"; Send_rpc "fourth-rpc" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      let _state = consume_and_apply_events ~state in
      (* This test currently shows current behavior. I think this behavior is correct, or
         rather, I think this is the least worst behavior. This only happens if
         stop_recording is called in the middle of the network panel actively recording,
         however, since stop_recording is meant to only be called after the devtool is
         closed, I think this is fine.

         - the finish event for first-rpc was missed and is forever unfinished.
         - the start event for second-rpc was missed and was never recorded. its finish event
           was observed, but ignored as no start event had been seen.
         - the third and fourth rpc's were seen and were recorded. *)
      [%expect
        {|
        ("RPC started" (query second-rpc))
        ("RPC started" (query third-rpc))
        ("RPC started" (query fourth-rpc))
        ("RPC finished" (response (Ok cpr-dnoces)))
        ("RPC finished" (response (Ok cpr-driht)))
        ("RPC finished" (response (Ok cpr-htruof)))
        Events
        =========================
        (Started (id 1)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided third-rpc))
         (path bonsai_path))

        (Started (id 2)
         (rpc_kind (Normal (name reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided fourth-rpc))
         (path bonsai_path))

        (Finished (id 1) (duration 0s) (response (Ok (Sexp_of_provided cpr-driht))))

        (Finished (id 2) (duration 0s) (response (Ok (Sexp_of_provided cpr-htruof))))

        State
        =========================
        -1,11 +1,32
          ((0 (
             (rpc_kind (
               Normal
               (name     reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided first-rpc))
             (status Running)
        +|   (path   bonsai_path)))
        +| (1 (
        +|   (rpc_kind (
        +|     Normal
        +|     (name     reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:00Z")
        +|   (query (Sexp_of_provided third-rpc))
        +|   (status (
        +|     Finished (duration 0s) (response (Ok (Sexp_of_provided cpr-driht)))))
        +|   (path bonsai_path)))
        +| (2 (
        +|   (rpc_kind (
        +|     Normal
        +|     (name     reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:00Z")
        +|   (query (Sexp_of_provided fourth-rpc))
        +|   (status (
        +|     Finished (duration 0s) (response (Ok (Sexp_of_provided cpr-htruof)))))
             (path bonsai_path))))
        |}];
      return ()
    ;;
  end)
;;

(* META comment on the tests below. The tests below test that
   we are able to correctly index information from the different
   kinds of rpc's that rpc_effect supports. The race-condition-ey
   protocol tests occurred above, and the tests below solely test that 
   we retrieve the right rpc kinds from the different rpc's in addition
   to letting us test getting unique data from individual rpc's in the future... *)

let%test_module "Rpc_effect.Rpc.babel_dispatcher" =
  (module struct
    let create_handle ?rpc_implementations () =
      Dispatcher_handle.create
        ?rpc_implementations
        ~dispatch:
          (Rpc_effect.Rpc.babel_dispatcher
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.babel_reverse_rpc_v2
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Indexing is correct." =
      let { Dispatcher_handle.handle; state } =
        create_handle ~rpc_implementations:[ reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query capybara))
        ("RPC finished" (response (Ok arabypac)))
        |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Babel
           (descriptions
            (((name reverse-rpc) (version 2)) ((name reverse-rpc) (version 1))))
           (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,13
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Babel
        +|      (descriptions (
        +|        ((name reverse-rpc) (version 2))
        +|        ((name reverse-rpc) (version 1))))
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path))))
        |}];
      return ()
    ;;
  end)
;;

let%test_module "Rpc_effect.Rpc.streamable_dispatcher" =
  (module struct
    let create_handle ?rpc_implementations () =
      Dispatcher_handle.create
        ?rpc_implementations
        ~dispatch:
          (Rpc_effect.Rpc.streamable_dispatcher
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.Streamable_rpc.rpc
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Indexing is correct." =
      let { Dispatcher_handle.handle; state } =
        create_handle ~rpc_implementations:[ streamable_reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query capybara))
        ("RPC finished" (response (Ok arabypac)))
        |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Streamable (name streamable-reverse-rpc) (version 1) (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,12
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Streamable
        +|      (name     streamable-reverse-rpc)
        +|      (version  1)
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path))))
        |}];
      return ()
    ;;
  end)
;;

let%test_module "Rpc_effect.Polling_state_rpc.dispatcher" =
  (module struct
    let create_handle ?rpc_implementations () =
      Dispatcher_handle.create
        ?rpc_implementations
        ~dispatch:
          (Rpc_effect.Polling_state_rpc.dispatcher
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.Polling_state_rpc.rpc_v1
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Indexing is correct." =
      let { Dispatcher_handle.handle; state } =
        create_handle
          ~rpc_implementations:[ polling_state_rpc_reverse_rpc_implementation ]
          ()
      in
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query capybara))
        ("RPC finished" (response (Ok arabypac)))
        |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Polling_state_rpc (name polling-state-rpc-reverse-rpc) (version 1)
           (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_y))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,12
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Polling_state_rpc
        +|      (name     polling-state-rpc-reverse-rpc)
        +|      (version  1)
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_y))))
        |}];
      return ()
    ;;

    let%expect_test "Indexing is correct. Does not finish immediately" =
      let response_ivar = Ivar.create () in
      let { Dispatcher_handle.handle; state } =
        create_handle
          ~rpc_implementations:
            [ Rpc.Implementation.lift
                ~f:(fun connection -> connection, connection)
                (Polling_state_rpc.implement
                   Rpcs.Polling_state_rpc.rpc_v1
                   (fun _ _ -> Ivar.read response_ivar)
                   ~on_client_and_server_out_of_sync:print_s)
            ]
          ()
      in
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC started" (query capybara)) |}];
      let state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Polling_state_rpc (name polling-state-rpc-reverse-rpc) (version 1)
           (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_y))

        State
        =========================
        -1,1 +1,11
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Polling_state_rpc
        +|      (name     polling-state-rpc-reverse-rpc)
        +|      (version  1)
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status Running)
        +|    (path   bonsai_path_y))))
        |}];
      Ivar.fill_exn response_ivar "arabypac";
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC finished" (response (Ok arabypac))) |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,11 +1,12
          ((
            0 (
              (rpc_kind (
                Polling_state_rpc
                (name     polling-state-rpc-reverse-rpc)
                (version  1)
                (interval Dispatch)))
              (start_time "1970-01-01 00:00:00Z")
              (query (Sexp_of_provided capybara))
        -|    (status Running)
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
              (path bonsai_path_y))))
        |}];
      return ()
    ;;

    let%expect_test "Indexing is correct. Does not finish immediately - many at the same \
                     time - rpc cancelling is recorded."
      =
      let response_a = Ivar.create () in
      let response_b = Ivar.create () in
      let { Dispatcher_handle.handle; state } =
        create_handle
          ~rpc_implementations:
            [ Rpc.Implementation.lift
                ~f:(fun connection -> connection, connection)
                (Polling_state_rpc.implement
                   Rpcs.Polling_state_rpc.rpc_v1
                   (fun _ string ->
                     match string with
                     | "a" -> Ivar.read response_a
                     | "b" -> Ivar.read response_b
                     | query -> raise_s [%message "unexpected query" (query : string)])
                   ~on_client_and_server_out_of_sync:print_s)
            ]
          ()
      in
      Handle.do_actions handle [ Send_rpc "a" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC started" (query a)) |}];
      let state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Polling_state_rpc (name polling-state-rpc-reverse-rpc) (version 1)
           (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided a))
         (path bonsai_path_y))

        State
        =========================
        -1,1 +1,11
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Polling_state_rpc
        +|      (name     polling-state-rpc-reverse-rpc)
        +|      (version  1)
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided a))
        +|    (status Running)
        +|    (path   bonsai_path_y))))
        |}];
      (* Changing the query "b". *)
      Handle.do_actions handle [ Send_rpc "b" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query b))
        ("RPC finished"
         (response
          (Error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn (monitor.ml.Error (Failure "this request was cancelled"))))))
            (connection_description <created-directly>)
            (rpc_name polling-state-rpc-reverse-rpc) (rpc_version 1)))))
        |}];
      (* This cancels the old query. *)
      let state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 1)
         (rpc_kind
          (Polling_state_rpc (name polling-state-rpc-reverse-rpc) (version 1)
           (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided b))
         (path bonsai_path_y))

        (Finished (id 0) (duration 0s)
         (response
          (Error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn (monitor.ml.Error (Failure "this request was cancelled"))))))
            (connection_description <created-directly>)
            (rpc_name polling-state-rpc-reverse-rpc) (rpc_version 1)))))

        State
        =========================
        -1,11 +1,31
          ((0 (
             (rpc_kind (
               Polling_state_rpc
               (name     polling-state-rpc-reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided a))
        +|   (status (
        +|     Finished
        +|     (duration 0s)
        +|     (response (
        +|       Error (
        +|         (rpc_error (
        +|           Uncaught_exn (
        +|             (location "server-side rpc computation")
        +|             (exn (monitor.ml.Error (Failure "this request was cancelled"))))))
        +|         (connection_description <created-directly>)
        +|         (rpc_name    polling-state-rpc-reverse-rpc)
        +|         (rpc_version 1))))))
        +|   (path bonsai_path_y)))
        +| (1 (
        +|   (rpc_kind (
        +|     Polling_state_rpc
        +|     (name     polling-state-rpc-reverse-rpc)
        +|     (version  1)
        +|     (interval Dispatch)))
        +|   (start_time "1970-01-01 00:00:00Z")
        +|   (query (Sexp_of_provided b))
             (status Running)
             (path   bonsai_path_y))))
        |}];
      (* Filling the old query does nothing. *)
      Ivar.fill_exn response_a "a";
      Handle.recompute_view handle;
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| |}];
      let state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        State
        =========================
        No diff!
        |}];
      (* Filling the new query is recorded. *)
      Ivar.fill_exn response_b "b";
      Handle.recompute_view handle;
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect {| ("RPC finished" (response (Ok b))) |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Finished (id 1) (duration 0s) (response (Ok (Sexp_of_provided b))))

        State
        =========================
        -14,18 +14,18
                   (rpc_error (
                     Uncaught_exn (
                       (location "server-side rpc computation")
                       (exn (monitor.ml.Error (Failure "this request was cancelled"))))))
                   (connection_description <created-directly>)
                   (rpc_name    polling-state-rpc-reverse-rpc)
                   (rpc_version 1))))))
             (path bonsai_path_y)))
           (1 (
             (rpc_kind (
               Polling_state_rpc
               (name     polling-state-rpc-reverse-rpc)
               (version  1)
               (interval Dispatch)))
             (start_time "1970-01-01 00:00:00Z")
             (query (Sexp_of_provided b))
        -|   (status Running)
        +|   (status (Finished (duration 0s) (response (Ok (Sexp_of_provided b)))))
             (path bonsai_path_y))))
        |}];
      return ()
    ;;
  end)
;;

let%test_module "Rpc_effect.Polling_state_rpc.babel_dispatcher" =
  (module struct
    let create_handle ?rpc_implementations () =
      Dispatcher_handle.create
        ?rpc_implementations
        ~dispatch:
          (Rpc_effect.Polling_state_rpc.babel_dispatcher
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.Polling_state_rpc.caller
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Indexing is correct." =
      let { Dispatcher_handle.handle; state } =
        create_handle
          ~rpc_implementations:[ polling_state_rpc_reverse_rpc_implementation ]
          ()
      in
      Handle.do_actions handle [ Send_rpc "capybara" ];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      [%expect
        {|
        ("RPC started" (query capybara))
        ("RPC finished" (response (Ok arabypac)))
        |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Babel_polling_state_rpc
           (descriptions
            (((name polling-state-rpc-reverse-rpc) (version 2))
             ((name polling-state-rpc-reverse-rpc) (version 1))))
           (interval Dispatch)))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_y))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,13
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Babel_polling_state_rpc
        +|      (descriptions (
        +|        ((name polling-state-rpc-reverse-rpc) (version 2))
        +|        ((name polling-state-rpc-reverse-rpc) (version 1))))
        +|      (interval Dispatch)))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_y))))
        |}];
      return ()
    ;;
  end)
;;

let recompute ~n handle =
  Deferred.for_ 0 ~to_:(n - 1) ~do_:(fun _ ->
    Handle.recompute_view handle;
    Async_kernel_scheduler.yield_until_no_jobs_remain ())
;;

let%test_module "Normal Rpc.Rpc.poll" =
  (module struct
    let create_handle ?rpc_implementations () =
      Poller_handle.create
        ?rpc_implementations
        ~poller:
          (Rpc_effect.Rpc.poll
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.reverse_rpc_v1
             ~equal_query:[%equal: string]
             ~every:(Time_ns.Span.of_sec 1.0)
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "dispatching a normal rpc - rpc successful (frame-by-frame)" =
      (* NOTE: This test shows that we are able to notice the frame delay
         from the on changes other tests recompute until stability is reached. *)
      let { Poller_handle.handle; state } =
        create_handle ~rpc_implementations:[ reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Set_query "capybara" ];
      [%expect {| Changing query |}];
      Handle.recompute_view handle;
      [%expect {| |}];
      let state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Normal (name reverse-rpc) (version 1) (interval (Poll (every 1s)))))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_x_y_x_x))

        State
        =========================
        -1,1 +1,11
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Normal
        +|      (name    reverse-rpc)
        +|      (version 1)
        +|      (interval (Poll (every 1s)))))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status Running)
        +|    (path   bonsai_path_x_y_x_x))))
        |}];
      let%bind () = recompute ~n:1 handle in
      [%expect {| |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,11 +1,12
          ((
            0 (
              (rpc_kind (
                Normal
                (name    reverse-rpc)
                (version 1)
                (interval (Poll (every 1s)))))
              (start_time "1970-01-01 00:00:00Z")
              (query (Sexp_of_provided capybara))
        -|    (status Running)
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
              (path bonsai_path_x_y_x_x))))
        |}];
      return ()
    ;;

    let%expect_test "Rpc_effect.Rpc.poll" =
      let { Poller_handle.handle; state } =
        create_handle ~rpc_implementations:[ reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Set_query "capybara" ];
      let%bind () = recompute ~n:2 handle in
      [%expect {| Changing query |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Normal (name reverse-rpc) (version 1) (interval (Poll (every 1s)))))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_x_y_x_x))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,12
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Normal
        +|      (name    reverse-rpc)
        +|      (version 1)
        +|      (interval (Poll (every 1s)))))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_x_y_x_x))))
        |}];
      return ()
    ;;

    let create_handle_for_until_ok ?rpc_implementations () =
      Poller_handle.create
        ?rpc_implementations
        ~poller:
          (Rpc_effect.Rpc.poll_until_ok
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.reverse_rpc_v1
             ~equal_query:[%equal: string]
             ~retry_interval:(Time_ns.Span.of_sec 1.0)
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Rpc_effect.Rpc.poll_until_ok" =
      let { Poller_handle.handle; state } =
        create_handle_for_until_ok ~rpc_implementations:[ reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Set_query "capybara" ];
      let%bind () = recompute ~n:2 handle in
      [%expect {| Changing query |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Normal (name reverse-rpc) (version 1)
           (interval (Poll_until_ok (retry_interval 1s)))))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_x_y_x_x))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,12
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Normal
        +|      (name    reverse-rpc)
        +|      (version 1)
        +|      (interval (Poll_until_ok (retry_interval 1s)))))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_x_y_x_x))))
        |}];
      return ()
    ;;
  end)
;;

let%test_module "Rpc_effect.Rpc.babel_poll and babel_poll_until_ok" =
  (module struct
    let create_handle ?rpc_implementations () =
      Poller_handle.create
        ?rpc_implementations
        ~poller:
          (Rpc_effect.Rpc.babel_poll
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.babel_reverse_rpc_v2
             ~equal_query:[%equal: string]
             ~every:(Time_ns.Span.of_sec 1.0)
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Rpc_effect.Rpc.babel_poll" =
      let { Poller_handle.handle; state } =
        create_handle ~rpc_implementations:[ reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Set_query "capybara" ];
      let%bind () = recompute ~n:2 handle in
      [%expect {| Changing query |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Babel
           (descriptions
            (((name reverse-rpc) (version 2)) ((name reverse-rpc) (version 1))))
           (interval (Poll (every 1s)))))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_x_y_x_x))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,13
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Babel
        +|      (descriptions (
        +|        ((name reverse-rpc) (version 2))
        +|        ((name reverse-rpc) (version 1))))
        +|      (interval (Poll (every 1s)))))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_x_y_x_x))))
        |}];
      return ()
    ;;

    let create_until_ok_handle ?rpc_implementations () =
      Poller_handle.create
        ?rpc_implementations
        ~poller:
          (Rpc_effect.Rpc.babel_poll_until_ok
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.babel_reverse_rpc_v2
             ~equal_query:[%equal: string]
             ~retry_interval:(Time_ns.Span.of_sec 1.0)
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Rpc_effect.Rpc.babel_poll_until_ok" =
      let { Poller_handle.handle; state } =
        create_until_ok_handle ~rpc_implementations:[ reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Set_query "capybara" ];
      let%bind () = recompute ~n:2 handle in
      [%expect {| Changing query |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Babel
           (descriptions
            (((name reverse-rpc) (version 2)) ((name reverse-rpc) (version 1))))
           (interval (Poll_until_ok (retry_interval 1s)))))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_x_y_x_x))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,13
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Babel
        +|      (descriptions (
        +|        ((name reverse-rpc) (version 2))
        +|        ((name reverse-rpc) (version 1))))
        +|      (interval (Poll_until_ok (retry_interval 1s)))))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_x_y_x_x))))
        |}];
      return ()
    ;;
  end)
;;

let%test_module "Rpc_effect.Polling_state_rpc.poll and babel_poll" =
  (module struct
    let create_handle ?rpc_implementations () =
      Poller_handle.create
        ?rpc_implementations
        ~poller:
          (Rpc_effect.Polling_state_rpc.poll
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.Polling_state_rpc.rpc_v1
             ~equal_query:[%equal: string]
             ~every:(Time_ns.Span.of_sec 1.0)
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Rpc_effect.Polling_state_rpc.poll" =
      let { Poller_handle.handle; state } =
        create_handle
          ~rpc_implementations:[ polling_state_rpc_reverse_rpc_implementation ]
          ()
      in
      Handle.do_actions handle [ Set_query "capybara" ];
      let%bind () = recompute ~n:2 handle in
      [%expect {| Changing query |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Polling_state_rpc (name polling-state-rpc-reverse-rpc) (version 1)
           (interval (Poll (every 1s)))))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_x_y_x_x))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,12
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Polling_state_rpc
        +|      (name    polling-state-rpc-reverse-rpc)
        +|      (version 1)
        +|      (interval (Poll (every 1s)))))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_x_y_x_x))))
        |}];
      return ()
    ;;

    let create_babel_handle ?rpc_implementations () =
      Poller_handle.create
        ?rpc_implementations
        ~poller:
          (Rpc_effect.Polling_state_rpc.babel_poll
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.Polling_state_rpc.caller
             ~equal_query:[%equal: string]
             ~every:(Time_ns.Span.of_sec 1.0)
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Rpc_effect.Polling_state_rpc.babel_poll" =
      let { Poller_handle.handle; state } =
        create_babel_handle
          ~rpc_implementations:[ polling_state_rpc_reverse_rpc_implementation ]
          ()
      in
      Handle.do_actions handle [ Set_query "capybara" ];
      let%bind () = recompute ~n:2 handle in
      [%expect {| Changing query |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Babel
           (descriptions
            (((name polling-state-rpc-reverse-rpc) (version 2))
             ((name polling-state-rpc-reverse-rpc) (version 1))))
           (interval (Poll (every 1s)))))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_x_y_x_x))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,13
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Babel
        +|      (descriptions (
        +|        ((name polling-state-rpc-reverse-rpc) (version 2))
        +|        ((name polling-state-rpc-reverse-rpc) (version 1))))
        +|      (interval (Poll (every 1s)))))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_x_y_x_x))))
        |}];
      return ()
    ;;
  end)
;;

let%test_module "Rpc_effect.Rpc.streamable_poll" =
  (module struct
    let create_handle ?rpc_implementations () =
      Poller_handle.create
        ?rpc_implementations
        ~poller:
          (Rpc_effect.Rpc.streamable_poll
             ~sexp_of_query:[%sexp_of: string]
             ~sexp_of_response:[%sexp_of: string]
             Rpcs.Streamable_rpc.rpc
             ~equal_query:[%equal: string]
             ~every:(Time_ns.Span.of_sec 1.0)
             ~where_to_connect:Self)
        ()
    ;;

    let%expect_test "Rpc_effect.Rpc.streamable_poll" =
      let { Poller_handle.handle; state } =
        create_handle ~rpc_implementations:[ streamable_reverse_rpc_implementation ] ()
      in
      Handle.do_actions handle [ Set_query "capybara" ];
      let%bind () = recompute ~n:2 handle in
      [%expect {| Changing query |}];
      let _state = consume_and_apply_events ~state in
      [%expect
        {|
        Events
        =========================
        (Started (id 0)
         (rpc_kind
          (Streamable (name streamable-reverse-rpc) (version 1)
           (interval (Poll (every 1s)))))
         (start_time "1970-01-01 00:00:00Z") (query (Sexp_of_provided capybara))
         (path bonsai_path_x_y_x_x))

        (Finished (id 0) (duration 0s) (response (Ok (Sexp_of_provided arabypac))))

        State
        =========================
        -1,1 +1,12
        -|()
        +|((
        +|  0 (
        +|    (rpc_kind (
        +|      Streamable
        +|      (name    streamable-reverse-rpc)
        +|      (version 1)
        +|      (interval (Poll (every 1s)))))
        +|    (start_time "1970-01-01 00:00:00Z")
        +|    (query (Sexp_of_provided capybara))
        +|    (status (
        +|      Finished (duration 0s) (response (Ok (Sexp_of_provided arabypac)))))
        +|    (path bonsai_path_x_y_x_x))))
        |}];
      return ()
    ;;
  end)
;;
