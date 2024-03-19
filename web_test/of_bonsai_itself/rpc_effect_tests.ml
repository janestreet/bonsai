open! Core
open! Async_kernel
open Bonsai_web
open Bonsai_web_test
open Async_rpc_kernel
open Async_js_test

let () = Async_js.init ()

let rpc_a =
  Rpc.Rpc.create
    ~name:"a"
    ~version:0
    ~bin_query:bin_int
    ~bin_response:bin_int
    ~include_in_error_count:Only_on_exn
;;

let rpc_b =
  Rpc.Rpc.create
    ~name:"b"
    ~version:0
    ~bin_query:bin_int
    ~bin_response:bin_int
    ~include_in_error_count:Only_on_exn
;;

let babel_rpc_a = Babel.Caller.Rpc.singleton rpc_a
let babel_rpc_b = Babel.Caller.Rpc.add babel_rpc_a ~rpc:rpc_b

module Diffable_int = struct
  type t = int [@@deriving sexp, bin_io]

  module Update = struct
    module Diff = struct
      type t = int [@@deriving sexp, bin_io]
    end

    type t = Diff.t list [@@deriving sexp, bin_io]
  end

  let update t diffs = Option.value ~default:t (List.last diffs)

  let diffs ~from ~to_ =
    print_s [%message "Computing diff" (from : int) (to_ : int)];
    [ to_ ]
  ;;
end

let polling_state_rpc =
  Polling_state_rpc.create
    ~name:"polling_state_rpc_a"
    ~version:0
    ~query_equal:[%equal: int]
    ~bin_query:bin_int
    (module Diffable_int)
;;

module Streamable_plain_rpc = struct
  module Response = struct
    module T = struct
      type t = int Map.M(Int).t [@@deriving bin_io, sexp_of]

      module Update = struct
        module Diff = struct
          type t =
            | Set of
                { key : int
                ; data : int
                }
            | Remove of { key : int }
          [@@deriving sexp, bin_io]
        end

        type t = Diff.t list [@@deriving sexp, bin_io]
      end

      let update t (diffs : Update.t) =
        List.fold diffs ~init:t ~f:(fun t (diff : Update.Diff.t) ->
          match diff with
          | Set { key; data } -> Map.set t ~key ~data
          | Remove { key } -> Map.remove t key)
      ;;

      let diffs ~from ~to_ =
        Map.symmetric_diff from to_ ~data_equal:[%equal: int]
        |> Sequence.map ~f:(fun (key, change) ->
             match change with
             | `Left _value -> Update.Diff.Remove { key }
             | `Right data | `Unequal (_, data) -> Update.Diff.Set { key; data })
        |> Sequence.to_list
      ;;

      let to_diffs t = diffs ~from:(Map.empty (module Int)) ~to_:t
      let of_diffs diffs = update (Map.empty (module Int)) diffs
    end

    include T
    include Legacy_diffable.Make_streamable_rpc (T) (T.Update.Diff)
  end

  include Streamable.Plain_rpc.Make (struct
    let name = "streamable-plain-rpc"
    let version = 0
    let client_pushes_back = false

    module Response = Response

    type query = unit [@@deriving bin_io]
    type response = Response.t
  end)
end

let async_do_actions handle actions =
  Handle.do_actions handle actions;
  Async_kernel_scheduler.yield_until_no_jobs_remain ()
;;

module Int_to_int_or_error = struct
  type t = int -> int Or_error.t Effect.t
  type incoming = int

  let view _ = ""

  let incoming f query =
    let%bind.Effect result = f query in
    Effect.print_s ([%sexp_of: int Or_error.t] result)
  ;;
end

let%expect_test "test fallback" =
  let computation = Rpc_effect.Rpc.dispatcher rpc_a ~where_to_connect:Self in
  let handle =
    Handle.create
      ~connectors:(fun _ -> Bonsai_web.Rpc_effect.Connector.test_fallback)
      (module Int_to_int_or_error)
      computation
  in
  (* Invoking the RPC before providing an implementation of it to the handle
     will yield an error as a response. *)
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect
    {|
    (Error
     ((rpc_error (Unimplemented_rpc a (Version 0)))
      (connection_description <created-directly>) (rpc_name a) (rpc_version 0)))
    |}];
  Deferred.unit
;;

let%expect_test "provided RPC" =
  let computation = Rpc_effect.Rpc.dispatcher rpc_a ~where_to_connect:Self in
  (* By providing an implementation to the handle, we get control over the
     value returned by the RPC. *)
  let handle =
    Handle.create
      ~rpc_implementations:[ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ]
      (module Int_to_int_or_error)
      computation
  in
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  Deferred.unit
;;

let%expect_test "not provided RPC" =
  let computation = Rpc_effect.Rpc.dispatcher rpc_a ~where_to_connect:Self in
  let handle =
    Handle.create ~rpc_implementations:[] (module Int_to_int_or_error) computation
  in
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect
    {|
    (Error
     ((rpc_error (Unimplemented_rpc a (Version 0)))
      (connection_description <created-directly>) (rpc_name a) (rpc_version 0)))
    |}];
  Deferred.unit
;;

let%expect_test "latest version of a babel RPC" =
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_a ~where_to_connect:Self in
  let handle =
    Handle.create
      ~rpc_implementations:[ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ]
      (module Int_to_int_or_error)
      computation
  in
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  Deferred.unit
;;

let%expect_test "previous version of a babel RPC" =
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_b ~where_to_connect:Self in
  let handle =
    Handle.create
      ~rpc_implementations:[ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ]
      (module Int_to_int_or_error)
      computation
  in
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  Deferred.unit
;;

let%expect_test "streamable plain rpc works on a type that isn't atomic" =
  let computation =
    Rpc_effect.Rpc.streamable_dispatcher Streamable_plain_rpc.rpc ~where_to_connect:Self
  in
  let handle =
    Handle.create
      ~rpc_implementations:
        [ Streamable.Plain_rpc.implement Streamable_plain_rpc.rpc (fun _ () ->
            Map.of_alist_exn (module Int) [ 1, 2; 3, 4 ] |> Deferred.Or_error.return)
        ]
      (module struct
        type t = unit -> Streamable_plain_rpc.Response.t Or_error.t Effect.t
        type incoming = unit

        let view _ = ""

        let incoming f () =
          let%bind.Effect result = f () in
          Effect.print_s ([%sexp_of: Streamable_plain_rpc.Response.t Or_error.t] result)
        ;;
      end)
      computation
  in
  let%bind.Deferred () = async_do_actions handle [ () ] in
  [%expect {| (Ok ((1 2) (3 4))) |}];
  Deferred.unit
;;

let incrementing_polling_state_rpc_implementation ?block_on () =
  let count = ref 0 in
  Rpc.Implementation.lift
    ~f:(fun connection -> connection, connection)
    (Polling_state_rpc.implement
       polling_state_rpc
       ~on_client_and_server_out_of_sync:
         (Expect_test_helpers_core.print_s ~hide_positions:true)
       ~for_first_request:(fun _ query ->
         let%bind () =
           match block_on with
           | Some bvar -> Bvar.wait bvar
           | None -> return ()
         in
         print_s [%message "For first request" (query : int)];
         incr count;
         return (query * !count))
       (fun _ query ->
         incr count;
         return (query * !count)))
;;

let%expect_test "polling_state_rpc" =
  let computation =
    Rpc_effect.Polling_state_rpc.dispatcher polling_state_rpc ~where_to_connect:Self
  in
  let handle =
    Handle.create
      ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 1 ] in
  [%expect {|
    ("For first request" (query 1))
    (Ok 1)
    |}];
  let%bind () = async_do_actions handle [ 1 ] in
  [%expect {|
    ("Computing diff" (from 1) (to_ 2))
    (Ok 2)
    |}];
  Deferred.unit
;;

let%expect_test "inactive delivery of a response will be ignored when \
                 [clear_when_deactivated] is true"
  =
  let is_active = Bonsai.Var.create true in
  let block = Bvar.create () in
  let computation =
    let open Bonsai.Let_syntax in
    if%sub Bonsai.Var.value is_active
    then (
      let%sub status =
        Rpc_effect.Polling_state_rpc.poll
          polling_state_rpc
          ~equal_query:[%equal: int]
          ~equal_response:[%equal: int]
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          ~clear_when_deactivated:true
          (Value.return 0)
      in
      return (status >>| Option.some))
    else Bonsai.const None
  in
  let handle =
    Handle.create
      ~rpc_implementations:
        [ incrementing_polling_state_rpc_implementation ~block_on:block () ]
      (Result_spec.sexp
         (module struct
           type t = (int, int) Rpc_effect.Poll_result.t option [@@deriving sexp_of]
         end))
      computation
  in
  Handle.show handle;
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect
    {|
    (((last_ok_response ()) (last_error ()) (inflight_query ())
      (refresh <opaque>)))
    |}];
  Bonsai.Var.set is_active false;
  Handle.show handle;
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect {| () |}];
  Handle.show handle;
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect {| () |}];
  Bonsai.Var.set is_active true;
  Handle.show handle;
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect
    {|
    (((last_ok_response ()) (last_error ()) (inflight_query ())
      (refresh <opaque>)))
    |}];
  Deferred.unit
;;

let%expect_test "BUG: completing an RPC at the same time as a disconnect" =
  let is_active = Bonsai.Var.create true in
  let block = Bvar.create () in
  let computation =
    let open Bonsai.Let_syntax in
    if%sub Bonsai.Var.value is_active
    then (
      let%sub status =
        Rpc_effect.Polling_state_rpc.poll
          polling_state_rpc
          ~equal_query:[%equal: int]
          ~equal_response:[%equal: int]
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Value.return 0)
      in
      return (status >>| Option.some))
    else Bonsai.const None
  in
  let handle =
    Handle.create
      ~rpc_implementations:
        [ incrementing_polling_state_rpc_implementation ~block_on:block () ]
      (Result_spec.sexp
         (module struct
           type t = (int, int) Rpc_effect.Poll_result.t option [@@deriving sexp_of]
         end))
      computation
  in
  Handle.show handle;
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect
    {|
    (((last_ok_response ()) (last_error ()) (inflight_query ())
      (refresh <opaque>)))
    |}];
  Bonsai.Var.set is_active false;
  Bvar.broadcast block ();
  Handle.show handle;
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect {|
    ()
    ("For first request" (query 0))
    |}];
  Bonsai.Var.set is_active true;
  Handle.show handle;
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect
    {|
    (((last_ok_response ())
      (last_error
       ((0
         ((rpc_error
           (Uncaught_exn
            ((location "server-side rpc computation")
             (exn
              (monitor.ml.Error
               ("Ivar.fill_exn called on full ivar" (t (Full _))))))))
          (connection_description <created-directly>)
          (rpc_name polling_state_rpc_a) (rpc_version 0)))))
      (inflight_query ()) (refresh <opaque>)))
    |}];
  Deferred.unit
;;

let%expect_test "multiple polling_state_rpc" =
  let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 10, () ]) in
  let map = Bonsai.Var.value map_var in
  let computation =
    let open Bonsai.Let_syntax in
    Bonsai.assoc
      (module Int)
      map
      ~f:(fun key _data ->
        let%sub dispatcher =
          Rpc_effect.Polling_state_rpc.dispatcher polling_state_rpc ~where_to_connect:Self
        in
        let%arr dispatcher = dispatcher
        and key = key in
        dispatcher key)
  in
  let handle =
    Handle.create
      ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
      (module struct
        type t = int Or_error.t Effect.t Int.Map.t
        type incoming = int

        let view _ = ""

        let incoming t query =
          match Map.find t query with
          | Some effect ->
            let%bind.Effect result = effect in
            Effect.print_s ([%sexp_of: int Or_error.t] result)
          | None -> Effect.print_s [%message "Query does not exist in map" (query : int)]
        ;;
      end)
      computation
  in
  (* Since the initial query for each entry of the map does not trigger a diff,
     we know that each one has a different polling_state_rpc client. *)
  let%bind () = async_do_actions handle [ 1 ] in
  [%expect {|
    ("For first request" (query 1))
    (Ok 1)
    |}];
  let%bind () = async_do_actions handle [ 2 ] in
  [%expect {|
    ("For first request" (query 2))
    (Ok 4)
    |}];
  let%bind () = async_do_actions handle [ 10 ] in
  [%expect {|
    ("For first request" (query 10))
    (Ok 30)
    |}];
  let%bind () = async_do_actions handle [ 10 ] in
  [%expect {|
    ("Computing diff" (from 30) (to_ 40))
    (Ok 40)
    |}];
  Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 10);
  Handle.recompute_view handle;
  let%bind () = async_do_actions handle [ 10 ] in
  [%expect {| ("Query does not exist in map" (query 10)) |}];
  Bonsai.Var.update map_var ~f:(fun map -> Map.set map ~key:10 ~data:());
  Handle.recompute_view handle;
  (* Having been de-activated, this map entry does not trigger a diff
     computation, thus demonstrating that the server probably isn't holding
     onto data about this client.. *)
  let%bind () = async_do_actions handle [ 10 ] in
  [%expect {|
    ("For first request" (query 10))
    (Ok 50)
    |}];
  Deferred.unit
;;

let create_connection implementations =
  let to_server = Pipe.create () in
  let to_client = Pipe.create () in
  let one_connection implementations pipe_to pipe_from =
    let transport =
      Pipe_transport.create Pipe_transport.Kind.string (fst pipe_to) (snd pipe_from)
    in
    let%bind conn =
      Rpc.Connection.create ?implementations ~connection_state:Fn.id transport
    in
    return (Result.ok_exn conn)
  in
  don't_wait_for
    (let%bind server_conn =
       one_connection
         (Some (Rpc.Implementations.create_exn ~implementations ~on_unknown_rpc:`Continue))
         to_server
         to_client
     in
     Rpc.Connection.close_finished server_conn);
  let%map connection = one_connection None to_client to_server in
  Or_error.return connection
;;

let%expect_test "disconnect and re-connect async_durable" =
  let is_broken = ref false in
  let implementations =
    ref (Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_a (fun _ _query -> 0) ])
  in
  let connector =
    Rpc_effect.Connector.async_durable
      (Async_durable.create
         ~to_create:(fun () ->
           is_broken := false;
           create_connection !implementations)
         ~is_broken:(fun _ -> !is_broken)
         ())
  in
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_b ~where_to_connect:Self in
  let handle =
    Handle.create
      ~connectors:(fun _ -> connector)
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  is_broken := true;
  implementations
    := Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_b (fun _ _query -> 1) ];
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 1) |}];
  return ()
;;

let%expect_test "disconnect and re-connect persistent_connection" =
  let module Conn =
    Persistent_connection_kernel.Make (struct
      type t = Rpc.Connection.t

      let close t = Rpc.Connection.close t
      let is_closed t = Rpc.Connection.is_closed t
      let close_finished t = Rpc.Connection.close_finished t
    end)
  in
  let implementations =
    ref (Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_a (fun _ _query -> 0) ])
  in
  let connection =
    Conn.create
      ~server_name:"test_server"
      ~connect:(fun () -> create_connection !implementations)
      ~address:(module Unit)
      (fun () -> Deferred.Or_error.return ())
  in
  let connector = Rpc_effect.Connector.persistent_connection (module Conn) connection in
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_b ~where_to_connect:Self in
  let handle =
    Handle.create
      ~connectors:(fun _ -> connector)
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  let%bind () =
    let connection = Option.value_exn (Conn.current_connection connection) in
    let%bind () = Rpc.Connection.close connection in
    Rpc.Connection.close_finished connection
  in
  implementations
    := Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_b (fun _ _query -> 1) ];
  let%bind _connection = Conn.connected connection in
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 1) |}];
  return ()
;;

let%expect_test "disconnect and re-connect with polling_state_rpc" =
  let module Conn =
    Persistent_connection_kernel.Make (struct
      type t = Rpc.Connection.t

      let close t = Rpc.Connection.close t
      let is_closed t = Rpc.Connection.is_closed t
      let close_finished t = Rpc.Connection.close_finished t
    end)
  in
  let implementations = [ incrementing_polling_state_rpc_implementation () ] in
  let connection =
    Conn.create
      ~server_name:"test_server"
      ~connect:(fun () -> create_connection implementations)
      ~address:(module Unit)
      (fun () -> Deferred.Or_error.return ())
  in
  let connector = Rpc_effect.Connector.persistent_connection (module Conn) connection in
  let computation =
    Rpc_effect.Polling_state_rpc.dispatcher polling_state_rpc ~where_to_connect:Self
  in
  let handle =
    Handle.create
      ~connectors:(fun _ -> connector)
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 1 ] in
  [%expect {|
    ("For first request" (query 1))
    (Ok 1)
    |}];
  let%bind () =
    let connection = Option.value_exn (Conn.current_connection connection) in
    let%bind () = Rpc.Connection.close connection in
    Rpc.Connection.close_finished connection
  in
  let%bind _connection = Conn.connected connection in
  let%bind () = async_do_actions handle [ 2 ] in
  [%expect {|
    ("For first request" (query 2))
    (Ok 4)
    |}];
  return ()
;;

let%test_module "versioned polling state rpc" =
  (module struct
    module Response = struct
      type t = string [@@deriving bin_io]

      module Update = String

      let diffs ~from:_ ~to_ = to_
      let update _ update = update
    end

    (* this module contains the _old_ implementation of V1 without the conversion functor *)
    module V1_old = struct
      module Response = struct
        include Int.Stable.V1
        module Update = Int.Stable.V1

        let diffs ~from:_ ~to_ = to_
        let update _prev next = next
      end

      let rpc =
        Polling_state_rpc.create
          ~name:"foo"
          ~version:1
          ~query_equal:[%equal: int]
          ~bin_query:bin_int
          (module Response)
      ;;
    end

    module V1 = struct
      module Response =
        Versioned_polling_state_rpc.Make_stable_response
          (Response)
          (V1_old.Response (* This is modeling a V1 that used ints instead of strings *))
          (struct
            let to_stable = Int.of_string
            let of_stable = Int.to_string

            module Update = struct
              let to_stable = Int.of_string
              let of_stable = Int.to_string
            end
          end)

      let rpc =
        Polling_state_rpc.create
          ~name:"foo"
          ~version:1
          ~query_equal:[%equal: int]
          ~bin_query:bin_int
          (module Response)
      ;;
    end

    module V2 = struct
      let rpc =
        Polling_state_rpc.create
          ~name:"foo"
          ~version:2
          ~query_equal:[%equal: int]
          ~bin_query:bin_int
          (module Response)
      ;;
    end

    module Erased_implementation = struct
      type t =
        | T :
            { rpc : (int, 'result) Polling_state_rpc.t
            ; latest_result_of_int : int -> 'result
            }
            -> t
    end

    let implementations rpcs =
      let implement (Erased_implementation.T { rpc; latest_result_of_int }) =
        Polling_state_rpc.implement
          ~on_client_and_server_out_of_sync:print_s
          rpc
          (fun (_ : Rpc.Connection.t) query ->
          let rpc =
            Polling_state_rpc.babel_generic_rpc rpc |> Babel.Generic_rpc.description
          in
          print_s [%message (rpc : Rpc.Description.t)];
          latest_result_of_int (query * 2) |> Deferred.return)
      in
      List.map rpcs ~f:implement
    ;;

    let v1_caller = Versioned_polling_state_rpc.Client.create_caller V1.rpc
    let v2_caller = Versioned_polling_state_rpc.Client.create_caller V2.rpc

    module Spec = struct
      type t = { dispatch : int -> string Or_error.t Effect.t }
      type incoming = Query of int

      let view _ = ""

      let incoming t incoming =
        match incoming with
        | Query query ->
          let%bind.Effect result = t.dispatch query in
          Effect.print_s ([%sexp_of: string Or_error.t] result)
      ;;
    end

    let setup_test_env ~rpcs_on_server ~rpcs_on_client =
      let caller = Babel.Caller.of_list_decreasing_preference rpcs_on_client in
      let make_implementations rpcs =
        Versioned_rpc.Menu.add
          (List.map
             (implementations rpcs)
             ~f:(Rpc.Implementation.lift ~f:(fun connection -> connection, connection)))
      in
      let is_broken = ref false in
      let implementations = ref (make_implementations rpcs_on_server) in
      let connector =
        Rpc_effect.Connector.async_durable
          (Async_durable.create
             ~to_create:(fun () ->
               is_broken := false;
               print_endline "creating rpc connection";
               create_connection !implementations)
             ~is_broken:(fun _ -> !is_broken)
             ())
      in
      let activated = Bonsai.Var.create true in
      let computation =
        let open Bonsai.Let_syntax in
        let%sub dispatch =
          match%sub Bonsai.Var.value activated with
          | true ->
            Rpc_effect.Polling_state_rpc.babel_dispatcher caller ~where_to_connect:Self
          | false ->
            Bonsai.const
              (Effect.of_sync_fun (fun (_ : int) -> Ok "fake rpc implementation"))
        in
        let%arr dispatch = dispatch in
        { Spec.dispatch }
      in
      let handle =
        Handle.create ~connectors:(fun _ -> connector) (module Spec) computation
      in
      let break_connection () = is_broken := true in
      let set_implementations l = implementations := make_implementations l in
      activated, handle, break_connection, set_implementations
    ;;

    let%expect_test "client and server pick latest (v2) version" =
      let _activated, handle, _break_connection, _set_implementations =
        setup_test_env
          ~rpcs_on_server:
            [ T { rpc = V1.rpc; latest_result_of_int = Int.to_string }
            ; T { rpc = V2.rpc; latest_result_of_int = Int.to_string }
            ]
          ~rpcs_on_client:[ v2_caller; v1_caller ]
      in
      let%bind.Deferred () = async_do_actions handle [ Query 8 ] in
      Handle.show handle;
      [%expect
        {|
        creating rpc connection
        (rpc ((name foo) (version 2)))
        (Ok 16)
        |}];
      let%bind.Deferred () = async_do_actions handle [ Query 9 ] in
      Handle.show handle;
      [%expect {|
        (rpc ((name foo) (version 2)))
        (Ok 18)
        |}];
      return ()
    ;;

    let%expect_test "client can downgrade" =
      let _activated, handle, _break_connection, _set_implementations =
        setup_test_env
          ~rpcs_on_server:[ T { rpc = V1.rpc; latest_result_of_int = Int.to_string } ]
          ~rpcs_on_client:[ v2_caller; v1_caller ]
      in
      let%bind.Deferred () = async_do_actions handle [ Query 8 ] in
      Handle.show handle;
      [%expect
        {|
        creating rpc connection
        (rpc ((name foo) (version 1)))
        (Ok 16)
        |}];
      let%bind.Deferred () = async_do_actions handle [ Query 9 ] in
      Handle.show handle;
      [%expect {|
        (rpc ((name foo) (version 1)))
        (Ok 18)
        |}];
      return ()
    ;;

    let%expect_test "server can downgrade" =
      let _activated, handle, _break_connection, _set_implementations =
        setup_test_env
          ~rpcs_on_server:
            [ T { rpc = V1.rpc; latest_result_of_int = Int.to_string }
            ; T { rpc = V2.rpc; latest_result_of_int = Int.to_string }
            ]
          ~rpcs_on_client:[ v1_caller ]
      in
      let%bind.Deferred () = async_do_actions handle [ Query 8 ] in
      Handle.show handle;
      [%expect
        {|
        creating rpc connection
        (rpc ((name foo) (version 1)))
        (Ok 16)
        |}];
      let%bind.Deferred () = async_do_actions handle [ Query 9 ] in
      Handle.show handle;
      [%expect {|
        (rpc ((name foo) (version 1)))
        (Ok 18)
        |}];
      return ()
    ;;

    let%expect_test "server rolls back and then forward while client is still active" =
      let server_impls : Erased_implementation.t list =
        [ T { rpc = V1.rpc; latest_result_of_int = Int.to_string }
        ; T { rpc = V1_old.rpc; latest_result_of_int = Fn.id }
        ]
      in
      Deferred.List.iter ~how:`Sequential server_impls ~f:(fun v1_server_impl ->
        let _activated, handle, break_connection, set_implementations =
          setup_test_env
            ~rpcs_on_server:
              [ v1_server_impl; T { rpc = V2.rpc; latest_result_of_int = Int.to_string } ]
            ~rpcs_on_client:[ v2_caller; v1_caller ]
        in
        let%bind.Deferred () = async_do_actions handle [ Query 8 ] in
        Handle.show handle;
        [%expect
          {|
          creating rpc connection
          (rpc ((name foo) (version 2)))
          (Ok 16)
          |}];
        (* simulate the server going down, and then coming back up on a previous
           version that doesn't have the V2 RPC. *)
        break_connection ();
        set_implementations [ v1_server_impl ];
        let%bind () = async_do_actions handle [ Query 7 ] in
        Handle.show handle;
        [%expect
          {|
          creating rpc connection
          (rpc ((name foo) (version 1)))
          (Ok 14)
          |}];
        (* simulate the server going down, and then coming back up on a
           that _does_ have the V2 RPC. *)
        break_connection ();
        set_implementations
          [ T { rpc = V1.rpc; latest_result_of_int = Int.to_string }
          ; T { rpc = V2.rpc; latest_result_of_int = Int.to_string }
          ];
        let%bind () = async_do_actions handle [ Query 6 ] in
        Handle.show handle;
        [%expect
          {|
          creating rpc connection
          (rpc ((name foo) (version 2)))
          (Ok 12)
          |}];
        return ())
    ;;

    let%expect_test "deactivate and reactivate component" =
      let activated, handle, _break_connection, _set_implementations =
        setup_test_env
          ~rpcs_on_server:
            [ T { rpc = V1.rpc; latest_result_of_int = Int.to_string }
            ; T { rpc = V2.rpc; latest_result_of_int = Int.to_string }
            ]
          ~rpcs_on_client:[ v2_caller; v1_caller ]
      in
      let%bind.Deferred () = async_do_actions handle [ Query 8 ] in
      Handle.show handle;
      [%expect
        {|
        creating rpc connection
        (rpc ((name foo) (version 2)))
        (Ok 16)
        |}];
      Bonsai.Var.set activated false;
      Handle.show handle;
      let%bind () = Handle.flush_async_and_bonsai handle in
      [%expect {| |}];
      let%bind () = async_do_actions handle [ Query 7 ] in
      Handle.show handle;
      [%expect {| (Ok "fake rpc implementation") |}];
      Bonsai.Var.set activated true;
      Handle.show handle;
      let%bind () = async_do_actions handle [ Query 42 ] in
      [%expect {|
        (rpc ((name foo) (version 2)))
        (Ok 84)
        |}];
      Deferred.unit
    ;;

    let%expect_test "downgrade rpc when component is deactivated" =
      let activated, handle, break_connection, set_implementations =
        setup_test_env
          ~rpcs_on_server:
            [ T { rpc = V1.rpc; latest_result_of_int = Int.to_string }
            ; T { rpc = V2.rpc; latest_result_of_int = Int.to_string }
            ]
          ~rpcs_on_client:[ v2_caller; v1_caller ]
      in
      let%bind.Deferred () = async_do_actions handle [ Query 8 ] in
      Handle.show handle;
      [%expect
        {|
        creating rpc connection
        (rpc ((name foo) (version 2)))
        (Ok 16)
        |}];
      Bonsai.Var.set activated false;
      let%bind () = async_do_actions handle [ Query 7 ] in
      Handle.show handle;
      [%expect {|
        (rpc ((name foo) (version 2)))
        (Ok 14)
        |}];
      break_connection ();
      set_implementations [ T { rpc = V1.rpc; latest_result_of_int = Int.to_string } ];
      Bonsai.Var.set activated true;
      Handle.show handle;
      let%bind () = async_do_actions handle [ Query 42 ] in
      [%expect
        {|
        creating rpc connection
        (rpc ((name foo) (version 1)))
        (Ok 84)
        |}];
      Deferred.unit
    ;;
  end)
;;

let%test_module "Rvar tests" =
  (module struct
    module Rvar = Rpc_effect.Private.For_tests.Rvar

    let%expect_test _ =
      let i = ref 0 in
      let rec t =
        lazy
          (Rvar.create (fun () ->
             incr i;
             print_s [%message "iteration" (!i : int)];
             if !i < 10 then Rvar.invalidate (Lazy.force t);
             Deferred.Or_error.return !i))
      in
      let%bind () =
        match%map Rvar.contents (Lazy.force t) with
        | Ok x -> print_s [%message "final result" (x : int)]
        | Error e -> print_s [%message (e : Error.t)]
      in
      [%expect
        {|
        (iteration (!i 1))
        (iteration (!i 2))
        (iteration (!i 3))
        (iteration (!i 4))
        (iteration (!i 5))
        (iteration (!i 6))
        (iteration (!i 7))
        (iteration (!i 8))
        (iteration (!i 9))
        (iteration (!i 10))
        ("final result" (x 10))
        |}];
      return ()
    ;;
  end)
;;

let%test_module "Status.state" =
  (module struct
    module Conn = Persistent_connection_kernel.Make (struct
      type t = Rpc.Connection.t

      let close t = Rpc.Connection.close t
      let is_closed t = Rpc.Connection.is_closed t
      let close_finished t = Rpc.Connection.close_finished t
    end)

    module Status_option = struct
      type t = Rpc_effect.Status.t option [@@deriving sexp_of]
    end

    let kill_connection connection =
      let%bind () =
        connection |> Conn.current_connection |> Option.value_exn |> Rpc.Connection.close
      in
      Async_kernel_scheduler.yield_until_no_jobs_remain ()
    ;;

    let next_connection connection =
      let%bind _connection = Conn.connected connection in
      Async_kernel_scheduler.yield_until_no_jobs_remain ()
    ;;

    let make_connection_and_connector () =
      let connection =
        Conn.create
          ~server_name:"test_server"
          ~connect:(fun () -> create_connection [])
          ~address:(module Unit)
          (fun () -> Deferred.Or_error.return ())
      in
      let connector =
        Rpc_effect.Connector.persistent_connection (module Conn) connection
      in
      connection, connector
    ;;

    let%expect_test "basic usage" =
      let connection, connector = make_connection_and_connector () in
      let handle =
        Handle.create
          ~connectors:(fun _ -> connector)
          (Result_spec.sexp (module Rpc_effect.Status))
          (Rpc_effect.Status.state ~where_to_connect:Self)
      in
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ())) |}];
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ("1970-01-01 00:00:00Z"))) |}];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect {| ((state Connected) (connecting_since ())) |}];
      let%bind () = kill_connection connection in
      Handle.show handle;
      [%expect
        {|
        ((state (Disconnected Rpc.Connection.close))
         (connecting_since ("1970-01-01 00:00:00Z")))
        |}];
      let%bind () = next_connection connection in
      Handle.show handle;
      [%expect {| ((state Connected) (connecting_since ())) |}];
      return ()
    ;;

    let%expect_test "connecting-since" =
      let connection, connector = make_connection_and_connector () in
      let handle =
        Handle.create
          ~connectors:(fun _ -> connector)
          (Result_spec.sexp (module Rpc_effect.Status))
          (Rpc_effect.Status.state ~where_to_connect:Self)
      in
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ())) |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ("1970-01-01 00:00:01Z"))) |}];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.show handle;
      [%expect {| ((state Connected) (connecting_since ())) |}];
      let%bind () = kill_connection connection in
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.show handle;
      [%expect
        {|
        ((state (Disconnected Rpc.Connection.close))
         (connecting_since ("1970-01-01 00:00:03Z")))
        |}];
      let%bind () = next_connection connection in
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.show handle;
      [%expect {| ((state Connected) (connecting_since ())) |}];
      return ()
    ;;

    let%expect_test "closing happens when component is inactive" =
      let connection, connector = make_connection_and_connector () in
      let is_active = Bonsai.Var.create true in
      let component =
        let open Bonsai.Let_syntax in
        if%sub Bonsai.Var.value is_active
        then (
          let%sub status = Rpc_effect.Status.state ~where_to_connect:Self in
          Bonsai.pure Option.some status)
        else Bonsai.const None
      in
      let handle =
        Handle.create
          ~connectors:(fun _ -> connector)
          (Result_spec.sexp (module Status_option))
          component
      in
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ()))) |}];
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ("1970-01-01 00:00:00Z")))) |}];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect {| (((state Connected) (connecting_since ()))) |}];
      Bonsai.Var.set is_active false;
      Handle.show handle;
      [%expect {| () |}];
      let%bind () = kill_connection connection in
      Handle.show handle;
      [%expect {| () |}];
      Bonsai.Var.set is_active true;
      Handle.show handle;
      [%expect
        {|
        (((state (Disconnected Rpc.Connection.close))
          (connecting_since ("1970-01-01 00:00:00Z"))))
        |}];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ("1970-01-01 00:00:00Z")))) |}];
      let%bind () = next_connection connection in
      Handle.show handle;
      [%expect {| (((state Connected) (connecting_since ()))) |}];
      return ()
    ;;

    let%expect_test "opening happens when component is inactive" =
      let _connection, connector = make_connection_and_connector () in
      let is_active = Bonsai.Var.create true in
      let component =
        let open Bonsai.Let_syntax in
        if%sub Bonsai.Var.value is_active
        then (
          let%sub status = Rpc_effect.Status.state ~where_to_connect:Self in
          Bonsai.pure Option.some status)
        else Bonsai.const None
      in
      let handle =
        Handle.create
          ~connectors:(fun _ -> connector)
          (Result_spec.sexp (module Status_option))
          component
      in
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ()))) |}];
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ("1970-01-01 00:00:00Z")))) |}];
      Bonsai.Var.set is_active false;
      Handle.show handle;
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect {|
        ()
        ()
        |}];
      Bonsai.Var.set is_active true;
      Handle.show handle;
      [%expect {| (((state Connected) (connecting_since ()))) |}];
      return ()
    ;;

    let%expect_test "failed to connect" =
      let component = Rpc_effect.Status.state ~where_to_connect:Self in
      let handle =
        Handle.create (Result_spec.sexp (module Rpc_effect.Status)) component
      in
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ())) |}];
      Handle.recompute_view_until_stable handle;
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect
        {|
        ((state (Failed_to_connect (Failure "BUG: no bonsai-rpc handler installed")))
         (connecting_since ("1970-01-01 00:00:00Z")))
        |}];
      return ()
    ;;
  end)
;;

let async_show handle =
  Handle.show handle;
  Async_kernel_scheduler.yield_until_no_jobs_remain ()
;;

let async_recompute_view handle =
  Handle.recompute_view handle;
  Async_kernel_scheduler.yield_until_no_jobs_remain ()
;;

let async_show_diff handle =
  Handle.show_diff ~diff_context:0 handle;
  Async_kernel_scheduler.yield_until_no_jobs_remain ()
;;

let%test_module "Polling_state_rpc.poll" =
  (module struct
    let%expect_test "basic usage" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Polling_state_rpc.poll
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          polling_state_rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      (* Initially, there is no response, but initial request got sent. *)
      [%expect
        {|
        ((last_ok_response ()) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        ("For first request" (query 1))
        |}];
      let%bind () = async_show handle in
      (* Because the clock triggers on activate, the next frame both receives the
         first request's response and also sets off the first polling request. *)
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      (* The result stays steady this frame, and no new requests are sent off. *)
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_show handle in
      (* After waiting a second, apparently the clock loop needs another frame
         to realize that its time is up. *)
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      (* But it eventually causes the next polling request to be sent. *)
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query (1))
         (refresh <opaque>))
        ("Computing diff" (from 1) (to_ 2))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      Bonsai.Var.set input_var 2;
      let%bind () = async_show handle in
      (* We also trigger poll requests on query changes. Observe that the
         response includes the query that was used to make the response, which
         in this case is different from the current query. *)
      [%expect
        {|
        ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        ("For first request" (query 2))
        ("Computing diff" (from 2) (to_ 6))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 6))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      Deferred.unit
    ;;

    let%expect_test "scheduling refresh effect" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Polling_state_rpc.poll
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          polling_state_rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let bvar = Async_kernel.Bvar.create () in
      let handle =
        Handle.create
          ~rpc_implementations:
            [ incrementing_polling_state_rpc_implementation ~block_on:bvar () ]
          (module struct
            type t = (int, int) Rpc_effect.Poll_result.t [@@deriving sexp_of]
            type incoming = unit

            let view status = Sexp.to_string ([%sexp_of: t] status)
            let incoming (status : t) () = status.refresh
          end)
          computation
      in
      let%bind () = async_show handle in
      (* On page load; sends rpc request.*)
      [%expect
        {| ((last_ok_response())(last_error())(inflight_query())(refresh <opaque>)) |}];
      Bvar.broadcast bvar ();
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response())(last_error())(inflight_query())(refresh <opaque>))
        +|((last_ok_response())(last_error())(inflight_query(1))(refresh <opaque>))
        ("For first request" (query 1))
        |}];
      let%bind () = async_show_diff handle in
      (* First response is received. *)
      [%expect
        {|
        -|((last_ok_response())(last_error())(inflight_query(1))(refresh <opaque>))
        +|((last_ok_response((1 1)))(last_error())(inflight_query())(refresh <opaque>))
        |}];
      Bvar.broadcast bvar ();
      let%bind () = async_show_diff handle in
      Bvar.broadcast bvar ();
      let%bind () = async_show_diff handle in
      Bvar.broadcast bvar ();
      let%bind () = async_show_diff handle in
      Bvar.broadcast bvar ();
      let%bind () = async_show_diff handle in
      Handle.do_actions handle [ () ];
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response((1 1)))(last_error())(inflight_query())(refresh <opaque>))
        +|((last_ok_response((1 1)))(last_error())(inflight_query(1))(refresh <opaque>))
        ("Computing diff" (from 1) (to_ 2))
        |}];
      Bvar.broadcast bvar ();
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response((1 1)))(last_error())(inflight_query(1))(refresh <opaque>))
        +|((last_ok_response((1 2)))(last_error())(inflight_query())(refresh <opaque>))
        |}];
      let%bind () = async_show_diff handle in
      [%expect {| |}];
      (* Doing two actions in a row does not dispatch RPC twice. *)
      Handle.do_actions handle [ (); () ];
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response((1 2)))(last_error())(inflight_query())(refresh <opaque>))
        +|((last_ok_response((1 2)))(last_error())(inflight_query(1))(refresh <opaque>))
        ("Computing diff" (from 2) (to_ 3))
        |}];
      let%bind () = async_show_diff handle in
      Bvar.broadcast bvar ();
      [%expect
        {|
        -|((last_ok_response((1 2)))(last_error())(inflight_query(1))(refresh <opaque>))
        +|((last_ok_response((1 3)))(last_error())(inflight_query())(refresh <opaque>))
        |}];
      let%bind () = async_show_diff handle in
      [%expect {| |}];
      return ()
    ;;

    let%expect_test "basic usage incrementing query ids" =
      (* Like the basic usage test, but the query changes on each response, to observe
         the behavior of the [inflight_request] field.*)
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Polling_state_rpc.poll
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          polling_state_rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ()) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        ("For first request" (query 1))
        |}];
      Bonsai.Var.set input_var 2;
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        ("For first request" (query 2))
        ("Computing diff" (from 1) (to_ 4))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      Deferred.unit
    ;;

    let every_other_error_polling_state_rpc_implementation () =
      let count = ref 0 in
      let next_response_is_error_ref = ref true in
      let next_result query =
        let next_response_is_error = !next_response_is_error_ref in
        next_response_is_error_ref := not next_response_is_error;
        let result =
          if next_response_is_error
          then raise_s [%message "Error response" (query : int)]
          else query * !count
        in
        return result
      in
      Rpc.Implementation.lift
        ~f:(fun connection -> connection, connection)
        (Polling_state_rpc.implement
           polling_state_rpc
           ~on_client_and_server_out_of_sync:
             (Expect_test_helpers_core.print_s ~hide_positions:true)
           ~for_first_request:(fun _ query -> next_result query)
           (fun _ query ->
             incr count;
             next_result query))
    ;;

    let%expect_test "hit all possible responses from the poller" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Polling_state_rpc.poll
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          ~on_response_received:
            (Value.return (fun query response ->
               Effect.print_s
                 [%message
                   "on_response_received" (query : int) (response : int Or_error.t)]))
          polling_state_rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ every_other_error_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ()) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        (on_response_received (query 1)
         (response
          (Error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn (monitor.ml.Error ("Error response" (query 1)))))))
            (connection_description <created-directly>)
            (rpc_name polling_state_rpc_a) (rpc_version 0)))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ())
         (last_error
          ((1
            ((rpc_error
              (Uncaught_exn
               ((location "server-side rpc computation")
                (exn (monitor.ml.Error ("Error response" (query 1)))))))
             (connection_description <created-directly>)
             (rpc_name polling_state_rpc_a) (rpc_version 0)))))
         (inflight_query ()) (refresh <opaque>))
        |}];
      Bonsai.Var.set input_var 2;
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ())
         (last_error
          ((1
            ((rpc_error
              (Uncaught_exn
               ((location "server-side rpc computation")
                (exn (monitor.ml.Error ("Error response" (query 1)))))))
             (connection_description <created-directly>)
             (rpc_name polling_state_rpc_a) (rpc_version 0)))))
         (inflight_query ()) (refresh <opaque>))
        (on_response_received (query 2) (response (Ok 0)))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 0))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      Bonsai.Var.set input_var 3;
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 0))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        (on_response_received (query 3)
         (response
          (Error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn (monitor.ml.Error ("Error response" (query 3)))))))
            (connection_description <created-directly>)
            (rpc_name polling_state_rpc_a) (rpc_version 0)))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 0)))
         (last_error
          ((3
            ((rpc_error
              (Uncaught_exn
               ((location "server-side rpc computation")
                (exn (monitor.ml.Error ("Error response" (query 3)))))))
             (connection_description <created-directly>)
             (rpc_name polling_state_rpc_a) (rpc_version 0)))))
         (inflight_query ()) (refresh <opaque>))
        |}];
      Deferred.unit
    ;;

    let%expect_test "multiple pollers, clear on deactivate (on by default)" =
      let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 10, () ]) in
      let map = Bonsai.Var.value map_var in
      let computation =
        Bonsai.assoc
          (module Int)
          map
          ~f:(fun key _data ->
            Rpc_effect.Polling_state_rpc.poll
              ~sexp_of_query:[%sexp_of: Int.t]
              ~sexp_of_response:[%sexp_of: Int.t]
              ~equal_query:[%equal: Int.t]
              ~equal_response:[%equal: Int.t]
              polling_state_rpc
              ~where_to_connect:Self
              ~every:(Time_ns.Span.of_sec 1.0)
              key)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t Int.Map.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        ("For first request" (query 2))
        ("For first request" (query 1))
        ("For first request" (query 10))
        |}];
      let%bind () = async_show handle in
      (* NOTE: The order of the response is [2 -> 1 -> 10] hence the response of [1] and
         [2] are the same because  [2 * 1] = [1 * 2].*)
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 30))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 30))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 10);
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.set map ~key:10 ~data:());
      let%bind () = async_show handle in
      (* since we clear the map entry when it gets de-activated, it does not
         remember its last response, and thus must poll for it again. *)
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        ("For first request" (query 10))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 40))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 40))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Deferred.unit
    ;;

    let%expect_test "multiple pollers, don't clear on deactivate" =
      let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 10, () ]) in
      let map = Bonsai.Var.value map_var in
      let computation =
        Bonsai.assoc
          (module Int)
          map
          ~f:(fun key _data ->
            Rpc_effect.Polling_state_rpc.poll
              ~sexp_of_query:[%sexp_of: Int.t]
              ~sexp_of_response:[%sexp_of: Int.t]
              ~equal_query:[%equal: Int.t]
              ~equal_response:[%equal: Int.t]
              polling_state_rpc
              ~clear_when_deactivated:false
              ~where_to_connect:Self
              ~every:(Time_ns.Span.of_sec 1.0)
              key)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t Int.Map.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        ("For first request" (query 2))
        ("For first request" (query 1))
        ("For first request" (query 10))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 30))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 30))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 10);
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.set map ~key:10 ~data:());
      let%bind () = async_show handle in
      (* since we do not clear the map entry when it gets de-activated, it does
         remember its last response, and thus does not need to poll for it again. *)
      [%expect
        {|
        ((1
          ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 2))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 30))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        ("For first request" (query 10))
        |}];
      Deferred.unit
    ;;
  end)
;;

let%test_module "Rpc.poll" =
  (module struct
    let rpc =
      Rpc.Rpc.create
        ~name:"rpc"
        ~version:0
        ~bin_query:bin_int
        ~bin_response:bin_int
        ~include_in_error_count:Only_on_exn
    ;;

    let incrementing_rpc_implementation ?block_on () =
      let count = ref 0 in
      Rpc.Rpc.implement rpc (fun _ query ->
        incr count;
        let%bind () =
          match block_on with
          | Some bvar -> Bvar.wait bvar
          | None -> return ()
        in
        return (query * !count))
    ;;

    let%expect_test "basic usage" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Rpc.poll
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ()) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ()) (last_error ()) (inflight_query (1))
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query (1))
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      Bonsai.Var.set input_var 2;
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 2))) (last_error ()) (inflight_query (2))
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 6))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      Deferred.unit
    ;;

    let%expect_test "scheduling refresh effect" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Rpc.poll
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let bvar = Async_kernel.Bvar.create () in
      let broadcast () =
        Bvar.broadcast bvar ();
        Async_kernel_scheduler.yield_until_no_jobs_remain ()
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_rpc_implementation ~block_on:bvar () ]
          (module struct
            type t = (int, int) Rpc_effect.Poll_result.t [@@deriving sexp_of]
            type incoming = unit

            let view status = Sexp.to_string ([%sexp_of: t] status)
            let incoming (status : t) () = status.refresh
          end)
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {| ((last_ok_response())(last_error())(inflight_query())(refresh <opaque>)) |}];
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response())(last_error())(inflight_query())(refresh <opaque>))
        +|((last_ok_response())(last_error())(inflight_query(1))(refresh <opaque>))
        |}];
      let%bind () = broadcast () in
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response())(last_error())(inflight_query(1))(refresh <opaque>))
        +|((last_ok_response((1 1)))(last_error())(inflight_query())(refresh <opaque>))
        |}];
      let%bind () = broadcast () in
      let%bind () = async_show_diff handle in
      let%bind () = broadcast () in
      let%bind () = async_show_diff handle in
      Handle.do_actions handle [ () ];
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response((1 1)))(last_error())(inflight_query())(refresh <opaque>))
        +|((last_ok_response((1 1)))(last_error())(inflight_query(1))(refresh <opaque>))
        |}];
      let%bind () = broadcast () in
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response((1 1)))(last_error())(inflight_query(1))(refresh <opaque>))
        +|((last_ok_response((1 2)))(last_error())(inflight_query())(refresh <opaque>))
        |}];
      (* Doing two actions causes them to be dispatched in sequence, rather
         than twice in a row. *)
      Handle.do_actions handle [ (); (); () ];
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response((1 2)))(last_error())(inflight_query())(refresh <opaque>))
        +|((last_ok_response((1 2)))(last_error())(inflight_query(1))(refresh <opaque>))
        |}];
      let%bind () = broadcast () in
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response((1 2)))(last_error())(inflight_query(1))(refresh <opaque>))
        +|((last_ok_response((1 3)))(last_error())(inflight_query(1))(refresh <opaque>))
        |}];
      let%bind () = broadcast () in
      let%bind () = async_show_diff handle in
      [%expect
        {|
        -|((last_ok_response((1 3)))(last_error())(inflight_query(1))(refresh <opaque>))
        +|((last_ok_response((1 4)))(last_error())(inflight_query())(refresh <opaque>))
        |}];
      return ()
    ;;

    let every_other_error_rpc_implementation () =
      let count = ref 0 in
      let next_response_is_error_ref = ref true in
      let next_result query =
        let next_response_is_error = !next_response_is_error_ref in
        next_response_is_error_ref := not next_response_is_error;
        let result =
          if next_response_is_error
          then raise_s [%message "Error response" (query : int)]
          else query * !count
        in
        return result
      in
      Rpc.Rpc.implement rpc (fun _ query ->
        incr count;
        next_result query)
    ;;

    let%expect_test "hit all possible responses from the poller" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Rpc.poll
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          ~on_response_received:
            (Value.return (fun query response ->
               Effect.print_s
                 [%message
                   "on_response_received" (query : int) (response : int Or_error.t)]))
          rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ every_other_error_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ()) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ()) (last_error ()) (inflight_query (1))
         (refresh <opaque>))
        (on_response_received (query 1)
         (response
          (Error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn (monitor.ml.Error ("Error response" (query 1)))))))
            (connection_description <created-directly>) (rpc_name rpc)
            (rpc_version 0)))))
        |}];
      Bonsai.Var.set input_var 2;
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ())
         (last_error
          ((1
            ((rpc_error
              (Uncaught_exn
               ((location "server-side rpc computation")
                (exn (monitor.ml.Error ("Error response" (query 1)))))))
             (connection_description <created-directly>) (rpc_name rpc)
             (rpc_version 0)))))
         (inflight_query ()) (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ())
         (last_error
          ((1
            ((rpc_error
              (Uncaught_exn
               ((location "server-side rpc computation")
                (exn (monitor.ml.Error ("Error response" (query 1)))))))
             (connection_description <created-directly>) (rpc_name rpc)
             (rpc_version 0)))))
         (inflight_query (2)) (refresh <opaque>))
        (on_response_received (query 2) (response (Ok 4)))
        |}];
      Bonsai.Var.set input_var 3;
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
         (refresh <opaque>))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 4))) (last_error ()) (inflight_query (3))
         (refresh <opaque>))
        (on_response_received (query 3)
         (response
          (Error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn (monitor.ml.Error ("Error response" (query 3)))))))
            (connection_description <created-directly>) (rpc_name rpc)
            (rpc_version 0)))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((2 4)))
         (last_error
          ((3
            ((rpc_error
              (Uncaught_exn
               ((location "server-side rpc computation")
                (exn (monitor.ml.Error ("Error response" (query 3)))))))
             (connection_description <created-directly>) (rpc_name rpc)
             (rpc_version 0)))))
         (inflight_query ()) (refresh <opaque>))
        |}];
      Deferred.unit
    ;;

    let%expect_test "multiple pollers, clear on deactivate (on by default)" =
      let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 10, () ]) in
      let map = Bonsai.Var.value map_var in
      let computation =
        Bonsai.assoc
          (module Int)
          map
          ~f:(fun key _data ->
            Rpc_effect.Rpc.poll
              ~sexp_of_query:[%sexp_of: Int.t]
              ~sexp_of_response:[%sexp_of: Int.t]
              ~equal_query:[%equal: Int.t]
              ~equal_response:[%equal: Int.t]
              rpc
              ~where_to_connect:Self
              ~every:(Time_ns.Span.of_sec 1.0)
              key)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t Int.Map.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ()) (last_error ()) (inflight_query (1))
           (refresh <opaque>)))
         (2
          ((last_ok_response ()) (last_error ()) (inflight_query (2))
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query (10))
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 30))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 10);
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.set map ~key:10 ~data:());
      let%bind () = async_show handle in
      (* since we clear the map entry when it gets de-activated, it does not
         remember its last response, and thus must poll for it again. *)
      [%expect
        {|
        ((1
          ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query (10))
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 40))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Deferred.unit
    ;;

    let%expect_test "multiple pollers, don't clear on deactivate" =
      let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 10, () ]) in
      let map = Bonsai.Var.value map_var in
      let computation =
        Bonsai.assoc
          (module Int)
          map
          ~f:(fun key _data ->
            Rpc_effect.Rpc.poll
              ~sexp_of_query:[%sexp_of: Int.t]
              ~sexp_of_response:[%sexp_of: Int.t]
              ~equal_query:[%equal: Int.t]
              ~equal_response:[%equal: Int.t]
              rpc
              ~clear_when_deactivated:false
              ~where_to_connect:Self
              ~every:(Time_ns.Span.of_sec 1.0)
              key)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Poll_result.t Int.Map.t [@@deriving sexp_of]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ()) (last_error ()) (inflight_query (1))
           (refresh <opaque>)))
         (2
          ((last_ok_response ()) (last_error ()) (inflight_query (2))
           (refresh <opaque>)))
         (10
          ((last_ok_response ()) (last_error ()) (inflight_query (10))
           (refresh <opaque>))))
        |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 30))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 10);
      let%bind () = async_show handle in
      [%expect
        {|
        ((1
          ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.set map ~key:10 ~data:());
      let%bind () = async_show handle in
      (* since we do not clear the map entry when it gets de-activated, it does
         remember its last response, and thus does not need to poll for it again. *)
      [%expect
        {|
        ((1
          ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (2
          ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())
           (refresh <opaque>)))
         (10
          ((last_ok_response ((10 30))) (last_error ()) (inflight_query ())
           (refresh <opaque>))))
        |}];
      Deferred.unit
    ;;
  end)
;;

let%test_module "Rpc.poll_until_ok" =
  (module struct
    let rpc =
      Rpc.Rpc.create
        ~name:"rpc"
        ~version:0
        ~bin_query:bin_int
        ~bin_response:bin_int
        ~include_in_error_count:Only_on_exn
    ;;

    let returns_ok_after ~iterations =
      let count = ref 0 in
      Rpc.Rpc.implement rpc (fun _ query ->
        print_endline "received rpc!";
        if !count < iterations
        then (
          incr count;
          failwith "too early!");
        incr count;
        return (query * !count))
    ;;

    module Result_spec = struct
      type t = (int, int) Rpc_effect.Poll_result.t
      type incoming = Refresh

      let view
        { Rpc_effect.Poll_result.last_ok_response
        ; last_error
        ; inflight_query
        ; refresh = _
        }
        =
        Sexp.to_string_hum
          [%message
            (last_ok_response : (int * int) option)
              (last_error : (int * Error.t) option)
              (inflight_query : int option)]
      ;;

      let incoming
        { Rpc_effect.Poll_result.last_ok_response = _
        ; last_error = _
        ; inflight_query = _
        ; refresh
        }
        Refresh
        =
        refresh
      ;;
    end

    let%expect_test "Stops polling after first response" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Rpc.poll_until_ok
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          rpc
          ~where_to_connect:Self
          ~retry_interval:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ returns_ok_after ~iterations:0 ]
          (module Result_spec)
          computation
      in
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ()) (last_error ()) (inflight_query ())) |}];
      let%bind () = async_recompute_view handle in
      [%expect {| received rpc! |}];
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())) |}];
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())) |}];
      (* Despite clock advancing, an rpc is not sent. *)
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())) |}];
      (* Even after stopping, if the query changes, the rpc is sent again. *)
      Bonsai.Var.set input_var 2;
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())) |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ((1 1))) (last_error ()) (inflight_query (2)))
        received rpc!
        |}];
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ((2 4))) (last_error ()) (inflight_query ())) |}];
      Deferred.unit
    ;;

    let%expect_test "If responses are an error, it continues polling until there are no \
                     errors and stops polling after first ok resonse."
      =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Rpc.poll_until_ok
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          rpc
          ~where_to_connect:Self
          ~retry_interval:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ returns_ok_after ~iterations:2 ]
          (module Result_spec)
          computation
      in
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ()) (last_error ()) (inflight_query ())) |}];
      let%bind () = async_recompute_view handle in
      [%expect {| received rpc! |}];
      let%bind () = async_show handle in
      (* First error. *)
      [%expect
        {|
        ((last_ok_response ())
         (last_error
          ((1
            ((rpc_error
              (Uncaught_exn
               ((location "server-side rpc computation")
                (exn (monitor.ml.Error (Failure "too early!"))))))
             (connection_description <created-directly>) (rpc_name rpc)
             (rpc_version 0)))))
         (inflight_query ()))
        |}];
      (* Advancing clock to send another rpc.*)
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_recompute_view handle in
      (* Retried rpc sent.*)
      let%bind () = async_recompute_view handle in
      [%expect {| received rpc! |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((last_ok_response ())
         (last_error
          ((1
            ((rpc_error
              (Uncaught_exn
               ((location "server-side rpc computation")
                (exn (monitor.ml.Error (Failure "too early!"))))))
             (connection_description <created-directly>) (rpc_name rpc)
             (rpc_version 0)))))
         (inflight_query ()))
        |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_recompute_view handle in
      (* Retried rpc sent.*)
      let%bind () = async_recompute_view handle in
      [%expect {| received rpc! |}];
      (* Third rpc returns ok. *)
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ((1 3))) (last_error ()) (inflight_query ())) |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      (* No more rpc's are sent. *)
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      [%expect {| |}];
      Deferred.unit
    ;;

    let%expect_test "Even after stopping, if the refresh effect is scheduled, the rpc is \
                     sent again"
      =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Rpc.poll_until_ok
          ~sexp_of_query:[%sexp_of: Int.t]
          ~sexp_of_response:[%sexp_of: Int.t]
          ~equal_query:[%equal: Int.t]
          ~equal_response:[%equal: Int.t]
          rpc
          ~where_to_connect:Self
          ~retry_interval:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ returns_ok_after ~iterations:0 ]
          (module Result_spec)
          computation
      in
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ()) (last_error ()) (inflight_query ())) |}];
      let%bind () = async_recompute_view handle in
      [%expect {| received rpc! |}];
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ((1 1))) (last_error ()) (inflight_query ())) |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      [%expect {| |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      [%expect {| |}];
      (* Rpc is sent when refresh is scheduled *)
      Handle.do_actions handle [ Refresh ];
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      [%expect {| received rpc! |}];
      let%bind () = async_show handle in
      [%expect {| ((last_ok_response ((1 2))) (last_error ()) (inflight_query ())) |}];
      (* Rpc is not resent afterwards when refresh is scheduled *)
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      let%bind () = async_recompute_view handle in
      [%expect {| |}];
      Deferred.unit
    ;;
  end)
;;

let%test_module "multi-poller" =
  (module struct
    open Bonsai.Let_syntax

    let dummy_poller input =
      let%sub () =
        Bonsai.Edge.lifecycle
          ~on_activate:
            (let%map input = input in
             Effect.print_s [%sexp "start", (input : int)])
          ~on_deactivate:
            (let%map input = input in
             Effect.print_s [%sexp "stop", (input : int)])
          ()
      in
      let%arr input = input in
      { Rpc_effect.Poll_result.last_ok_response = Some (input, "hello")
      ; last_error = None
      ; inflight_query = None
      ; refresh = Effect.Ignore
      }
    ;;

    let%expect_test "single multi-poller" =
      let component =
        let%sub poller =
          Bonsai_web.Rpc_effect.Shared_poller.custom_create (module Int) ~f:dummy_poller
        in
        let%sub lookup =
          Bonsai_web.Rpc_effect.Shared_poller.lookup
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            poller
            (Value.return 5)
        in
        let%arr lookup = lookup in
        [%message "" ~_:(lookup.last_ok_response : (int * string) option)]
      in
      let handle =
        Bonsai_test.Handle.create (Bonsai_test.Result_spec.sexp (module Sexp)) component
      in
      let open Deferred.Let_syntax in
      Handle.show handle;
      [%expect {| () |}];
      Handle.show handle;
      [%expect {|
        ((5 hello))
        (start 5)
        |}];
      Handle.show handle;
      [%expect {| ((5 hello)) |}];
      return ()
    ;;

    let%expect_test "two multi-pollers looking at the same key" =
      let component =
        let%sub poller =
          Bonsai_web.Rpc_effect.Shared_poller.custom_create (module Int) ~f:dummy_poller
        in
        let%sub a =
          Bonsai_web.Rpc_effect.Shared_poller.lookup
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            poller
            (Value.return 5)
        in
        let%sub b =
          Bonsai_web.Rpc_effect.Shared_poller.lookup
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            poller
            (Value.return 5)
        in
        let%arr a = a
        and b = b in
        [%message
          ""
            ~a:(a.last_ok_response : (int * string) option)
            ~b:(b.last_ok_response : (int * string) option)]
      in
      let handle =
        Bonsai_test.Handle.create (Bonsai_test.Result_spec.sexp (module Sexp)) component
      in
      let open Deferred.Let_syntax in
      Handle.show handle;
      [%expect {| ((a ()) (b ())) |}];
      Handle.show handle;
      [%expect {|
        ((a ((5 hello))) (b ((5 hello))))
        (start 5)
        |}];
      Handle.show handle;
      [%expect {| ((a ((5 hello))) (b ((5 hello)))) |}];
      return ()
    ;;

    let%expect_test "two multi-pollers looking at the different keys" =
      let component =
        let%sub poller =
          Bonsai_web.Rpc_effect.Shared_poller.custom_create (module Int) ~f:dummy_poller
        in
        let%sub a =
          Bonsai_web.Rpc_effect.Shared_poller.lookup
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            poller
            (Value.return 5)
        in
        let%sub b =
          Bonsai_web.Rpc_effect.Shared_poller.lookup
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            poller
            (Value.return 10)
        in
        let%arr a = a
        and b = b in
        [%message
          ""
            ~a:(a.last_ok_response : (int * string) option)
            ~b:(b.last_ok_response : (int * string) option)]
      in
      let handle =
        Bonsai_test.Handle.create (Bonsai_test.Result_spec.sexp (module Sexp)) component
      in
      let open Deferred.Let_syntax in
      Handle.show handle;
      [%expect {| ((a ()) (b ())) |}];
      Handle.show handle;
      [%expect
        {|
        ((a ((5 hello))) (b ((10 hello))))
        (start 5)
        (start 10)
        |}];
      Handle.show handle;
      [%expect {| ((a ((5 hello))) (b ((10 hello)))) |}];
      return ()
    ;;

    let%expect_test "one multi-pollers looking a key and then it quits" =
      let bool_var = Bonsai.Var.create true in
      let component =
        let%sub poller =
          Bonsai_web.Rpc_effect.Shared_poller.custom_create (module Int) ~f:dummy_poller
        in
        let%sub lookup =
          if%sub Bonsai.Var.value bool_var
          then
            Bonsai_web.Rpc_effect.Shared_poller.lookup
              ~sexp_of_model:[%sexp_of: Int.t]
              ~equal:[%equal: Int.t]
              poller
              (Value.return 5)
          else
            Bonsai.const
              { Rpc_effect.Poll_result.last_ok_response = Some (5, "INACTIVE")
              ; last_error = None
              ; inflight_query = None
              ; refresh = Effect.Ignore
              }
        in
        let%arr lookup = lookup in
        [%message "" ~_:(lookup.last_ok_response : (int * string) option)]
      in
      let handle =
        Bonsai_test.Handle.create (Bonsai_test.Result_spec.sexp (module Sexp)) component
      in
      let open Deferred.Let_syntax in
      Handle.show handle;
      [%expect {| () |}];
      Handle.show handle;
      [%expect {|
        ((5 hello))
        (start 5)
        |}];
      Handle.show handle;
      [%expect {| ((5 hello)) |}];
      Bonsai.Var.set bool_var false;
      Handle.show handle;
      [%expect {| ((5 INACTIVE)) |}];
      Handle.show handle;
      [%expect {|
        ((5 INACTIVE))
        (stop 5)
        |}];
      return ()
    ;;

    let%expect_test "two multi-pollers looking at the same key then one of them quits" =
      let bool_var = Bonsai.Var.create true in
      let component =
        let%sub poller =
          Bonsai_web.Rpc_effect.Shared_poller.custom_create (module Int) ~f:dummy_poller
        in
        let%sub a =
          Bonsai_web.Rpc_effect.Shared_poller.lookup
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            poller
            (Value.return 5)
        in
        let%sub b =
          if%sub Bonsai.Var.value bool_var
          then
            Bonsai_web.Rpc_effect.Shared_poller.lookup
              ~sexp_of_model:[%sexp_of: Int.t]
              ~equal:[%equal: Int.t]
              poller
              (Value.return 5)
          else
            Bonsai.const
              { Rpc_effect.Poll_result.last_ok_response = Some (5, "INACTIVE")
              ; last_error = None
              ; inflight_query = None
              ; refresh = Effect.Ignore
              }
        in
        let%arr a = a
        and b = b in
        [%message
          ""
            ~a:(a.last_ok_response : (int * string) option)
            ~b:(b.last_ok_response : (int * string) option)]
      in
      let handle =
        Bonsai_test.Handle.create (Bonsai_test.Result_spec.sexp (module Sexp)) component
      in
      let open Deferred.Let_syntax in
      Handle.show handle;
      [%expect {| ((a ()) (b ())) |}];
      Handle.show handle;
      [%expect {|
        ((a ((5 hello))) (b ((5 hello))))
        (start 5)
        |}];
      Handle.show handle;
      [%expect {| ((a ((5 hello))) (b ((5 hello)))) |}];
      Bonsai.Var.set bool_var false;
      Handle.show handle;
      [%expect {| ((a ((5 hello))) (b ((5 INACTIVE)))) |}];
      return ()
    ;;

    let%expect_test "two multi-pollers looking at different keys then one of them quits" =
      let bool_var = Bonsai.Var.create true in
      let component =
        let%sub poller =
          Bonsai_web.Rpc_effect.Shared_poller.custom_create (module Int) ~f:dummy_poller
        in
        let%sub a =
          Bonsai_web.Rpc_effect.Shared_poller.lookup
            ~sexp_of_model:[%sexp_of: Int.t]
            ~equal:[%equal: Int.t]
            poller
            (Value.return 5)
        in
        let%sub b =
          if%sub Bonsai.Var.value bool_var
          then
            Bonsai_web.Rpc_effect.Shared_poller.lookup
              ~sexp_of_model:[%sexp_of: Int.t]
              ~equal:[%equal: Int.t]
              poller
              (Value.return 10)
          else
            Bonsai.const
              { Rpc_effect.Poll_result.last_ok_response = Some (10, "INACTIVE")
              ; last_error = None
              ; inflight_query = None
              ; refresh = Effect.Ignore
              }
        in
        let%arr a = a
        and b = b in
        [%message
          ""
            ~a:(a.last_ok_response : (int * string) option)
            ~b:(b.last_ok_response : (int * string) option)]
      in
      let handle =
        Bonsai_test.Handle.create (Bonsai_test.Result_spec.sexp (module Sexp)) component
      in
      let open Deferred.Let_syntax in
      Handle.show handle;
      [%expect {| ((a ()) (b ())) |}];
      Handle.show handle;
      [%expect
        {|
        ((a ((5 hello))) (b ((10 hello))))
        (start 5)
        (start 10)
        |}];
      Handle.show handle;
      [%expect {| ((a ((5 hello))) (b ((10 hello)))) |}];
      Bonsai.Var.set bool_var false;
      Handle.show handle;
      [%expect {| ((a ((5 hello))) (b ((10 INACTIVE)))) |}];
      Handle.show handle;
      [%expect
        {|
        ((a ((5 hello))) (b ((10 INACTIVE))))
        (stop 10)
        |}];
      return ()
    ;;
  end)
;;

let%expect_test "There should be 0 nodes being observed. (This test should ideally be at \
                 the end of the file.)"
  =
  (* This test is a test against a regression test where many incremental nodes were still
     observed across test runs. This test tests that the [Expect_test_config] in
     [Async_js_test] works. *)
  let number_of_observed_nodes = Incremental.State.num_active_observers Ui_incr.State.t in
  print_s [%message (number_of_observed_nodes : int)];
  [%expect {| (number_of_observed_nodes 0) |}];
  return ()
;;

let%expect_test "Check that no introspection recording occurs unless started..." =
  let module Intro = Rpc_effect.For_introspection.For_testing in
  let introspection_enabled = Intro.get_introspection_supported () in
  let is_recording = Intro.get_is_recording () in
  print_s [%message (introspection_enabled : bool) (is_recording : bool)];
  (* This is OK. Introspection being enabled only means that the feature/ability
     to start recording is available. (e.g. if the devtool panel is opened in wikipedia the
     abscence of the variable is used to show a nicer error message.) *)
  [%expect {| ((introspection_enabled true) (is_recording false)) |}];
  let popped_events = Intro.pop_events () |> Js_of_ocaml.Js.to_string in
  print_endline popped_events;
  [%expect {| () |}];
  return ()
;;
