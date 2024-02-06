open! Core
open! Bonsai_web
open! Async_kernel
open! Async_rpc_kernel
open Bonsai.Let_syntax

(* This example is more of a stress-test than as demo. It starts both a client
   and a server in the same browser page. If you click the "Add" button a ton
   of times, it will attempt to trigger a memory leak on the server. The memory
   leak gets triggered if [Rpc_effect.Polling_state_rpc.poll] is not
   implemented to clear its model and also call [forget_on_server]; since it
   does those two things, the leak shouldn't get triggered. *)

type Rpc_effect.Where_to_connect.Custom.t += Connection

module T = struct
  type t = { data : int Int.Map.t [@diff.map] } [@@deriving sexp, diff, bin_io, equal]
end

let rpc =
  Polling_state_rpc.create
    ~name:"the_rpc"
    ~version:0
    ~query_equal:[%equal: int]
    ~bin_query:[%bin_type_class: int]
    (module Diffable_polling_state_rpc_response.Polling_state_rpc_response.Make (T))
;;

let component =
  let%sub (_, items), inject =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: int * unit Int.Map.t]
      ~equal:[%equal: int * unit Int.Map.t]
      ~sexp_of_action:[%sexp_of: [ `Add | `Remove of int ]]
      ~default_model:(0, Int.Map.empty)
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) (last_index, map) action ->
      match action with
      | `Add ->
        let map =
          if Map.length map > 100 then Map.remove map (fst (Map.min_elt_exn map)) else map
        in
        last_index + 1, Map.set map ~key:last_index ~data:()
      | `Remove i -> last_index, Map.remove map i)
  in
  let%sub items =
    Bonsai.assoc
      (module Int)
      items
      ~f:(fun key _data ->
        let%sub response =
          Rpc_effect.Polling_state_rpc.poll
            ~sexp_of_query:[%sexp_of: Int.t]
            ~sexp_of_response:[%sexp_of: T.t]
            ~equal_query:[%equal: Int.t]
            ~equal_response:[%equal: T.t]
            rpc
            ~where_to_connect:(Custom Connection)
            ~every:(Time_ns.Span.of_sec 1.0)
            key
        in
        let%arr key = key
        and inject = inject
        and response = response in
        Vdom.Node.div
          [ Vdom.Node.button
              ~attrs:[ Vdom.Attr.on_click (fun _ -> inject (`Remove key)) ]
              [ Vdom.Node.text "Remove" ]
          ; Vdom.Node.div
              [ Vdom.Node.sexp_for_debugging
                  [%sexp (response : (int, T.t) Rpc_effect.Poll_result.t)]
              ]
          ])
  in
  let%arr items = items
  and inject = inject in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Add) ]
        [ Vdom.Node.text "Append Item" ]
    ; Vdom.Node.div (Map.data items)
    ]
;;

let implementation =
  let t = ref { T.data = Int.Map.empty } in
  let implementation _ query =
    t
      := { T.data =
             Map.update !t.data (query % 100) ~f:(function
               | Some count -> count + 1
               | None -> 0)
         };
    Deferred.return { T.data = Map.map !t.data ~f:Fn.id }
  in
  Polling_state_rpc.implement
    rpc
    ~on_client_and_server_out_of_sync:
      (Expect_test_helpers_core.print_s ~hide_positions:true)
    ~for_first_request:implementation
    (fun _ query -> implementation () query)
;;

let run () =
  let connector =
    Rpc_effect.Connector.for_test
      (Rpc.Implementations.create_exn
         ~implementations:[ implementation ]
         ~on_unknown_rpc:`Raise)
      ~connection_state:(fun conn -> (), conn)
  in
  let () =
    Bonsai_web.Start.start
      ~custom_connector:(function
        | Connection -> connector
        | _ -> Rpc_effect.Connector.test_fallback)
      component
  in
  Deferred.never ()
;;

let () = don't_wait_for (run ())
