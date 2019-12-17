open! Core_kernel
open! Async_kernel
open Bonsai_web
open Async_js_jane
open Bonsai_chat_common
module Outgoing_command = Outgoing
module Incoming_command = Nothing

let refresh_rooms ~conn ~app_handle =
  let%map rooms = Rpc.Rpc.dispatch_exn Protocol.List_rooms.t conn () in
  Start.Handle.update_input app_handle ~f:(fun input -> { input with App.Input.rooms })
;;

let process_message_stream ~conn ~app_handle =
  let%bind pipe, _ = Rpc.Pipe_rpc.dispatch_exn Protocol.Message_stream.t conn () in
  let input = Start.Handle.input app_handle in
  Pipe.iter pipe ~f:(fun message ->
    if Option.exists input.App.Input.current_room ~f:(Room.equal message.room)
    then
      Start.Handle.set_input
        app_handle
        { input with messages = List.append input.messages [ message ] };
    Deferred.unit)
;;

let send_message ~conn ~room ~contents =
  let message = { Message.author = ""; contents; room } in
  Rpc.Rpc.dispatch_exn Protocol.Send_message.t conn message >>| Or_error.ok_exn
;;

let switch_room ~conn ~app_handle ~room =
  let%map messages = Rpc.Rpc.dispatch_exn Protocol.Messages_request.t conn room in
  Start.Handle.update_input app_handle ~f:(fun input ->
    { input with App.Input.messages; current_room = Some room })
;;

let handle_outgoing_bonsai_messages ~conn ~app_handle =
  let eval_event = function
    | Outgoing_command.Refresh_rooms -> refresh_rooms ~conn ~app_handle
    | Outgoing_command.Send_message contents ->
      (match (Start.Handle.input app_handle).current_room with
       | None -> Deferred.unit
       | Some room -> send_message ~conn ~room ~contents)
    | Outgoing_command.Switch_room room -> switch_room ~conn ~app_handle ~room
  in
  app_handle |> Start.Handle.outgoing |> Pipe.iter ~f:eval_event
;;

let run () =
  let app_handle =
    Start.start
      ~initial_input:App.Input.default
      ~initial_model:Compose.Model.default
      ~bind_to_element_with_id:"app"
      App.component
  in
  let check_hg_version_upon_connection =
    Rpc.Check_hg_version_upon_connection.Yes { on_version_mismatch = Alert }
  in
  let%map conn = Rpc.Connection.client_exn () ~check_hg_version_upon_connection in
  List.iter
    [ refresh_rooms; handle_outgoing_bonsai_messages; process_message_stream ]
    ~f:(fun f -> don't_wait_for (f ~conn ~app_handle))
;;

let () = don't_wait_for (run ())
