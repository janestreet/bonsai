open! Core
open! Async_kernel
open! Bonsai_web
open Async_js
open Bonsai_chat_common
module Rpc_connection = Persistent_connection.Rpc

module Room_state = struct
  type t =
    { messages : Message.t list
    ; current_room : Room.t option
    }
  [@@deriving fields]
end

let process_message_stream ~conn ~room_state_var =
  let%bind conn = Rpc_connection.connected conn in
  let%bind pipe, _ = Rpc.Pipe_rpc.dispatch_exn Protocol.Message_stream.t conn () in
  Pipe.iter_without_pushback pipe ~f:(fun message ->
    Bonsai.Var.update room_state_var ~f:(function
      | { Room_state.messages; current_room }
        when [%equal: Room.t option] current_room (Some message.room) ->
        { current_room; messages = List.append messages [ message ] }
      | other -> other))
;;

let obfuscate_message message =
  String.hash message
  |> Int.to_string
  |> Js_of_ocaml.Js.string
  |> (fun x -> Js_of_ocaml.Dom_html.window##btoa x)
  |> Js_of_ocaml.Js.to_string
  |> String.lowercase
  |> String.filter ~f:Char.is_alpha
;;

let change_room ~conn ~room_state_var =
  let on_room_switch room =
    let%bind conn = Rpc_connection.connected conn in
    let%map messages = Rpc.Rpc.dispatch_exn Protocol.Messages_request.t conn room in
    Bonsai.Var.set room_state_var { Room_state.messages; current_room = Some room }
  in
  let dispatch = Effect.of_deferred_fun on_room_switch in
  fun room -> dispatch room
;;

let run () =
  let conn =
    Rpc_connection.create
      ~server_name:"ws-server"
      ~connect:(fun () -> Rpc.Connection.client ())
      ~address:(module Unit)
      Deferred.Or_error.return
  in
  let room_state_var =
    Bonsai.Var.create { Room_state.messages = []; current_room = None }
  in
  let change_room = change_room ~conn ~room_state_var in
  let component =
    let open Bonsai.Let_syntax in
    Bonsai_examples_rpc_chat_client.App.component
      ~current_room:(Bonsai.Var.value room_state_var >>| Room_state.current_room)
      ~messages:(Bonsai.Var.value room_state_var >>| Room_state.messages)
      ~change_room
      ~obfuscate_message
  in
  let () = Bonsai_web.Start.start component in
  don't_wait_for (process_message_stream ~conn ~room_state_var);
  return ()
;;

let () = don't_wait_for (run ())
