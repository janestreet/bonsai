open! Core_kernel
open! Async_kernel
open Bonsai_web
open Async_js
open Bonsai_chat_common
open Composition_infix

let run_refresh_rooms ~conn ~app_handle =
  let%map rooms = Rpc.Rpc.dispatch_exn Protocol.List_rooms.t conn () in
  Start.Handle.update_input app_handle ~f:(fun input -> { input with App.Input.rooms })
;;

let refresh_rooms ~conn ~app_handle =
  let dispatch =
    (fun () -> run_refresh_rooms ~conn ~app_handle) |> Effect.of_deferred_fun |> unstage
  in
  dispatch ()
;;

let process_message_stream ~conn ~app_handle =
  let%bind pipe, _ = Rpc.Pipe_rpc.dispatch_exn Protocol.Message_stream.t conn () in
  Pipe.iter pipe ~f:(fun message ->
    Start.Handle.update_input app_handle ~f:(fun input ->
      if [%equal: Room.t option] input.App.Input.current_room (Some message.room)
      then { input with messages = List.append input.messages [ message ] }
      else input);
    Deferred.unit)
;;

let send_message ~conn =
  let obfuscate message =
    String.hash message
    |> Int.to_string
    |> Js_of_ocaml.Js.string
    |> (fun x -> Js_of_ocaml.Dom_html.window##btoa x)
    |> Js_of_ocaml.Js.to_string
    |> String.lowercase
    |> String.filter ~f:Char.is_alpha
  in
  let dispatch =
    Rpc.Rpc.dispatch_exn Protocol.Send_message.t conn
    |> Effect.of_deferred_fun
    |> unstage
    >> Effect.handle_error ~f:(Fn.const Vdom.Event.Ignore)
  in
  fun ~room ~contents ->
    let contents = obfuscate contents in
    dispatch { Message.room; contents; author = "" }
;;

let switch_room ~conn ~app_handle =
  let on_room_switch room =
    let%map messages = Rpc.Rpc.dispatch_exn Protocol.Messages_request.t conn room in
    Start.Handle.update_input app_handle ~f:(fun input ->
      { input with App.Input.messages; current_room = Some room })
  in
  let dispatch = on_room_switch |> Effect.of_deferred_fun |> unstage in
  fun room -> dispatch room
;;

let run () =
  let app_handle =
    Start.start_standalone
      ~initial_input:App.Input.default
      ~bind_to_element_with_id:"app"
      App.component
  in
  let%bind conn = Rpc.Connection.client_exn () in
  don't_wait_for @@ run_refresh_rooms ~conn ~app_handle;
  don't_wait_for @@ process_message_stream ~conn ~app_handle;
  Start.Handle.update_input app_handle ~f:(fun input ->
    { input with
      switch_room = switch_room ~conn ~app_handle
    ; send_message = send_message ~conn
    ; refresh_rooms = refresh_rooms ~conn ~app_handle
    });
  Deferred.unit
;;

let () = don't_wait_for (run ())
