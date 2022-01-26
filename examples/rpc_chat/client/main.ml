open! Core
open! Async_kernel
open! Bonsai_web
open Async_js
open Bonsai_chat_common

let run_refresh_rooms ~conn ~rooms_list_var =
  let%map rooms = Rpc.Rpc.dispatch_exn Protocol.List_rooms.t conn () in
  Bonsai.Var.set rooms_list_var rooms
;;

let refresh_rooms ~conn ~rooms_list_var =
  let dispatch =
    (fun () -> run_refresh_rooms ~conn ~rooms_list_var) |> Effect.of_deferred_fun
  in
  dispatch ()
;;

module Room_state = struct
  type t =
    { messages : Message.t list
    ; current_room : Room.t option
    }
  [@@deriving fields]
end

let process_message_stream ~conn ~room_state_var =
  let%bind pipe, _ = Rpc.Pipe_rpc.dispatch_exn Protocol.Message_stream.t conn () in
  Pipe.iter_without_pushback pipe ~f:(fun message ->
    Bonsai.Var.update room_state_var ~f:(function
      | { Room_state.messages; current_room }
        when [%equal: Room.t option] current_room (Some message.room) ->
        { current_room; messages = List.append messages [ message ] }
      | other -> other))
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
  let dispatch message =
    match%map Rpc.Rpc.dispatch_exn Protocol.Send_message.t conn message with
    | Ok () -> ()
    | Error _ -> ()
  in
  let dispatch = Effect.of_deferred_fun dispatch in
  fun ~room ~contents ->
    let contents = obfuscate contents in
    dispatch { Message.room; contents; author = "" }
;;

let change_room ~conn ~room_state_var =
  let on_room_switch room =
    let%map messages = Rpc.Rpc.dispatch_exn Protocol.Messages_request.t conn room in
    Bonsai.Var.set room_state_var { Room_state.messages; current_room = Some room }
  in
  let dispatch = Effect.of_deferred_fun on_room_switch in
  fun room -> dispatch room
;;

let run () =
  Async_js.init ();
  let%bind conn = Rpc.Connection.client_exn () in
  let rooms_list_var = Bonsai.Var.create [] in
  let room_state_var =
    Bonsai.Var.create { Room_state.messages = []; current_room = None }
  in
  let change_room = change_room ~conn ~room_state_var in
  let refresh_rooms = refresh_rooms ~conn ~rooms_list_var in
  let send_message = send_message ~conn in
  let component =
    let open Bonsai.Let_syntax in
    App.component
      ~room_list:(Bonsai.Var.value rooms_list_var)
      ~current_room:(Bonsai.Var.value room_state_var >>| Room_state.current_room)
      ~messages:(Bonsai.Var.value room_state_var >>| Room_state.messages)
      ~refresh_rooms
      ~change_room
      ~send_message
  in
  let (_ : _ Start.Handle.t) =
    Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
  in
  don't_wait_for (run_refresh_rooms ~conn ~rooms_list_var);
  don't_wait_for (process_message_stream ~conn ~room_state_var);
  return ()
;;

let () = don't_wait_for (run ())
