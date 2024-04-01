open! Core
open! Async_kernel
open! Bonsai_web.Cont
open! Bonsai_web_test
open Bonsai.Let_syntax
open Async_rpc_kernel

(* $MDX part-begin=protocol-1 *)
let double_rpc =
  Rpc.Rpc.create
    ~name:"double"
    ~version:0
    ~bin_query:[%bin_type_class: int]
    ~bin_response:[%bin_type_class: int]
    ~include_in_error_count:Only_on_exn
;;

(* $MDX part-end *)

(* $MDX part-begin=protocol-2 *)
module Current_time = struct
  include String
  include Legacy_diffable.Atomic.Make (String)
end

let current_time_rpc =
  Polling_state_rpc.create
    ~name:"current_time"
    ~version:0
    ~query_equal:[%equal: string]
    ~bin_query:[%bin_type_class: string]
    (module Current_time)
;;

(* $MDX part-end *)

(* $MDX part-begin=implementation-1 *)
let double_implementation =
  Rpc.Rpc.implement' double_rpc (fun _connection_state query -> Int.max 1 (query * 2))
;;

(* $MDX part-end *)

(* $MDX part-begin=implementation-2 *)
let current_time_implementation =
  Polling_state_rpc.implement
    ~on_client_and_server_out_of_sync:print_s
    current_time_rpc
    (fun _connection_state zone ->
    Deferred.return
      (Time_ns.to_string_trimmed ~zone:(Timezone.of_string zone) (Time_ns.now ())))
  |> Rpc.Implementation.lift ~f:(fun connection_state ->
       connection_state, connection_state)
;;

(* $MDX part-end *)

type Rpc_effect.Where_to_connect.Custom.t += Connection

(* Below is some sleight-of-hand. We want the readers of the guide to think that
   we are using [Self], but we don't *actually* want to do that, since it would
   require having a server to connect to. Thus, we shadow the text we want to
   display with the value we want use. *)

(* $MDX part-begin=where_to_connect *)
let where_to_connect : Rpc_effect.Where_to_connect.t = Self
(* $MDX part-end *)

let () = ignore (where_to_connect : Rpc_effect.Where_to_connect.t)
let where_to_connect : Rpc_effect.Where_to_connect.t = Custom Connection

let connector =
  Rpc_effect.Connector.for_test
    (Rpc.Implementations.create_exn
       ~implementations:[ double_implementation; current_time_implementation ]
       ~on_unknown_rpc:`Raise)
    ~connection_state:Fn.id
;;

let custom_connector = function
  | Connection -> connector
  | _ -> Rpc_effect.Connector.test_fallback
;;

(* $MDX part-begin=client-1 *)
let double_number_app graph =
  let dispatch_double_rpc =
    Rpc_effect.Rpc.dispatcher double_rpc ~where_to_connect graph
  in
  let number, set_number = Bonsai.state 1 graph in
  let%arr dispatch_double_rpc = dispatch_double_rpc
  and number = number
  and set_number = set_number in
  Vdom.Node.div
    [ Vdom.Node.div [ Vdom.Node.text [%string "The number is: %{number#Int}"] ]
    ; Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.on_click (fun _ ->
              match%bind.Effect dispatch_double_rpc number with
              | Ok doubled_number -> set_number doubled_number
              | Error error -> Effect.of_sync_fun eprint_s [%sexp (error : Error.t)])
          ]
        [ Vdom.Node.text "Double the number" ]
    ]
;;

(* $MDX part-end *)

module Css = [%css stylesheet {|
.error_text {
  color: red;
}
|}]

let zone_form graph =
  let module Form = Bonsai_web_ui_form.With_automatic_view in
  let form =
    Form.Elements.Textbox.string
      ~placeholder:(Bonsai.return "timezone")
      ~allow_updates_when_focused:`Always
      ()
      graph
  in
  let form = Form.Dynamic.with_default (Bonsai.return "America/New_York") form graph in
  let value =
    let%arr form = form in
    Form.value_or_default ~default:"America/New_York" form
  in
  let view =
    let%arr form = form in
    Form.view_as_vdom form
  in
  value, view
;;

(* $MDX part-begin=client-2 *)
let current_time_app graph =
  let zone, zone_view = zone_form graph in
  let poll =
    Rpc_effect.Polling_state_rpc.poll
      current_time_rpc
      ~equal_query:[%equal: string]
      ~equal_response:[%equal: Current_time.t]
      ~where_to_connect
      ~every:(Time_ns.Span.of_sec 0.1)
      zone
      graph
  in
  let%arr { last_ok_response; last_error; inflight_query = _; refresh = _ } = poll
  and zone_view = zone_view in
  let text =
    match last_ok_response with
    | Some (zone, current_time) ->
      [%string "The current time in the zone '%{zone}' is %{current_time}"]
    | None -> "Loading..."
  in
  let error_view =
    match last_error with
    | Some (zone, error) ->
      Vdom.Node.div
        ~attrs:[ Css.error_text ]
        [ Vdom.Node.text [%string "Got error when requesting time in zone '%{zone}'"]
        ; Vdom.Node.pre [ Vdom.Node.text (Error.to_string_hum error) ]
        ]
    | None -> Vdom.Node.none
  in
  Vdom.Node.div [ zone_view; Vdom.Node.div [ Vdom.Node.text text ]; error_view ]
;;

(* $MDX part-end *)
