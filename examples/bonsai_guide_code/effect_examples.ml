open! Core_kernel
open! Async_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
module Forms = Bonsai_web_ui_form

let uppercase s =
  let open Async_kernel in
  let%map () = after (Time_ns.Span.of_sec 0.5) in
  String.uppercase s
;;

(* $MDX part-begin=uppercase_e *)
let uppercase_e : string -> string Effect.t =
  unstage (Bonsai_web.Effect.of_deferred_fun uppercase)
;;

(* $MDX part-end *)

(* $MDX part-begin=uppercase_rpc_sender *)

module Request_state = struct
  type t =
    | Empty
    | Pending
    | Filled of string
  [@@deriving sexp, equal]

  let to_string = function
    | Empty -> "<no request sent>"
    | Pending -> "pending..."
    | Filled s -> s
  ;;
end

let uppercase_rpc_sender =
  let%sub textbox = Forms.Elements.Textbox.string [%here] in
  let%sub result_state =
    Bonsai.state [%here] (module Request_state) ~default_model:Empty
  in
  return
    (let%map textbox = textbox
     and result_state, set_result = result_state in
     let textbox = textbox |> Forms.label "text to capitalize" in
     let textbox_contents = Forms.value_or_default textbox ~default:"" in
     let result = Request_state.to_string result_state in
     let on_click : Vdom.Event.t =
       let uppercased : string Effect.t = uppercase_e textbox_contents in
       Bonsai.Effect.inject uppercased ~on_response:(fun s -> set_result (Filled s))
     in
     let send_rpc_button =
       Vdom.Node.button
         [ Vdom.Attr.on_click (fun _ -> on_click) ]
         [ Vdom.Node.text "request uppercase" ]
     in
     Vdom.Node.div
       [ Vdom.Attr.style (Css_gen.display `Inline_grid) ]
       [ Forms.view_as_vdom textbox; Vdom.Node.text result; send_rpc_button ])
;;

(* $MDX part-end *)

let () = Util.run uppercase_rpc_sender ~id:"uppercase_rpc_sender"

(* $MDX part-begin=uppercase_rpc_sender_bind *)
let uppercase_rpc_sender_bind =
  let%sub textbox = Forms.Elements.Textbox.string [%here] in
  let%sub result_state =
    Bonsai.state [%here] (module Request_state) ~default_model:Empty
  in
  return
    (let%map textbox = textbox
     and result_state, set_result = result_state in
     let textbox = textbox |> Forms.label "text to capitalize" in
     let textbox_contents = Forms.value_or_default textbox ~default:"" in
     let result = Request_state.to_string result_state in
     let on_click : Vdom.Event.t =
       let open Bonsai.Effect.Let_syntax in
       Bonsai.Effect.inject_ignoring_response
         (let%bind () = set_result Pending |> Bonsai.Effect.of_event in
          let%bind s = uppercase_e textbox_contents in
          set_result (Filled s) |> Bonsai.Effect.of_event)
     in
     let send_rpc_button =
       Vdom.Node.button
         [ Vdom.Attr.on_click (fun _ -> on_click) ]
         [ Vdom.Node.text "request uppercase" ]
     in
     Vdom.Node.div
       [ Vdom.Attr.style (Css_gen.display `Inline_grid) ]
       [ Forms.view_as_vdom textbox; Vdom.Node.text result; send_rpc_button ])
;;

(* $MDX part-end *)
let () = Util.run uppercase_rpc_sender_bind ~id:"uppercase_rpc_sender_bind"
