open! Core
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
let uppercase_e : string -> string Effect.t = Bonsai_web.Effect.of_deferred_fun uppercase

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
     let on_submit (contents : string) : unit Effect.t =
       let%bind.Effect s = uppercase_e contents in
       set_result (Filled s)
     in
     let form_view =
       textbox
       |> Forms.label "text to capitalize"
       |> Forms.view_as_vdom ~on_submit:(Forms.Submit.create ~f:on_submit ())
     in
     Vdom.Node.div
       ~attr:(Vdom.Attr.style (Css_gen.display `Inline_grid))
       [ form_view; Vdom.Node.text (Request_state.to_string result_state) ])
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
     let on_submit contents =
       let%bind.Effect () = set_result Pending in
       let%bind.Effect s = uppercase_e contents in
       set_result (Filled s)
     in
     let form_view =
       textbox
       |> Forms.label "text to capitalize"
       |> Forms.view_as_vdom ~on_submit:(Forms.Submit.create ~f:on_submit ())
     in
     Vdom.Node.div
       ~attr:(Vdom.Attr.style (Css_gen.display `Inline_grid))
       [ form_view; Vdom.Node.text (Request_state.to_string result_state) ])
;;

(* $MDX part-end *)
let () = Util.run uppercase_rpc_sender_bind ~id:"uppercase_rpc_sender_bind"
