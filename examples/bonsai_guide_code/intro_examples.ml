open! Bonsai_web.Cont

let name_undefined = "Some User"
let new_emails_undefined = 412
let on_click_undefined = Effect.alert "Not implemented"

(* $MDX part-begin=message_vdom *)
open! Core
open Virtual_dom

let message_vdom ~name ~new_emails =
  Vdom.Node.div
    ~attrs:[ [%css {|font-size: 16px;|}] ]
    [ Vdom.Node.textf "hello %s! you have %d new emails" name new_emails ]
;;

(* $MDX part-end *)
let () =
  Util.run_vdom
    (message_vdom ~name:name_undefined ~new_emails:new_emails_undefined)
    ~id:"message_vdom"
;;

(* $MDX part-begin=read_email_button *)
let read_email_button ~on_click =
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> on_click) ]
    [ Vdom.Node.text "Read an email!" ]
;;

(* $MDX part-end *)
let () =
  Util.run_vdom (read_email_button ~on_click:on_click_undefined) ~id:"read_email_button"
;;

(* $MDX part-begin=emails_bonsai *)
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let emails_bonsai ~name ~new_emails ~read_email_effect =
  let message =
    let%arr name = name
    and new_emails = new_emails in
    message_vdom ~name ~new_emails
  in
  let%arr message = message
  and read_email_effect = read_email_effect in
  Vdom.Node.div [ message; read_email_button ~on_click:read_email_effect ]
;;

(* $MDX part-end *)
let () =
  Util.run
    (fun _ ->
      emails_bonsai
        ~name:(return name_undefined)
        ~new_emails:(return new_emails_undefined)
        ~read_email_effect:(return on_click_undefined))
    ~id:"emails_bonsai"
;;

(* $MDX part-begin=emails_stateful *)
let emails_stateful ~name graph =
  let default_count = 999 in
  let (count : int Bonsai.t), (set_count : (int -> unit Effect.t) Bonsai.t) =
    Bonsai.state default_count graph
  in
  let read_email_effect =
    let%arr count = count
    and set_count = set_count in
    set_count (count - 1)
  in
  emails_bonsai ~name ~new_emails:count ~read_email_effect
;;

(* $MDX part-end *)
let () = Util.run (emails_stateful ~name:(return name_undefined)) ~id:"emails_stateful"

(* $MDX part-begin=app *)
let app graph = emails_stateful ~name:(Bonsai.return "User") graph
(* $MDX part-end *)

let () = Util.run app ~id:"app"
