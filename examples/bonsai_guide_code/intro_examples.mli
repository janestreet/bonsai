open! Core
open! Bonsai_web.Cont

(* $MDX part-begin=message_vdom *)
val message_vdom : name:string -> new_emails:int -> Vdom.Node.t
(* $MDX part-end *)

(* $MDX part-begin=read_email_button *)
val read_email_button : on_click:unit Effect.t -> Vdom.Node.t
(* $MDX part-end *)

(* $MDX part-begin=emails_bonsai *)
val emails_bonsai
  :  name:string Bonsai.t
  -> new_emails:int Bonsai.t
  -> read_email_effect:unit Effect.t Bonsai.t
  -> Vdom.Node.t Bonsai.t
(* $MDX part-end *)

(* $MDX part-begin=emails_stateful *)
val emails_stateful : name:string Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t
(* $MDX part-end *)

(* $MDX part-begin=app *)
val app : Bonsai.graph -> Vdom.Node.t Bonsai.t
(* $MDX part-end *)
