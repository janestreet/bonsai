open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax

(* $MDX part-begin=int_view *)
let int_view (a : int Bonsai.t) : Vdom.Node.t Bonsai.t =
  let%arr a = a in
  Vdom.Node.div [ Vdom.Node.text (Int.to_string a) ]
;;

(* $MDX part-end *)

let () = Util.run_vdom_val (int_view (Bonsai.return 5)) ~id:"int_view"

(* $MDX part-begin=sum_and_display *)
let sum_and_display (a : int Bonsai.t) (b : int Bonsai.t) : Vdom.Node.t Bonsai.t =
  let%arr a = a
  and b = b in
  Vdom.Node.textf "%d + %d = %d" a b (a + b)
;;

(* $MDX part-end *)

let () =
  Util.run_vdom_val
    (sum_and_display (Bonsai.return 5) (Bonsai.return 8))
    ~id:"sum_and_display"
;;
