open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

(* $MDX part-begin=counter_var *)
let counter_every_second : int Value.t =
  let counter_var : int Bonsai.Var.t = Bonsai.Var.create (-1) in
  every (Time_ns.Span.of_sec 1.0) (fun () ->
    Bonsai.Var.update counter_var ~f:(fun i -> i + 1));
  Bonsai.Var.value counter_var
;;

(* $MDX part-end *)

let counter_every_other_second =
  let counter_var : int Bonsai.Var.t = Bonsai.Var.create (-1) in
  every (Time_ns.Span.of_sec 2.0) (fun () ->
    Bonsai.Var.update counter_var ~f:(fun i -> i + 1));
  Bonsai.Var.value counter_var
;;

(* $MDX part-begin=counter *)

let view_for_counter (counter : int Value.t) : Vdom.Node.t Value.t =
  let%map counter = counter in
  Vdom.Node.textf "counter: %d" counter
;;

(* $MDX part-end *)

let () = Util.run (Bonsai.read (view_for_counter counter_every_second)) ~id:"counter"

(* $MDX part-begin=counter_both *)
let view_for_counter_both (a : int Value.t) (b : int Value.t) =
  let%map a = a
  and b = b in
  Vdom.Node.textf "a: %d, b: %d" a b
;;

(* $MDX part-end *)

let () =
  Util.run
    (Bonsai.read (view_for_counter_both counter_every_second counter_every_other_second))
    ~id:"counter_both"
;;

(* $MDX part-begin=split *)

let split (both : (int * string) Value.t) =
  let%sub a, b = Bonsai.read both in
  let (_ : int Value.t) = a in
  let (_ : string Value.t) = b in
  (* $MDX part-end *)
  assert false
;;

let _ = split

(* $MDX part-begin=bad_split *)

let bad_split (both : (int * string) Value.t) =
  let a = Value.map both ~f:fst in
  let b = Value.map both ~f:snd in
  let (_ : int Value.t) = a in
  let (_ : string Value.t) = b in
  (* $MDX part-end *)
  assert false
;;

let _ = bad_split
