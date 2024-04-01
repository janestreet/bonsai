open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

(* $MDX part-begin=hello-world *)
let hello_world = Vdom.Node.span [ Vdom.Node.text "hello world" ]

(* $MDX part-end *)

(* $MDX part-begin=hello-user *)
let hello_user (name : string Bonsai.t) : Vdom.Node.t Bonsai.t =
  let%arr name = name in
  Vdom.Node.span [ Vdom.Node.textf "hello %s" name ]
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-text-box *)
let hello_textbox graph : Vdom.Node.t Bonsai.t =
  let state, set = Bonsai.state "" graph in
  let%arr message = hello_user state
  and set = set in
  Vdom.Node.div
    [ Vdom.Node.input ~attrs:[ Vdom.Attr.on_input (fun _ text -> set text) ] (); message ]
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-world-test *)
module Handle = Bonsai_web_test.Handle
module Result_spec = Bonsai_web_test.Result_spec

let%expect_test "it shows hello world" =
  let handle = Handle.create (Result_spec.vdom Fn.id) (fun _ -> return hello_world) in
  Handle.show handle;
  [%expect {| <span> hello world </span> |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-user-test *)
let%expect_test "shows hello to a user" =
  let user_var = Bonsai.Expert.Var.create "Bob" in
  let user = Bonsai.Expert.Var.value user_var in
  let handle = Handle.create (Result_spec.vdom Fn.id) (fun _ -> hello_user user) in
  Handle.show handle;
  [%expect {| <span> hello Bob </span> |}];
  Bonsai.Expert.Var.set user_var "Alice";
  Handle.show handle;
  [%expect {| <span> hello Alice </span> |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-user-diff-test *)
let%expect_test "shows hello to a user" =
  let user_var = Bonsai.Expert.Var.create "Bob" in
  let user = Bonsai.Expert.Var.value user_var in
  let handle = Handle.create (Result_spec.vdom Fn.id) (fun _ -> hello_user user) in
  Handle.show handle;
  [%expect {| <span> hello Bob </span> |}];
  Bonsai.Expert.Var.set user_var "Alice";
  Handle.show_diff handle;
  [%expect {|
    -|<span> hello Bob </span>
    +|<span> hello Alice </span>
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-text-box-diff-test *)
let%expect_test "shows hello to a specified user" =
  let handle = Handle.create (Result_spec.vdom Fn.id) hello_textbox in
  Handle.show handle;
  [%expect
    {|
    <div>
      <input oninput> </input>
      <span> hello  </span>
    </div>
    |}];
  Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Bob";
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input oninput> </input>
    -|  <span> hello  </span>
    +|  <span> hello Bob </span>
      </div>
    |}];
  Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Alice";
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input oninput> </input>
    -|  <span> hello Bob </span>
    +|  <span> hello Alice </span>
      </div>
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=state-test *)
module State_view_spec = struct
  type t = string * (string -> unit Effect.t)
  type incoming = string

  let view : t -> string = fun (view, _) -> view
  let incoming : t -> incoming -> unit Effect.t = fun (_, incoming) -> incoming
end

let%expect_test "test Bonsai.state" =
  let state_single_bonsai graph : (string * (string -> unit Vdom.Effect.t)) Bonsai.t =
    let state, inject = Bonsai.state "hello" graph in
    Bonsai.both state inject
  in
  let handle = Handle.create (module State_view_spec) state_single_bonsai in
  Handle.show handle;
  [%expect {| hello |}];
  Handle.do_actions handle [ "world" ];
  Handle.show handle;
  [%expect {| world |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=test-clock *)
let%expect_test "test clock" =
  let handle = Handle.create (Result_spec.vdom Fn.id) Time_examples.current_time in
  Handle.show handle;
  [%expect {| 1970-01-01 00:00:00.000000000Z |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 2.0);
  Handle.show handle;
  [%expect {| 1970-01-01 00:00:02.000000000Z |}]
;;

(* $MDX part-end *)
