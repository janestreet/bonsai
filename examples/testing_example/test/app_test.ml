open! Core
open! Bonsai_web_test
open! Bonsai_web

let hello_world = Bonsai_testing_example_lib.hello_world
let hello_user = Bonsai_testing_example_lib.hello_user
let hello_textbox = Bonsai_testing_example_lib.hello_textbox

(* [HELLO_WORLD_TEST BEGIN] *)
module Handle = Bonsai_web_test.Handle
module Result_spec = Bonsai_web_test.Result_spec

let%expect_test "it shows hello world" =
  let handle = Handle.create (Result_spec.vdom Fn.id) hello_world in
  Handle.show handle;
  [%expect {| <span> hello world </span> |}]
;;

(* [HELLO_WORLD_TEST END] *)

(* [HELLO_USER_TEST BEGIN] *)
let%expect_test "shows hello to a user" =
  let user_var = Bonsai.Var.create "Bob" in
  let user = Bonsai.Var.value user_var in
  let handle = Handle.create (Result_spec.vdom Fn.id) (hello_user user) in
  Handle.show handle;
  [%expect {| <span> hello Bob </span> |}];
  Bonsai.Var.set user_var "Alice";
  Handle.show handle;
  [%expect {| <span> hello Alice </span> |}]
;;

(* [HELLO_USER_TEST END] *)

(* [HELLO_USER_DIFF_TEST BEGIN] *)
let%expect_test "shows hello to a user" =
  let user_var = Bonsai.Var.create "Bob" in
  let user = Bonsai.Var.value user_var in
  let handle = Handle.create (Result_spec.vdom Fn.id) (hello_user user) in
  Handle.show handle;
  [%expect {| <span> hello Bob </span> |}];
  Bonsai.Var.set user_var "Alice";
  Handle.show_diff handle;
  [%expect {|
    -|<span> hello Bob </span>
    +|<span> hello Alice </span> |}]
;;

(* [HELLO_USER_DIFF_TEST END] *)

(* [HELLO_TEXTBOX_DIFF_TEST BEGIN] *)
let%expect_test "shows hello to a specified user" =
  let handle = Handle.create (Result_spec.vdom Fn.id) hello_textbox in
  Handle.show handle;
  [%expect
    {|
    <div>
      <input oninput> </input>
      <span> hello  </span>
    </div> |}];
  Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Bob";
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input oninput> </input>
    -|  <span> hello  </span>
    +|  <span> hello Bob </span>
      </div> |}];
  Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Alice";
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input oninput> </input>
    -|  <span> hello Bob </span>
    +|  <span> hello Alice </span>
      </div> |}]
;;

(* [HELLO_TEXTBOX_DIFF_TEST END] *)

(* [STATE_TEST BEGIN] *)
module State_view_spec = struct
  type t = string * (string -> unit Vdom.Effect.t)
  type incoming = string

  let view (view, _) = view
  let incoming (_, incoming) = incoming
end

let%expect_test "test Bonsai.state" =
  let component : (string * (string -> unit Vdom.Effect.t)) Computation.t =
    Bonsai.state [%here] (module String) ~default_model:"hello"
  in
  let handle = Handle.create (module State_view_spec) component in
  Handle.show handle;
  [%expect {| hello |}];
  Handle.do_actions handle [ "world" ];
  Handle.show handle;
  [%expect {| world |}]
;;

(* [STATE_TEST END] *)
