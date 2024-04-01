open! Core
open! Bonsai_web_test
open! Bonsai_web.Cont

let hello_world = Bonsai_testing_example_lib.hello_world
let hello_user = Bonsai_testing_example_lib.hello_user
let hello_textbox = Bonsai_testing_example_lib.hello_textbox

(* $MDX part-begin=hello-world-test *)
module Handle = Bonsai_web_test.Handle
module Result_spec = Bonsai_web_test.Result_spec

let%expect_test "it shows hello world" =
  let handle = Handle.create (Result_spec.vdom Fn.id) hello_world in
  Handle.show handle;
  [%expect {| <span> hello world </span> |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-user-test *)
let%expect_test "shows hello to a user" =
  let user_var = Bonsai.Expert.Var.create "Bob" in
  let user = Bonsai.Expert.Var.value user_var in
  let handle = Handle.create (Result_spec.vdom Fn.id) (hello_user user) in
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
  let handle = Handle.create (Result_spec.vdom Fn.id) (hello_user user) in
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
  type t = string * (string -> unit Vdom.Effect.t)
  type incoming = string

  let view (view, _) = view
  let incoming (_, incoming) = incoming
end

let%expect_test "test Bonsai.state" =
  let component : Bonsai.graph -> (string * (string -> unit Vdom.Effect.t)) Bonsai.t =
    fun graph ->
    Tuple2.uncurry Bonsai.both
    @@ Bonsai.state
         "hello"
         ~sexp_of_model:[%sexp_of: String.t]
         ~equal:[%equal: String.t]
         graph
  in
  let handle = Handle.create (module State_view_spec) component in
  Handle.show handle;
  [%expect {| hello |}];
  Handle.do_actions handle [ "world" ];
  Handle.show handle;
  [%expect {| world |}]
;;

(* $MDX part-end *)
