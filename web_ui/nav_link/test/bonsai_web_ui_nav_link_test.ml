open! Core
open! Async_kernel
open Bonsai_web
open Bonsai_web_test
open Async_js_test

let create_handle node = Handle.create (Result_spec.vdom Fn.id) (Bonsai.const node)
let make_with_children = Bonsai_web_ui_nav_link.make' ~page_to_string:Fn.id
let make = Bonsai_web_ui_nav_link.make ~page_to_string:Fn.id

let%expect_test "make_with_children includes children in VDOM" =
  let handle =
    create_handle
      (make_with_children
         ~set_url:(fun _ -> Effect.Ignore)
         "/foo"
         [ Vdom.Node.span [ Vdom.Node.text "go to foo" ] ])
  in
  Handle.show handle;
  [%expect {|
    <a href="/foo" onclick>
      <span> go to foo </span>
    </a> |}];
  return ()
;;

let%expect_test "make includes text in VDOM" =
  let handle =
    create_handle (make ~set_url:(fun _ -> Effect.Ignore) "/foo" "go to foo")
  in
  Handle.show handle;
  [%expect {|
    <a href="/foo" onclick> go to foo </a> |}];
  return ()
;;

(* We've already tested the difference between [make_children] and [make], so we only test
   [make] below. *)

let%expect_test "custom attr" =
  let handle =
    create_handle
      (make
         ~attrs:[ Vdom.Attr.class_ "my-link" ]
         ~set_url:(fun _ -> Effect.Ignore)
         "/foo"
         "go to foo")
  in
  Handle.show handle;
  [%expect {|
    <a href="/foo" class="my-link" onclick> go to foo </a> |}];
  return ()
;;

let click_nav_link ?shift_key_down ?alt_key_down ?ctrl_key_down () =
  let handle =
    create_handle
      (make
         ~set_url:
           (Effect.of_sync_fun (fun url ->
              print_s [%message "set_url called with" (url : string)]))
         "/foo"
         "foobar")
  in
  Handle.click_on
    ?shift_key_down
    ?alt_key_down
    ?ctrl_key_down
    handle
    ~get_vdom:Fn.id
    ~selector:"a"
;;

let%expect_test "set_url triggered if clicking with no modifier" =
  click_nav_link ();
  [%expect {| ("set_url called with" (url /foo)) |}];
  return ()
;;

let%expect_test "set_url not triggered if shift-clicking" =
  click_nav_link ~shift_key_down:true ();
  [%expect {| |}];
  return ()
;;

let%expect_test "set_url not triggered if alt-clicking" =
  click_nav_link ~alt_key_down:true ();
  [%expect {| |}];
  return ()
;;

let%expect_test "set_url not triggered if ctrl-clicking" =
  click_nav_link ~ctrl_key_down:true ();
  [%expect {| |}];
  return ()
;;
