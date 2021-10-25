open! Core
open Bonsai_web
open Bonsai_web_test

let component =
  Bonsai_web_ui_not_connected_warning_box.(
    component ~create_message:message_for_async_durable)
;;

let%expect_test _ =
  let is_connected = Bonsai.Var.create true in
  let handle =
    Handle.create
      (Result_spec.vdom ~filter_printed_attributes:(const false) Fn.id)
      (component (Bonsai.Var.value is_connected))
  in
  Handle.show handle;
  [%expect {| <div> </div> |}];
  Bonsai.Var.set is_connected false;
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> Warning! </div>
      <div>
        You've been disconnected from the server for 0ns. There is no need to refresh the page, since the web client will reconnect automatically when the server becomes available again.
      </div>
    </div> |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 4.5);
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> Warning! </div>
      <div>
        You've been disconnected from the server for 5s. There is no need to refresh the page, since the web client will reconnect automatically when the server becomes available again.
      </div>
    </div> |}];
  Bonsai.Var.set is_connected true;
  Handle.show handle;
  [%expect {| <div> </div> |}];
  Bonsai.Var.set is_connected false;
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> Warning! </div>
      <div>
        You've been disconnected from the server for 0ns. There is no need to refresh the page, since the web client will reconnect automatically when the server becomes available again.
      </div>
    </div> |}]
;;
