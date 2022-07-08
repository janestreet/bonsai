open! Core
open! Bonsai_web_test
module Notifications = Bonsai_web_ui_notifications

module Notification_spec = struct
  type t = Notifications.t

  type incoming =
    | Add_notification of
        { level : [ `Error of Error.t | `Success ]
        ; text : string
        }

  let view t =
    let module V = (val Result_spec.vdom Fn.id) in
    Notifications.to_vdom t |> V.view
  ;;

  let incoming t incoming =
    match incoming with
    | Add_notification { level; text } ->
      (match level with
       | `Error error -> Notifications.add_error t ~error ~text
       | `Success -> Notifications.add_success t ~text)
  ;;
end

let%expect_test "render some notifications and test that they close as expected" =
  let open! Bonsai.Let_syntax in
  let handle =
    Handle.create
      (module Notification_spec)
      (Notifications.create
         ~dismiss_notifications_after:
           (Bonsai.Value.return (Time_ns.Span.create ~sec:15 ()))
         ~dismiss_errors_automatically:(Bonsai.Value.return false)
         ())
  in
  Handle.show handle;
  (* Empty notification container as nothing has raised yet. *)
  [%expect {| <div class="notification_container_hash_replaced_in_test"> </div> |}];
  Handle.do_actions
    handle
    [ Notification_spec.Add_notification { level = `Success; text = "An action occured!" }
    ];
  Handle.show_diff handle;
  (* Single notification that has opened. *)
  [%expect
    {|
    -|<div class="notification_container_hash_replaced_in_test"> </div>
    +|<div class="notification_container_hash_replaced_in_test">
    +|  <div @key=0 data-notification-id="0" class="notification_hash_replaced_in_test" onclick>
    +|    <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    +|         style={
    +|           animation-name: fadeOut;
    +|           animation-duration: 15.00s;
    +|           animation-timing-function: ease-in;
    +|         }> An action occured! </div>
    +|  </div>
    +|</div> |}];
  Handle.advance_clock_by handle (Time_ns.Span.create ~sec:20 ());
  Handle.recompute_view_until_stable handle;
  (* Notification should be closed as more than threshold of time has passed. *)
  Handle.show_diff handle;
  [%expect
    {|
    -|<div class="notification_container_hash_replaced_in_test">
    -|  <div @key=0 data-notification-id="0" class="notification_hash_replaced_in_test" onclick>
    -|    <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    -|         style={
    -|           animation-name: fadeOut;
    -|           animation-duration: 15.00s;
    -|           animation-timing-function: ease-in;
    -|         }> An action occured! </div>
    -|  </div>
    -|</div>
    +|<div class="notification_container_hash_replaced_in_test"> </div> |}];
  Handle.do_actions
    handle
    [ Notification_spec.Add_notification
        { level =
            `Error
              (Error.of_lazy_sexp
                 (lazy
                   [%message "RPC timed out" ~rpc_that_timed_out:("my-bad-rpc" : string)]))
        ; text = "An action occured!"
        }
    ];
  Handle.show_diff handle;
  (* New error notification. *)
  [%expect
    {|
    -|<div class="notification_container_hash_replaced_in_test"> </div>
    +|<div class="notification_container_hash_replaced_in_test">
    +|  <div @key=1 data-notification-id="1" class="notification_hash_replaced_in_test" onclick>
    +|    <div class="error_hash_replaced_in_test notification_body_hash_replaced_in_test">
    +|      An action occured!
    +|      <pre> ("RPC timed out" (rpc_that_timed_out my-bad-rpc)) </pre>
    +|    </div>
    +|  </div>
    +|</div> |}];
  Handle.advance_clock_by handle (Time_ns.Span.create ~sec:20 ());
  Handle.recompute_view_until_stable handle;
  (* No diff expected - error notification should still be open. *)
  Handle.show_diff handle;
  [%expect {||}];
  Handle.click_on
    handle
    ~get_vdom:Notifications.to_vdom
    ~selector:"[data-notification-id=1]";
  Handle.show_diff handle;
  (* Notification should be closed as we clicked on it. *)
  [%expect
    {|
    -|<div class="notification_container_hash_replaced_in_test">
    -|  <div @key=1 data-notification-id="1" class="notification_hash_replaced_in_test" onclick>
    -|    <div class="error_hash_replaced_in_test notification_body_hash_replaced_in_test">
    -|      An action occured!
    -|      <pre> ("RPC timed out" (rpc_that_timed_out my-bad-rpc)) </pre>
    -|    </div>
    -|  </div>
    -|</div>
    +|<div class="notification_container_hash_replaced_in_test"> </div> |}];
  Handle.do_actions
    handle
    [ Notification_spec.Add_notification { level = `Success; text = "Notification #1" } ];
  Handle.advance_clock_by handle (Time_ns.Span.create ~sec:1 ());
  Handle.recompute_view handle;
  Handle.do_actions
    handle
    [ Notification_spec.Add_notification { level = `Success; text = "Notification #2" } ];
  Handle.show_diff handle;
  [%expect
    {|
    -|<div class="notification_container_hash_replaced_in_test"> </div>
    +|<div class="notification_container_hash_replaced_in_test">
    +|  <div @key=2 data-notification-id="2" class="notification_hash_replaced_in_test" onclick>
    +|    <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    +|         style={
    +|           animation-name: fadeOut;
    +|           animation-duration: 15.00s;
    +|           animation-timing-function: ease-in;
    +|         }> Notification #1 </div>
    +|  </div>
    +|  <div @key=3 data-notification-id="3" class="notification_hash_replaced_in_test" onclick>
    +|    <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    +|         style={
    +|           animation-name: fadeOut;
    +|           animation-duration: 15.00s;
    +|           animation-timing-function: ease-in;
    +|         }> Notification #2 </div>
    +|  </div>
    +|</div> |}];
  Handle.click_on
    handle
    ~get_vdom:Notifications.to_vdom
    ~selector:"[data-notification-id=2]";
  Handle.show_diff handle;
  (* Only "Notification #1" should be closed. *)
  [%expect
    {|
      <div class="notification_container_hash_replaced_in_test">
    -|  <div @key=2 data-notification-id="2" class="notification_hash_replaced_in_test" onclick>
    -|    <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    -|         style={
    -|           animation-name: fadeOut;
    -|           animation-duration: 15.00s;
    -|           animation-timing-function: ease-in;
    -|         }> Notification #1 </div>
    -|  </div>
        <div @key=3 data-notification-id="3" class="notification_hash_replaced_in_test" onclick>
          <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
               style={
                 animation-name: fadeOut;
                 animation-duration: 15.00s;
                 animation-timing-function: ease-in;
               }> Notification #2 </div>
        </div>
      </div> |}]
;;
