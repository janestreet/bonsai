open! Core
open Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax
module Notifications = Bonsai_web_ui_notifications

module Notification_spec = struct
  type t = Notifications.Basic.t * Vdom.Node.t

  type incoming =
    | Add_notification of
        { level : [ `Error of Error.t | `Success ]
        ; text : string
        }

  let view t =
    let module V = (val Result_spec.vdom Fn.id) in
    V.view (Tuple2.get2 t)
  ;;

  let incoming (t, _) incoming =
    match incoming with
    | Add_notification { level; text } ->
      (match level with
       | `Error error -> Notifications.Basic.add_error t ~error ~text
       | `Success -> Notifications.Basic.add_success t ~text)
  ;;
end

let%expect_test "render some notifications and test that they close as expected" =
  let open! Bonsai.Let_syntax in
  let handle =
    Handle.create
      (module Notification_spec)
      (let%sub notifications =
         Notifications.Basic.create
           ~dismiss_notifications_after:
             (Bonsai.Value.return (Time_ns.Span.create ~sec:15 ()))
           ~dismiss_errors_automatically:(Bonsai.Value.return false)
           ()
       in
       let%sub vdom = Notifications.Basic.render notifications in
       let%arr notifications = notifications
       and vdom = vdom in
       notifications, vdom)
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
    +|  <div @key=0 class="notification_hash_replaced_in_test">
    +|    <div @key=0 data-notification-id="0" class="notification_hash_replaced_in_test" onclick>
    +|      <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    +|           style={
    +|             animation-name: fadeOut;
    +|             animation-duration: 15.00s;
    +|             animation-timing-function: ease-in;
    +|           }> An action occured! </div>
    +|    </div>
    +|  </div>
    +|</div> |}];
  Handle.advance_clock_by handle (Time_ns.Span.create ~sec:20 ());
  Handle.recompute_view_until_stable handle;
  (* Notification should be closed as more than threshold of time has passed. *)
  Handle.show_diff handle;
  [%expect
    {|
    -|<div class="notification_container_hash_replaced_in_test">
    -|  <div @key=0 class="notification_hash_replaced_in_test">
    -|    <div @key=0 data-notification-id="0" class="notification_hash_replaced_in_test" onclick>
    -|      <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    -|           style={
    -|             animation-name: fadeOut;
    -|             animation-duration: 15.00s;
    -|             animation-timing-function: ease-in;
    -|           }> An action occured! </div>
    -|    </div>
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
    +|  <div @key=1 class="notification_hash_replaced_in_test">
    +|    <div @key=1 data-notification-id="1" class="notification_hash_replaced_in_test" onclick>
    +|      <div class="error_hash_replaced_in_test notification_body_hash_replaced_in_test">
    +|        An action occured!
    +|        <pre> ("RPC timed out" (rpc_that_timed_out my-bad-rpc)) </pre>
    +|      </div>
    +|    </div>
    +|  </div>
    +|</div> |}];
  Handle.advance_clock_by handle (Time_ns.Span.create ~sec:20 ());
  Handle.recompute_view_until_stable handle;
  (* No diff expected - error notification should still be open. *)
  Handle.show_diff handle;
  [%expect {||}];
  Handle.click_on handle ~get_vdom:Tuple2.get2 ~selector:"[data-notification-id=1]";
  Handle.show_diff handle;
  (* Notification should be closed as we clicked on it. *)
  [%expect
    {|
    -|<div class="notification_container_hash_replaced_in_test">
    -|  <div @key=1 class="notification_hash_replaced_in_test">
    -|    <div @key=1 data-notification-id="1" class="notification_hash_replaced_in_test" onclick>
    -|      <div class="error_hash_replaced_in_test notification_body_hash_replaced_in_test">
    -|        An action occured!
    -|        <pre> ("RPC timed out" (rpc_that_timed_out my-bad-rpc)) </pre>
    -|      </div>
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
    +|  <div @key=2 class="notification_hash_replaced_in_test">
    +|    <div @key=2 data-notification-id="2" class="notification_hash_replaced_in_test" onclick>
    +|      <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    +|           style={
    +|             animation-name: fadeOut;
    +|             animation-duration: 15.00s;
    +|             animation-timing-function: ease-in;
    +|           }> Notification #1 </div>
    +|    </div>
    +|  </div>
    +|  <div @key=3 class="notification_hash_replaced_in_test">
    +|    <div @key=3 data-notification-id="3" class="notification_hash_replaced_in_test" onclick>
    +|      <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    +|           style={
    +|             animation-name: fadeOut;
    +|             animation-duration: 15.00s;
    +|             animation-timing-function: ease-in;
    +|           }> Notification #2 </div>
    +|    </div>
    +|  </div>
    +|</div> |}];
  Handle.click_on handle ~get_vdom:Tuple2.get2 ~selector:"[data-notification-id=2]";
  Handle.show_diff handle;
  (* Only "Notification #1" should be closed. *)
  [%expect
    {|
      <div class="notification_container_hash_replaced_in_test">
    -|  <div @key=2 class="notification_hash_replaced_in_test">
    -|    <div @key=2 data-notification-id="2" class="notification_hash_replaced_in_test" onclick>
    -|      <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
    -|           style={
    -|             animation-name: fadeOut;
    -|             animation-duration: 15.00s;
    -|             animation-timing-function: ease-in;
    -|           }> Notification #1 </div>
    -|    </div>
    -|  </div>
        <div @key=3 class="notification_hash_replaced_in_test">
          <div @key=3 data-notification-id="3" class="notification_hash_replaced_in_test" onclick>
            <div class="notification_body_hash_replaced_in_test success_hash_replaced_in_test"
                 style={
                   animation-name: fadeOut;
                   animation-duration: 15.00s;
                   animation-timing-function: ease-in;
                 }> Notification #2 </div>
          </div>
        </div>
      </div> |}]
;;

let%test_module "generic notification test" =
  (module struct
    module Notification_type = struct
      type t =
        | A of string
        | B of string
      [@@deriving equal, sexp]

      let to_attr = function
        | A x -> Vdom.Attr.create "data-test" [%string "a-%{x}"]
        | B x -> Vdom.Attr.create "data-test" [%string "b-%{x}"]
      ;;

      let selector = function
        | A x -> [%string "[data-test=a-%{x}]"]
        | B x -> [%string "[data-test=b-%{x}]"]
      ;;
    end

    module Result_spec = struct
      type t = Vdom.Node.t * Notification_type.t Notifications.t
      type incoming = Notification_type.t * Time_ns.Span.t option

      let view (vdom, _) =
        let module V = (val Result_spec.vdom Fn.id) in
        V.view vdom
      ;;

      let incoming (_, notifications) (incoming_notification, close_after) =
        let f = Notifications.send_notification ?close_after notifications in
        Effect.ignore_m (f incoming_notification)
      ;;
    end

    let%expect_test "opening and closing a single notification" =
      let computation =
        let%sub notifications = Notifications.component (module Notification_type) in
        let%sub vdom =
          Notifications.render notifications ~f:(fun ~close element ->
            let%arr element = element
            and close = close in
            Vdom.Node.div
              ~attrs:
                [ Notification_type.to_attr element; Vdom.Attr.on_click (fun _ -> close) ]
              [ Vdom.Node.sexp_for_debugging [%message (element : Notification_type.t)] ])
        in
        let%arr vdom = vdom
        and notifications = notifications in
        vdom, notifications
      in
      let handle = Handle.create (module Result_spec) computation in
      Handle.show handle;
      [%expect {| <div class="notification_container_hash_replaced_in_test"> </div> |}];
      Handle.do_actions handle [ A "1", None ];
      Handle.show_diff handle;
      [%expect
        {|
        -|<div class="notification_container_hash_replaced_in_test"> </div>
        +|<div class="notification_container_hash_replaced_in_test">
        +|  <div @key=0 class="notification_hash_replaced_in_test">
        +|    <div data-test="a-1" onclick>
        +|      <pre> (element (A 1)) </pre>
        +|    </div>
        +|  </div>
        +|</div> |}];
      Handle.click_on
        handle
        ~get_vdom:Tuple2.get1
        ~selector:(Notification_type.selector (A "1"));
      Handle.show_diff handle;
      [%expect
        {|
        -|<div class="notification_container_hash_replaced_in_test">
        -|  <div @key=0 class="notification_hash_replaced_in_test">
        -|    <div data-test="a-1" onclick>
        -|      <pre> (element (A 1)) </pre>
        -|    </div>
        -|  </div>
        -|</div>
        +|<div class="notification_container_hash_replaced_in_test"> </div> |}]
    ;;

    let bare_bones_notification_ui =
      let%sub notifications = Notifications.component (module Notification_type) in
      let%sub vdom =
        Notifications.render notifications ~f:(fun ~close:_ element ->
          let%arr element = element in
          Vdom.Node.sexp_for_debugging [%message (element : Notification_type.t)])
      in
      let%arr vdom = vdom
      and notifications = notifications in
      vdom, notifications
    ;;

    let%expect_test "manually closing a notification" =
      let handle = Handle.create (module Result_spec) bare_bones_notification_ui in
      let _, notification_handle = Handle.result handle in
      let id_ref = ref None in
      Effect.Expert.handle_non_dom_event_exn
        (let%bind.Effect id1 =
           Notifications.send_notification notification_handle (A "1")
         in
         let%map.Effect id2 =
           Notifications.send_notification notification_handle (A "2")
         in
         id_ref := Some (id1, id2));
      Handle.show handle;
      [%expect
        {|
        <div class="notification_container_hash_replaced_in_test">
          <div @key=0 class="notification_hash_replaced_in_test">
            <pre> (element (A 1)) </pre>
          </div>
          <div @key=1 class="notification_hash_replaced_in_test">
            <pre> (element (A 2)) </pre>
          </div>
        </div> |}];
      let id1, id2 = Option.value_exn !id_ref in
      Effect.Expert.handle_non_dom_event_exn
        (Notifications.close_notification notification_handle id1);
      Handle.show_diff handle;
      [%expect
        {|
          <div class="notification_container_hash_replaced_in_test">
        -|  <div @key=0 class="notification_hash_replaced_in_test">
        -|    <pre> (element (A 1)) </pre>
        -|  </div>
            <div @key=1 class="notification_hash_replaced_in_test">
              <pre> (element (A 2)) </pre>
            </div>
          </div> |}];
      Effect.Expert.handle_non_dom_event_exn
        (Notifications.close_notification notification_handle id2);
      Handle.show_diff handle;
      [%expect
        {|
        -|<div class="notification_container_hash_replaced_in_test">
        -|  <div @key=1 class="notification_hash_replaced_in_test">
        -|    <pre> (element (A 2)) </pre>
        -|  </div>
        -|</div>
        +|<div class="notification_container_hash_replaced_in_test"> </div> |}]
    ;;

    let%expect_test "manually modifying a notification" =
      let handle = Handle.create (module Result_spec) bare_bones_notification_ui in
      let _, notification_handle = Handle.result handle in
      let id_ref = ref None in
      Effect.Expert.handle_non_dom_event_exn
        (let%bind.Effect id1 =
           Notifications.send_notification notification_handle (A "1")
         in
         let%map.Effect id2 =
           Notifications.send_notification notification_handle (A "2")
         in
         id_ref := Some (id1, id2));
      Handle.show handle;
      [%expect
        {|
        <div class="notification_container_hash_replaced_in_test">
          <div @key=0 class="notification_hash_replaced_in_test">
            <pre> (element (A 1)) </pre>
          </div>
          <div @key=1 class="notification_hash_replaced_in_test">
            <pre> (element (A 2)) </pre>
          </div>
        </div> |}];
      let id1, id2 = Option.value_exn !id_ref in
      Effect.Expert.handle_non_dom_event_exn
        (Notifications.modify_notification notification_handle id1 (A "3"));
      Handle.show_diff handle;
      [%expect
        {|
          <div class="notification_container_hash_replaced_in_test">
            <div @key=0 class="notification_hash_replaced_in_test">
        -|    <pre> (element (A 1)) </pre>
        +|    <pre> (element (A 3)) </pre>
            </div>
            <div @key=1 class="notification_hash_replaced_in_test">
              <pre> (element (A 2)) </pre>
            </div>
          </div> |}];
      Effect.Expert.handle_non_dom_event_exn
        (Notifications.modify_notification notification_handle id2 (A "10"));
      Handle.show_diff handle;
      [%expect
        {|
          <div class="notification_container_hash_replaced_in_test">
            <div @key=0 class="notification_hash_replaced_in_test">
              <pre> (element (A 3)) </pre>
            </div>
            <div @key=1 class="notification_hash_replaced_in_test">
        -|    <pre> (element (A 2)) </pre>
        +|    <pre> (element (A 10)) </pre>
            </div>
          </div> |}]
    ;;

    let%expect_test "opening and closing many notifications" =
      let computation =
        let%sub notifications = Notifications.component (module Notification_type) in
        let%sub vdom =
          Notifications.render notifications ~f:(fun ~close element ->
            let%arr element = element
            and close = close in
            Vdom.Node.div
              ~attrs:
                [ Notification_type.to_attr element; Vdom.Attr.on_click (fun _ -> close) ]
              [ Vdom.Node.sexp_for_debugging [%message (element : Notification_type.t)] ])
        in
        let%arr vdom = vdom
        and notifications = notifications in
        vdom, notifications
      in
      let handle = Handle.create (module Result_spec) computation in
      Handle.show handle;
      [%expect {| <div class="notification_container_hash_replaced_in_test"> </div> |}];
      Handle.do_actions
        handle
        [ A "1", None; B "2", None; A "3", None; B "4", None; A "5", None; B "6", None ];
      Handle.show_diff handle;
      [%expect
        {|
        -|<div class="notification_container_hash_replaced_in_test"> </div>
        +|<div class="notification_container_hash_replaced_in_test">
        +|  <div @key=0 class="notification_hash_replaced_in_test">
        +|    <div data-test="a-1" onclick>
        +|      <pre> (element (A 1)) </pre>
        +|    </div>
        +|  </div>
        +|  <div @key=1 class="notification_hash_replaced_in_test">
        +|    <div data-test="b-2" onclick>
        +|      <pre> (element (B 2)) </pre>
        +|    </div>
        +|  </div>
        +|  <div @key=2 class="notification_hash_replaced_in_test">
        +|    <div data-test="a-3" onclick>
        +|      <pre> (element (A 3)) </pre>
        +|    </div>
        +|  </div>
        +|  <div @key=3 class="notification_hash_replaced_in_test">
        +|    <div data-test="b-4" onclick>
        +|      <pre> (element (B 4)) </pre>
        +|    </div>
        +|  </div>
        +|  <div @key=4 class="notification_hash_replaced_in_test">
        +|    <div data-test="a-5" onclick>
        +|      <pre> (element (A 5)) </pre>
        +|    </div>
        +|  </div>
        +|  <div @key=5 class="notification_hash_replaced_in_test">
        +|    <div data-test="b-6" onclick>
        +|      <pre> (element (B 6)) </pre>
        +|    </div>
        +|  </div>
        +|</div> |}];
      let click notifications =
        List.iter notifications ~f:(fun notification ->
          Handle.click_on
            handle
            ~get_vdom:Tuple2.get1
            ~selector:(Notification_type.selector notification))
      in
      click [ B "2"; B "4"; B "6" ];
      Handle.show_diff handle;
      [%expect
        {|
          <div class="notification_container_hash_replaced_in_test">
            <div @key=0 class="notification_hash_replaced_in_test">
              <div data-test="a-1" onclick>
                <pre> (element (A 1)) </pre>
              </div>
            </div>
        -|  <div @key=1 class="notification_hash_replaced_in_test">
        -|    <div data-test="b-2" onclick>
        -|      <pre> (element (B 2)) </pre>
        -|    </div>
        -|  </div>
            <div @key=2 class="notification_hash_replaced_in_test">
              <div data-test="a-3" onclick>
                <pre> (element (A 3)) </pre>
              </div>
            </div>
        -|  <div @key=3 class="notification_hash_replaced_in_test">
        -|    <div data-test="b-4" onclick>
        -|      <pre> (element (B 4)) </pre>
        -|    </div>
        -|  </div>
            <div @key=4 class="notification_hash_replaced_in_test">
              <div data-test="a-5" onclick>
                <pre> (element (A 5)) </pre>
              </div>
            </div>
        -|  <div @key=5 class="notification_hash_replaced_in_test">
        -|    <div data-test="b-6" onclick>
        -|      <pre> (element (B 6)) </pre>
        -|    </div>
        -|  </div>
          </div> |}]
    ;;

    let%expect_test "notification closig due to time expiry at different times" =
      let computation =
        let%sub notifications = Notifications.component (module Notification_type) in
        let%sub vdom =
          Notifications.render notifications ~f:(fun ~close:_ element ->
            let%arr element = element in
            Vdom.Node.div
              ~attrs:[ Notification_type.to_attr element ]
              [ Vdom.Node.sexp_for_debugging [%message (element : Notification_type.t)] ])
        in
        let%arr vdom = vdom
        and notifications = notifications in
        vdom, notifications
      in
      let handle = Handle.create (module Result_spec) computation in
      Handle.show handle;
      [%expect {| <div class="notification_container_hash_replaced_in_test"> </div> |}];
      Handle.do_actions
        handle
        [ A "1", Some (Time_ns.Span.of_sec 1.0); B "2", Some (Time_ns.Span.of_sec 2.0) ];
      Handle.show_diff handle;
      [%expect
        {|
        -|<div class="notification_container_hash_replaced_in_test"> </div>
        +|<div class="notification_container_hash_replaced_in_test">
        +|  <div @key=0 class="notification_hash_replaced_in_test">
        +|    <div data-test="a-1">
        +|      <pre> (element (A 1)) </pre>
        +|    </div>
        +|  </div>
        +|  <div @key=1 class="notification_hash_replaced_in_test">
        +|    <div data-test="b-2">
        +|      <pre> (element (B 2)) </pre>
        +|    </div>
        +|  </div>
        +|</div> |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      (* NOTE: Due to the implementation of the notifications API, there is a one frame
         delay for the actual removal to occur. *)
      Handle.show_diff handle;
      [%expect {||}];
      Handle.show_diff handle;
      (* First notification disappears after 1 second as specified. *)
      [%expect
        {|
          <div class="notification_container_hash_replaced_in_test">
        -|  <div @key=0 class="notification_hash_replaced_in_test">
        -|    <div data-test="a-1">
        -|      <pre> (element (A 1)) </pre>
        -|    </div>
        -|  </div>
            <div @key=1 class="notification_hash_replaced_in_test">
              <div data-test="b-2">
                <pre> (element (B 2)) </pre>
              </div>
            </div>
          </div> |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.show_diff handle;
      [%expect {||}];
      (* Second notification disappears after two seconds. *)
      Handle.show_diff handle;
      [%expect
        {|
        -|<div class="notification_container_hash_replaced_in_test">
        -|  <div @key=1 class="notification_hash_replaced_in_test">
        -|    <div data-test="b-2">
        -|      <pre> (element (B 2)) </pre>
        -|    </div>
        -|  </div>
        -|</div>
        +|<div class="notification_container_hash_replaced_in_test"> </div> |}]
    ;;
  end)
;;
