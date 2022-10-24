open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Level = struct
  type t =
    | Success
    | Error of Error.t option
  [@@deriving compare, sexp]
end

module Notification = struct
  module T = struct
    type t =
      { text : string
      ; level : Level.t
      ; opened_at : Time_ns.Alternate_sexp.t
      ; open_for_duration : Time_ns.Span.t
      }
    [@@deriving compare, fields, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Style =
  [%css
    stylesheet
      {|
.notification_container {
    position: fixed;
    bottom: 20px;
    right: 20px;
}

.notification_container .notification {
    color: white;
    cursor: pointer;
    text-align: right;
}

.notification_container .notification:not(:last-child) {
    margin-bottom: 20px;
}

.notification_body {
    padding: 15px;
}

.notification_body > pre {
    background-color: unset;
    font-size: x-small;
    margin-bottom: 0;
}

@keyframes fadeOut {
    0% {
        opacity: 1;
    }

    100% {
        opacity: 0;
    }
}
|}]

module Notification_style =
  [%css
    stylesheet
      {|
.success {
    background-color: #00AB66;
}

.error {
    background-color: #C70039;
}
|}]

module Notification_id = Bonsai_extra.Id_gen (Int) ()

module Action = struct
  type t =
    | Add of Notification_id.t * Notification.t
    | Remove of Notification_id.t
  [@@deriving equal, sexp]
end

type t =
  { notifications : Notification.t Map.M(Notification_id).t
  ; inject_notification_action : Action.t -> unit Ui_effect.t
  ; id_generator : Notification_id.t Ui_effect.t
  ; dismiss_notifications_after : Time_ns.Span.t
  ; dismiss_errors_automatically : bool
  ; get_now : Time_ns.t Effect.t
  }
[@@deriving fields]

let create
      ?(dismiss_notifications_after = Value.return (Time_ns.Span.create ~sec:15 ()))
      ?(dismiss_errors_automatically = Value.return false)
      ()
  =
  let%sub id_generator = Notification_id.component in
  let%sub notifications =
    Bonsai.state_machine0
      (module struct
        type t = Notification.t Map.M(Notification_id).t [@@deriving equal, sexp]
      end)
      (module Action)
      ~default_model:(Map.empty (module Notification_id))
      ~apply_action:(fun ~inject:_ ~schedule_event:_ notifications action ->
        match action with
        | Add (notification_id, notification) ->
          Map.set notifications ~key:notification_id ~data:notification
        | Remove notification_id -> Map.remove notifications notification_id)
  in
  let%sub notification_expiry_map =
    Bonsai.assoc
      (module Notification_id)
      (notifications >>| fst)
      ~f:(fun _key notification ->
        Bonsai.Clock.at
          (let%map { Notification.opened_at; open_for_duration; text = _; level = _ } =
             notification
           in
           Time_ns.add opened_at open_for_duration))
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module struct
        type t = Bonsai.Clock.Before_or_after.t Map.M(Notification_id).t
        [@@deriving equal, sexp]
      end)
      notification_expiry_map
      ~callback:
        (let%map notifications, inject_notification_action = notifications
         and dismiss_errors_automatically = dismiss_errors_automatically in
         fun notification_expiry_map ->
           let effects =
             Map.fold2
               ~init:[]
               notification_expiry_map
               notifications
               ~f:(fun ~key ~data accum ->
                 match data with
                 | `Left expiration_without_notification ->
                   Ui_effect.print_s
                     [%message
                       "BUG in bonsai_web_ui_notifications: notification present in \
                        expiration map, but not in notification map."
                         (expiration_without_notification
                          : Bonsai.Clock.Before_or_after.t)]
                   :: accum
                 | `Right notification ->
                   Ui_effect.print_s
                     [%message
                       "BUG in bonsai_web_ui_notifications: notification present in \
                        notifications map, but not in expiration map"
                         (notification : Notification.t)]
                   :: accum
                 | `Both (Bonsai.Clock.Before_or_after.Before, _) -> accum
                 | `Both (After, { Notification.level; _ }) ->
                   (match level with
                    | Success -> inject_notification_action (Remove key) :: accum
                    | Error _ ->
                      if dismiss_errors_automatically
                      then inject_notification_action (Remove key) :: accum
                      else accum))
           in
           Ui_effect.Many effects)
  in
  let%sub get_now = Bonsai.Clock.get_current_time in
  let%arr notifications, inject_notification_action = notifications
  and get_now = get_now
  and dismiss_notifications_after = dismiss_notifications_after
  and dismiss_errors_automatically = dismiss_errors_automatically
  and id_generator = id_generator in
  { notifications
  ; inject_notification_action
  ; id_generator
  ; dismiss_notifications_after
  ; dismiss_errors_automatically
  ; get_now
  }
;;

let add_notification
      { notifications = _
      ; inject_notification_action
      ; id_generator
      ; dismiss_notifications_after
      ; dismiss_errors_automatically = _
      ; get_now
      }
      ~text
      ~level
  =
  let%bind.Effect now = get_now in
  let notification =
    Notification.Fields.create
      ~text
      ~level
      ~opened_at:now
      ~open_for_duration:dismiss_notifications_after
  in
  let%bind.Effect notification_id = id_generator in
  inject_notification_action (Add (notification_id, notification))
;;

let add_error ?error = add_notification ~level:(Error error)
let add_success = add_notification ~level:Success

let to_vdom
      ?(notification_style = Notification_style.default)
      ?(notification_extra_attr = Vdom.Attr.empty)
      { notifications
      ; inject_notification_action
      ; dismiss_errors_automatically
      ; id_generator = _
      ; dismiss_notifications_after = _
      ; get_now = _
      }
  =
  let module Notification_style = (val notification_style) in
  Vdom.Node.div
    ~attr:(Vdom.Attr.class_ Style.notification_container)
    (Map.to_alist notifications
     |> List.sort ~compare:(fun (_, a) (_, b) ->
       [%compare: Time_ns.t] a.Notification.opened_at b.opened_at)
     |> List.map
          ~f:(fun
               ( notification_id
               , { Notification.text; level; opened_at = _; open_for_duration } )
               ->
                 let level_class =
                   match level with
                   | Success -> Notification_style.success
                   | Error _ -> Notification_style.error
                 in
                 let should_dismiss_automatically =
                   match level, dismiss_errors_automatically with
                   | Success, _ | Error _, true -> true
                   | Error _, false -> false
                 in
                 Vdom.Node.div
                   ~key:(Notification_id.to_string notification_id)
                   ~attr:
                     Vdom.Attr.(
                       class_ Style.notification
                       @ on_click (fun (_ : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) ->
                         inject_notification_action (Remove notification_id))
                       @ notification_extra_attr
                       @ create
                           "data-notification-id"
                           (Notification_id.to_string notification_id))
                   [ Vdom.Node.div
                       ~attr:
                         Vdom.Attr.(
                           classes [ Style.notification_body; level_class ]
                           @
                           if should_dismiss_automatically
                           then
                             style
                               (let open Css_gen in
                                animation
                                  ~name:"fadeOut"
                                  ~duration:open_for_duration
                                  ~timing_function:"ease-in"
                                  ())
                           else empty)
                       [ Vdom.Node.text text
                       ; (match level with
                          | Success | Error None -> Vdom.Node.None
                          | Error (Some error) ->
                            Vdom.Node.pre [ Vdom.Node.text (Error.to_string_hum error) ])
                       ]
                   ]))
;;
