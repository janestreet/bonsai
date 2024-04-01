open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Basic_notification = struct
  let name = "Basic Notifications test"

  let description =
    {| The Basic module provides short-hand aliases for a pre-defined notification type to show that
there are no changes to the previous API.|}
  ;;

  include
    [%demo
    module Notifications = Bonsai_web_ui_notifications

    let component graph =
      let notifications = Notifications.Basic.create () graph in
      let vdom = Notifications.Basic.render notifications graph in
      let%arr vdom = vdom
      and notifications = notifications in
      vdom, notifications
    ;;]

  let view graph =
    let theme = View.Theme.current graph in
    let%sub component, notifications = component graph in
    let%arr component = component
    and notifications = notifications
    and theme = theme in
    let vdom =
      View.hbox
        ~gap:(`Em 1)
        [ View.button
            theme
            ~on_click:
              (Bonsai_web_ui_notifications.Basic.add_success notifications ~text:"Yay!")
            ~intent:Success
            "Send success notification"
        ; View.button
            theme
            ~on_click:
              (Bonsai_web_ui_notifications.Basic.add_error
                 notifications
                 ~error:(Error.of_string "Something went wrong.")
                 ~text:"Whoops!")
            ~intent:Error
            "Send error notification"
        ; component
        ]
    in
    vdom, ppx_demo_string
  ;;

  let selector = None
  let filter_attrs = None
end

let component graph =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () graph in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Notifications test"
         , {| This is the Bonsai example for internal use for Bonsai devs to make sure that we
         are backwards compatible with the old API without showing the old API in the homepage.|}
         , [ Gallery.make_demo (module Basic_notification) ] )
       ])
    graph
;;

let () = Start.start ~bind_to_element_with_id:"app" component
