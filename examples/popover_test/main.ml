open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Toggle_popover = struct
  let name = "Popover toggling"

  let description =
    {| This tests that there is not a bad interaction between popover toggling behavior and the [close_when_clicked_outside] flag. |}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let%sub theme = View.Theme.current in
        let popover_content ~close:_ = Bonsai.const (View.text "Popover contents") in
        let%sub popover =
          Bonsai_web_ui_popover.component
            ~close_when_clicked_outside:true
            ~direction:(Value.return Bonsai_web_ui_popover.Direction.Right)
            ~alignment:(Value.return Bonsai_web_ui_popover.Alignment.Center)
            ~popover:popover_content
            ()
        in
        let%arr { wrap; open_ = _; close = _; toggle; is_open = _ } = popover
        and theme = theme in
        wrap (View.button theme ~intent:Info ~on_click:toggle "toggle popover")]
    in
    Computation.map vdom ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Two_left_click_popovers = struct
  let name = "Multiple context menu popovers"

  let description =
    {|This test tests against a regression where clicking on the outside to click a context menu, also opened chrome's context menu.|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let%sub theme = View.Theme.current in
        let popover_content ~close:_ = Bonsai.const (View.text "Popover contents") in
        let popover =
          let%sub popover =
            Bonsai_web_ui_popover.component
              ~close_when_clicked_outside:true
              ~allow_event_propagation_when_clicked_outside:
                (Value.return (function
                   | `Left_click | `Escape -> false
                   | `Right_click -> true))
              ~direction:(Value.return Bonsai_web_ui_popover.Direction.Right)
              ~alignment:(Value.return Bonsai_web_ui_popover.Alignment.Center)
              ~popover:popover_content
              ()
          in
          let%arr { wrap; open_; close = _; toggle = _; is_open = _ } = popover
          and theme = theme in
          wrap
            (View.button
               theme
               ~intent:Info
               ~attrs:
                 [ Vdom.Attr.on_contextmenu (fun _ ->
                     Effect.Many [ open_; Effect.Prevent_default ])
                 ]
               ~on_click:open_
               "toggle popover")
        in
        let%map.Computation p1 = popover
        and p2 = popover in
        Vdom.Node.div [ p1; p2 ]]
    in
    Computation.map vdom ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Popover tests"
         , {|This file cont|}
         , [ Gallery.make_demo (module Two_left_click_popovers)
           ; Gallery.make_demo (module Toggle_popover)
           ] )
       ])
;;

let () =
  Async_js.init ();
  Auto_reload.refresh_on_build ();
  Bonsai_web.Start.start component
;;
