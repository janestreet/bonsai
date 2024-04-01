open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Toggle_popover = struct
  let name = "Popover toggling"

  let description =
    {| This tests that there is not a bad interaction between popover toggling behavior and the [close_when_clicked_outside] flag. |}
  ;;

  let view graph =
    let vdom, demo =
      [%demo
        fun graph ->
          let theme = View.Theme.current graph in
          let popover_content ~close:_ _graph =
            Bonsai.return (View.text "Popover contents")
          in
          let popover =
            Bonsai_web_ui_popover.component
              ~close_when_clicked_outside:(Bonsai.return true)
              ~direction:(Bonsai.return Bonsai_web_ui_popover.Direction.Right)
              ~alignment:(Bonsai.return Bonsai_web_ui_popover.Alignment.Center)
              ~popover:popover_content
              ()
              graph
          in
          let%arr { wrap; open_ = _; close = _; toggle; is_open = _ } = popover
          and theme = theme in
          wrap (View.button theme ~intent:Info ~on_click:toggle "toggle popover")]
    in
    Bonsai.map (vdom graph) ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Two_left_click_popovers = struct
  let name = "Multiple context menu popovers"

  let description =
    {|This test tests against a regression where clicking on the outside to click a context menu, also opened chrome's context menu.|}
  ;;

  let view graph =
    let vdom, demo =
      [%demo
        let theme = View.Theme.current graph in
        let popover_content ~close:_ _graph =
          Bonsai.return (View.text "Popover contents")
        in
        let popover graph =
          let popover =
            Bonsai_web_ui_popover.component
              ~close_when_clicked_outside:(Bonsai.return true)
              ~allow_event_propagation_when_clicked_outside:
                (Bonsai.return (function
                  | `Left_click | `Escape -> false
                  | `Right_click -> true))
              ~direction:(Bonsai.return Bonsai_web_ui_popover.Direction.Right)
              ~alignment:(Bonsai.return Bonsai_web_ui_popover.Alignment.Center)
              ~popover:popover_content
              ()
              graph
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
        let%map p1 = popover graph
        and p2 = popover graph in
        Vdom.Node.div [ p1; p2 ]]
    in
    Bonsai.map vdom ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component graph =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () graph in
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
    graph
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component
;;
