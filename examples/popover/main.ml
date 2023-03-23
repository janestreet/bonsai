open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Popover = struct
  let name = "Popover"

  let description =
    {| This popover gives a warning that prompts for extra confirmation. |}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let%sub theme = View.Theme.current in
        let popover_content ~close =
          let%arr close = close
          and theme = theme in
          View.button theme ~on_click:close "Close popover"
        in
        let%sub popover =
          Bonsai_web_ui_popover.component
            ~close_when_clicked_outside:true
            ~direction:(Value.return Bonsai_web_ui_popover.Direction.Right)
            ~alignment:(Value.return Bonsai_web_ui_popover.Alignment.Center)
            ~popover:popover_content
            ()
        in
        let%arr { wrap; open_; close = _; toggle = _; is_open = _ } = popover
        and theme = theme in
        wrap (View.button theme ~intent:Info ~on_click:open_ "Open Popover")]
    in
    Computation.map vdom ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Context_menu_popover = struct
  let name = "Context menu popover"

  let description =
    {| This popover shows available actions, similar to a context menu. It also showcases nested popovers.|}
  ;;

  let view =
    let computation, demo =
      [%demo
        let%sub theme = View.Theme.current in
        let%sub popover =
          Bonsai_web_ui_popover.component
            ~close_when_clicked_outside:true
            ~direction:(Value.return Bonsai_web_ui_popover.Direction.Right)
            ~alignment:(Value.return Bonsai_web_ui_popover.Alignment.Center)
            ~popover:(fun ~close:_ ->
              let%arr theme = theme in
              View.vbox
                [ View.text "Context Menu"
                ; View.button theme ~intent:Success ~on_click:Effect.Ignore "Action 1"
                ; View.button theme ~intent:Warning ~on_click:Effect.Ignore "Action 2"
                ])
            ()
        in
        let%arr { wrap; open_; _ } = popover in
        let attr =
          Vdom.Attr.on_contextmenu (fun _ ->
            Effect.Many [ open_; Effect.Prevent_default ])
        in
        wrap (View.text ~attrs:[ attr ] "Right click me!")]
    in
    Computation.map computation ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component =
  let%sub theme, theme_picker = Gallery.Theme_picker.component in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Popover"
         , {| Popovers are like tooltips, but they give you finer grain control as to when
         it opens and closes rather than just opening while hovering.|}
         , [ Gallery.make_demo (module Popover)
           ; Gallery.make_demo (module Context_menu_popover)
           ] )
       ])
;;

let () =
  Async_js.init ();
  Auto_reload.refresh_on_build ();
  Bonsai_web.Start.start component
;;
