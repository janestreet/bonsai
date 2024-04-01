open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Basic_accordion = struct
  let name = "Basic Accordion Usage"
  let description = "Toggle the accordion by clicking its title bar"

  let view graph =
    let vdom, demo =
      [%demo
        let%sub { view; is_open = _; open_ = _; toggle = _; close = _ } =
          Bonsai_web_ui_accordion.component
            ~starts_open:true
            ~title:
              (Bonsai.return (Vdom.Node.text "I am an accordion, click me to toggle!"))
            ~content:(fun _graph -> Bonsai.return (Vdom.Node.text "I am the content!"))
            ()
            graph
        in
        view]
    in
    Bonsai.map vdom ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Accordion_with_controls = struct
  let name = "Accordion With External Controls"

  let description =
    "You can also control the accordion programmatically. Try clicking the buttons below."
  ;;

  let view graph =
    let computation, demo =
      let vbox = View.vbox ~gap:(`Em 1) in
      let hbox = View.hbox ~gap:(`Em_float 0.5) in
      [%demo
        let theme = View.Theme.current graph in
        let accordion =
          Bonsai_web_ui_accordion.component
            ~starts_open:false
            ~title:(Bonsai.return (Vdom.Node.text "Important!"))
            ~content:(fun _graph -> Bonsai.return (Vdom.Node.text "Wow, very important"))
            ()
            graph
        in
        let%arr { view; is_open; open_; toggle; close } = accordion
        and theme = theme in
        let open_button = View.button theme ~disabled:is_open ~on_click:open_ "Open" in
        let toggle_button = View.button theme ~on_click:toggle "Toggle" in
        let close_button =
          View.button theme ~disabled:(not is_open) ~on_click:close "Close"
        in
        vbox [ hbox [ open_button; toggle_button; close_button ]; view ]]
    in
    Bonsai.map computation ~f:(fun vdom -> vdom, demo)
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
       [ ( "Accordion"
         , {| Accordions can be used to toggle the visibility of different parts of your
         UI, which can lead to more compact and user-friendly user experiences|}
         , [ Gallery.make_demo (module Basic_accordion)
           ; Gallery.make_demo (module Accordion_with_controls)
           ] )
       ])
    graph
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component
;;
