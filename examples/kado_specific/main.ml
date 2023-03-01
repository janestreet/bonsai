open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let card ~theme ~name ~content =
  View.card'
    theme
    ~title_attr:(Vdom.Attr.style (Css_gen.flex_container ~justify_content:`Center ()))
    ~container_attr:(Vdom.Attr.style (Css_gen.create ~field:"width" ~value:"fit-content"))
    ~title:[ Vdom.Node.text name ]
    content
;;

let buttons =
  let%map.Computation buttons = Button.component
  and theme = View.Theme.current in
  card ~theme ~name:"buttons" ~content:buttons
;;

let textbox =
  let%map.Computation textbox = Textbox.component
  and theme = View.Theme.current in
  card ~theme ~name:"input" ~content:textbox
;;

let app =
  let%map.Computation buttons = buttons
  and textbox = textbox in
  View.hbox ~gap:(`Em 1) [ buttons; textbox ]
;;

let app =
  let theme = Value.return (Kado.theme ~version:Bleeding ()) in
  View.Theme.set_for_app theme app
;;

let () =
  let () = Auto_reload.refresh_on_build () in
  Bonsai_web.Start.start app
;;
