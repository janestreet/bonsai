open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

let card ~theme ~name ~content =
  View.card'
    theme
    ~title_attrs:[ Vdom.Attr.style (Css_gen.flex_container ~justify_content:`Center ()) ]
    ~container_attrs:
      [ Vdom.Attr.style (Css_gen.create ~field:"width" ~value:"fit-content") ]
    ~title:[ Vdom.Node.text name ]
    content
;;

let buttons graph =
  let%map.Bonsai buttons = Button.component graph
  and theme = View.Theme.current graph in
  card ~theme ~name:"buttons" ~content:buttons
;;

let textbox graph =
  let%map.Bonsai textbox = Textbox.component graph
  and theme = View.Theme.current graph in
  card ~theme ~name:"input" ~content:textbox
;;

let app graph =
  let%map.Bonsai buttons = buttons graph
  and textbox = textbox graph in
  View.hbox ~gap:(`Em 1) [ buttons; textbox ]
;;

let app graph =
  let theme = Bonsai.return (Kado.theme ~version:Bleeding ()) in
  View.Theme.set_for_app theme app graph
;;

let () = Bonsai_web.Start.start app
