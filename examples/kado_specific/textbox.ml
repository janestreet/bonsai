open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let on_change _ = Effect.Ignore

let component =
  let%map.Computation theme = View.Theme.current in
  [ Kado.Unstable.Input.textbox
      ~constants:(View.constants theme)
      ~input_attr:Vdom.Attr.empty
      ~container_attr:Vdom.Attr.empty
      ~title:(Some "text box")
      ~on_change
      ~value:"testing"
  ; Kado.Unstable.Input.dropdown
      ~constants:(View.constants theme)
      ~input_attr:Vdom.Attr.empty
      ~container_attr:Vdom.Attr.empty
      ~title:(Some "drop down")
      ~on_change
      ~options:
        [ "foo", true, Vdom.Node.text "foo"
        ; "bar", false, Vdom.Node.text "bar"
        ; "baz", false, Vdom.Node.text "baz"
        ]
  ; Kado.Unstable.Input.date
      ~constants:(View.constants theme)
      ~input_attr:Vdom.Attr.empty
      ~container_attr:Vdom.Attr.empty
      ~title:(Some "birthday")
      ~on_change
      ~value:"1993-12-09"
  ; Kado.Unstable.Input.datetime
      ~constants:(View.constants theme)
      ~input_attr:Vdom.Attr.empty
      ~container_attr:Vdom.Attr.empty
      ~title:(Some "birthday")
      ~on_change
      ~value:"1993-12-09"
  ; Kado.Unstable.Input.checkbox
      ~constants:(View.constants theme)
      ~input_attr:Vdom.Attr.empty
      ~container_attr:Vdom.Attr.empty
      ~checked:false
      ~on_change
      ~label:(Vdom.Node.text "reticulate splines")
  ; Kado.Unstable.Input.button_vbox
      [ Kado.Unstable.Input.checkbox
          ~constants:(View.constants theme)
          ~input_attr:Vdom.Attr.empty
          ~container_attr:Vdom.Attr.empty
          ~checked:true
          ~on_change
          ~label:(Vdom.Node.text "refrigerate leftovers")
      ; Kado.Unstable.Input.checkbox
          ~constants:(View.constants theme)
          ~input_attr:Vdom.Attr.empty
          ~container_attr:Vdom.Attr.empty
          ~on_change
          ~checked:true
          ~label:(Vdom.Node.text "invert polarity")
      ; Kado.Unstable.Input.checkbox
          ~constants:(View.constants theme)
          ~input_attr:Vdom.Attr.empty
          ~container_attr:Vdom.Attr.empty
          ~on_change
          ~checked:false
          ~label:(Vdom.Node.text "realign deck chairs")
      ]
  ]
;;
