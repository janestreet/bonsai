open! Core
open! Import
module Style = Devbar_style

let make constants ~is_dark ~attrs ~count ~intent text =
  let colors = Option.map intent ~f:(View.Constants.Intent.lookup constants.intent) in
  let colors =
    match colors with
    | None -> Vdom.Attr.empty
    | Some { Fg_bg.foreground; background } ->
      Style.Variables.set
        ~fst:(Css_gen.Color.to_string_css foreground)
        ~snd:(Css_gen.Color.to_string_css background)
        ()
  in
  let rep = List.init count ~f:(fun _ -> Vdom.Node.span [ Vdom.Node.text text ]) in
  Vdom.Node.div
    ~attrs:
      (attrs @ [ Style.devbar; colors; (if is_dark then Style.dark else Vdom.Attr.empty) ])
    [ Vdom.Node.div
        ~attrs:[ Style.container ]
        (Vdom.Node.div ~attrs:[ Style.background ] [] :: rep)
    ]
;;
