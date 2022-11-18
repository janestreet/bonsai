open! Core
open! Import
module Style = Devbar_style

let make constants ~attr ~count ~intent text =
  let colors = Option.map intent ~f:(View.Constants.Intent.lookup constants.intent) in
  let attr =
    match colors with
    | None -> attr
    | Some { Fg_bg.foreground; background } ->
      let colors =
        Style.Variables.set
          ~fst:(Css_gen.Color.to_string_css foreground)
          ~snd:(Css_gen.Color.to_string_css background)
          ()
      in
      Vdom.Attr.many [ attr; colors ]
  in
  let rep = List.init count ~f:(fun _ -> Vdom.Node.span [ Vdom.Node.text text ]) in
  Vdom.Node.div
    ~attr:(Vdom.Attr.many [ Style.devbar; attr ])
    [ Vdom.Node.div ~attr:Style.container (Vdom.Node.div ~attr:Style.background [] :: rep)
    ]
;;
