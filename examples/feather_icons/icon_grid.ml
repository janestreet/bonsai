open! Core
open! Import

module Style =
  [%css.raw
    {|
.cards {
  display: flex;
  flex-wrap: wrap;
  gap: 16px;
}

.card {
  display: flex;
  flex-direction: column;
  align-items: center;
  height: 165px;
  width: 165px;
  padding-bottom: 16px;
}

.label {
  flex: 0 0 auto;
  font-size: 14px;
  text-align: center;
  padding: 0 16px;
  word-break: break-word;
}

.icon {
  flex: 1 1 auto;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
}
    |}]

let icon_div icon ~controls =
  let { Controls.size; stroke_width; stroke; fill } = controls in
  let icon =
    Feather_icon.svg
      icon
      ~size:(`Px size)
      ~stroke_width:(`Px_float stroke_width)
      ~stroke
      ?fill
      ()
  in
  Vdom.Node.div ~attr:(Vdom.Attr.class_ Style.icon) [ icon ]
;;

let component ~icons ~controls =
  Vdom.Node.div
    ~attr:(Vdom.Attr.class_ Style.cards)
    (List.map icons ~f:(fun icon ->
       let icon_div = icon_div icon ~controls in
       let label_span =
         Vdom.Node.span
           ~attr:(Vdom.Attr.class_ Style.label)
           [ Vdom.Node.text (Feather_icon.to_string icon) ]
       in
       Vdom.Node.div
         ~key:(Feather_icon.to_string icon)
         ~attr:(Vdom.Attr.classes [ Card_like.class_; Style.card ])
         [ icon_div; label_span ]))
;;
