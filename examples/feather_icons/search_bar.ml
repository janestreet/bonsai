open! Core
open! Import

module Search_container = [%css.raw {|
.class_ {
  position: relative;
}
|}]

module Search_icon =
  [%css.raw
    {|
.class_ {
  position: absolute;
  left: 16px;
  top: 8px;
}
|}]

module Search_bar =
  [%css.raw
    {|
.class_ {
  width: 100%;
  height: 40px;
  font-size: 16px;
  text-indent: 40px;
}
|}]

let component =
  let%sub search_bar =
    Form.Elements.Textbox.string
      ~extra_attrs:
        (Value.return [ Vdom.Attr.classes [ Card_like.class_; Search_bar.class_ ] ])
      ~placeholder:"Search icons"
      [%here]
  in
  let%arr search_bar = search_bar in
  let icons =
    match Form.value search_bar with
    | Error error ->
      eprint_s [%message "failed to get value from search bar" (error : Error.t)];
      Feather_icon.all
    | Ok search ->
      List.filter Feather_icon.all ~f:(fun icon ->
        let icon_string = Feather_icon.to_string icon in
        Fuzzy_match.is_match icon_string ~char_equal:Char.Caseless.equal ~pattern:search)
  in
  let search_icon =
    let gray = `Hex "#959da5" in
    Vdom.Node.div
      ~attr:(Vdom.Attr.class_ Search_icon.class_)
      [ Feather_icon.svg Search ~stroke:gray () ]
  in
  let view =
    Vdom.Node.div
      ~attr:(Vdom.Attr.class_ Search_container.class_)
      (search_icon :: (Form.view search_bar |> Form.View.to_vdom_plain))
  in
  icons, view
;;
