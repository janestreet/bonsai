open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form.With_automatic_view

let tomato = `Hex "#FF6347"

let component =
  let open Bonsai.Let_syntax in
  let%sub applied, toggle = Bonsai.toggle ~default_model:true in
  let%sub color_form =
    Form.Elements.Color_picker.hex ()
    |> Bonsai.sub ~f:(Form.Dynamic.with_default (Value.return tomato))
  in
  let%sub toggle_button =
    let%sub theme = View.Theme.current in
    let%arr toggle = toggle
    and applied = applied
    and theme = theme in
    View.button
      theme
      ~on_click:toggle
      (match applied with
       | false -> "Apply"
       | true -> "Disable")
  in
  let%sub attr =
    match%sub applied with
    | false -> Bonsai.const Vdom.Attr.empty
    | true ->
      let%sub color =
        let%arr color_form = color_form in
        Form.value_or_default color_form ~default:tomato
      in
      let%arr color = color in
      Inline_css.Private.Dynamic.attr
        [%string
          {|
        :root {
        background-color: %{Css_gen.Color.to_string_css color}
        }
      |}]
  in
  let%arr attr = attr
  and color_form = color_form
  and toggle_button = toggle_button in
  Vdom.Node.div
    ~attrs:
      [ attr
      ; [%css {|
          display: flex; 
          flex-direction: row;
        |}]
      ]
    [ toggle_button; Form.view_as_vdom color_form ]
;;

let () = Bonsai_web.Start.start component
