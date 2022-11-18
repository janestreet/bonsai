open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

module Theme_id = struct
  type t =
    | Default
    | Kado
  [@@deriving sexp, equal, enumerate, compare]
end

let theme_var =
  Persistent_var.create
    (module Theme_id)
    `Local_storage
    ~unique_id:"bonsai-view-theme"
    ~default:Default
;;

module Style =
  [%css.hash_variables
    stylesheet
      {|
  .container {
    position: fixed;
    padding: 0.5em 1em;
    width: fit-content;
    top: 0;
    background: var(--bg);
    border: 1px solid var(--border);
    border-top:0;
    z-index:1;
    border-bottom-left-radius: 3px;
    border-bottom-right-radius: 3px;
  }

  .container select {
    width: unset !important;
    font-size: inherit;
    padding: 0.2em 0.3em;
  }
|}]

let component =
  let var_value = Persistent_var.value theme_var in
  let%sub picker =
    Form.Elements.Dropdown.enumerable
      (module Theme_id)
      ~init:(`This var_value)
      ~to_string:(function
        | Default -> "Default"
        | Kado -> "Kado")
  in
  let%sub () =
    Bonsai_extra.mirror
      (module Theme_id)
      ~store_value:var_value
      ~store_set:(Value.return (Persistent_var.effect theme_var))
      ~interactive_value:(picker >>| Form.value_or_default ~default:Theme_id.Default)
      ~interactive_set:(picker >>| Form.set)
  in
  let%arr picker_view = picker >>| Form.view
  and theme_id = var_value in
  let theme =
    match theme_id with
    | Default -> View.Expert.default_theme
    | Kado -> Kado.theme ~version:Bleeding ()
  in
  let vars =
    Style.Variables.set
      ~border:(Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
      ~bg:(Css_gen.Color.to_string_css (View.primary_colors theme).background)
      ()
  in
  let view =
    View.hbox
      ~gap:(`Em 1)
      ~attr:(Vdom.Attr.many [ Style.container; vars ])
      (Vdom.Node.text "Pick a Theme" :: Form.View.to_vdom_plain picker_view)
  in
  theme, view
;;
