open! Core
open Import
module Style = Cards_style

let make
      constants
      ~container_attr
      ~title_attr
      ~content_attr
      ~intent
      ~on_click
      ~title
      ~title_kind
      ~content
  =
  let title_fg, title_bg, title_border =
    let { Fg_bg.foreground; background }, title_border =
      match intent with
      | None -> constants.extreme, constants.extreme_primary_border
      | Some intent ->
        let border =
          Css_gen.Color.RGBA.create ~r:255 ~g:255 ~b:255 ~a:(Percent.of_mult 0.3) ()
        in
        Intent.lookup constants.intent intent, `RGBA border
    in
    Css_gen.Color.(
      to_string_css foreground, to_string_css background, to_string_css title_border)
  in
  let fg, bg, border =
    let { Fg_bg.foreground = fg; background = bg } = constants.primary in
    let border =
      match intent, title_kind with
      | Some intent, Card_title_kind.Discreet ->
        (Intent.lookup constants.intent intent).background
      | _ -> constants.extreme_primary_border
    in
    Css_gen.Color.(to_string_css fg, to_string_css bg, to_string_css border)
  in
  let title, content_attr =
    match title with
    | [] -> Vdom.Node.none, Vdom.Attr.many [ content_attr; Style.no_title ]
    | _ ->
      let create_title ~f ~extra_attr =
        f
          ~attr:
            (Vdom.Attr.many [ Style.title_bar; Style.title_text; title_attr; extra_attr ])
          title
      in
      let title =
        match title_kind with
        | Card_title_kind.Prominent ->
          create_title ~f:(fun ~attr x -> View.hbox ~attr x) ~extra_attr:Vdom.Attr.empty
        | Discreet ->
          create_title
            ~f:(fun ~attr x -> Vdom.Node.legend ~attr x)
            ~extra_attr:Style.card_legend
      in
      let content_attr = Vdom.Attr.many [ content_attr; Style.yes_title ] in
      title, content_attr
  in
  let contrasting_fg_intent_color =
    Option.value_map intent ~default:constants.primary.foreground ~f:(fun intent ->
      (Intent.lookup constants.intent intent).background)
    |> Css_gen.Color.to_string_css
  in
  let vars =
    Style.Variables.set
      ~title_fg
      ~title_bg
      ~bg
      ~fg
      ~title_border
      ~border
      ~contrasting_fg_intent_color
      ()
  in
  let create_card ~f ~extra_container_attr ~extra_items_attr =
    f
      ~attr:
        (Vdom.Attr.many
           [ container_attr
           ; Vdom.Attr.on_click (fun _ -> on_click)
           ; vars
           ; Style.container
           ; extra_container_attr
           ])
      [ title
      ; View.vbox ~attr:(Vdom.Attr.many [ content_attr; extra_items_attr ]) content
      ]
  in
  match title_kind with
  | Card_title_kind.Prominent ->
    create_card
      ~f:(fun ~attr x -> View.vbox ~attr x)
      ~extra_container_attr:Vdom.Attr.empty
      ~extra_items_attr:Style.content_prominent
  | Discreet ->
    create_card
      ~f:(fun ~attr x -> Vdom.Node.fieldset ~attr x)
      ~extra_container_attr:Style.fieldset_container
      ~extra_items_attr:Vdom.Attr.empty
;;
