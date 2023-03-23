open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

type t =
  { view : Vdom.Node.t
  ; is_open : bool
  ; open_ : unit Effect.t
  ; close : unit Effect.t
  ; toggle : unit Effect.t
  }

let component
      ?(extra_container_attr = Value.return Vdom.Attr.empty)
      ?(extra_title_attr = Value.return Vdom.Attr.empty)
      ?(extra_content_attr = Value.return Vdom.Attr.empty)
      ~starts_open
      ~title
      ~content
      ()
  =
  let%sub { state = is_open; set_state = set_is_open; toggle } =
    Bonsai.toggle' ~default_model:starts_open
  in
  let%sub open_, close =
    let%arr set_is_open = set_is_open in
    set_is_open true, set_is_open false
  in
  let%sub view =
    let%sub theme = View.Theme.current in
    let%sub title_attr =
      let%arr extra_title_attr = extra_title_attr
      and toggle = toggle
      and theme = theme
      and is_open = is_open in
      let constants = View.constants theme in
      Vdom.Attr.many
        [ Style.title
        ; Vdom.Attr.on_click (fun _ -> toggle)
        ; (if is_open then Style.title_open else Style.title_closed)
        ; Style.Variables.set
            ~border:(Css_gen.Color.to_string_css constants.extreme_primary_border)
            ~fg_text:(Css_gen.Color.to_string_css constants.extreme.foreground)
            ()
        ; extra_title_attr
        ]
    in
    let%sub title =
      let%arr is_open = is_open
      and title = title in
      let is_open_attr =
        if is_open then Style.accordion_open else Style.accordion_closed
      in
      [ View.hbox
          ~main_axis_alignment:Start
          ~cross_axis_alignment:Center
          [ Vdom.Node.div
              ~attrs:[ Style.icon_container ]
              [ Vdom.Node.div ~attrs:[ is_open_attr; Style.icon ] [] ]
          ; title
          ]
      ]
    in
    match%sub is_open with
    | false ->
      let%arr theme = theme
      and title = title
      and title_attr = title_attr
      and extra_content_attr = extra_content_attr
      and extra_container_attr = extra_container_attr in
      View.card'
        theme
        ~content_attr:(Vdom.Attr.many [ Style.no_padding; extra_content_attr ])
        ~container_attr:extra_container_attr
        ~title_attr
        ~title
        [ Vdom.Node.none ]
    | true ->
      let%sub content = content in
      let%arr theme = theme
      and content = content
      and title = title
      and title_attr = title_attr
      and extra_container_attr = extra_container_attr
      and extra_content_attr = extra_content_attr in
      View.card'
        theme
        ~container_attr:extra_container_attr
        ~content_attr:extra_content_attr
        ~title_attr
        ~title
        [ content ]
  in
  let%arr view = view
  and is_open = is_open
  and open_ = open_
  and close = close
  and toggle = toggle in
  { view; is_open; open_; close; toggle }
;;
