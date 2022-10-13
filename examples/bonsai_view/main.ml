open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

let vbox c = View.vbox ~cross_axis_alignment:Start ~gap:(`Px 5) c
let hbox c = View.hbox ~cross_axis_alignment:Start ~gap:(`Px 5) c

module Text = struct
  let name = "Plain Text"

  let description =
    {| Strings are rendered to the page with View.text.
    When invoked without any optional parameters, it will produce a dom Text node. |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo View.text theme "hello world"]
  ;;

  let selector = None
  let filter_attrs = None
end

module Text_with_intent = struct
  let name = "Text With Intent"

  let description =
    {| When text conveys special meaning, like an error or a warning, it can be useful
       to give the theme the ability to style it differently. |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo
      vbox
        [ View.text theme ?intent:None "no intent"
        ; View.text theme ~intent:Info "informational"
        ; View.text theme ~intent:Success "success!!!"
        ; View.text theme ~intent:Warning "hmmmm"
        ; View.text theme ~intent:Error "oh no."
        ]]
  ;;

  let selector = Some "span"
  let filter_attrs k _ = not (String.is_substring k ~substring:"padding")
  let filter_attrs = Some filter_attrs
end

module Button = struct
  let name = "Regular Button"

  let description =
    {| The button function requires an on_click handler, and takes the button label
       as input in the form of a string |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo View.button theme ~on_click:Effect.Ignore "click me"]
  ;;

  let selector = None
  let filter_attrs = None
end

module Disabled_button = struct
  let name = "Disabled button"
  let description = {| Buttons can be disabled with an optional parameter |}

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo
      hbox
        [ View.button theme ~disabled:false ~on_click:Effect.Ignore "not disabled"
        ; View.button theme ~disabled:true ~on_click:Effect.Ignore "disabled"
        ]]
  ;;

  let selector = Some "button"
  let filter_attrs = None
end

module Buttons_with_intent = struct
  let name = "Buttons with Intent"
  let description = {| When given an intent, buttons can change their style |}

  let view =
    let%map.Computation theme = View.Theme.current in
    let on_click = Effect.Ignore in
    [%demo
      hbox
        [ View.button theme ?intent:None ~on_click "no intent"
        ; View.button theme ~intent:Info ~on_click "informational"
        ; View.button theme ~intent:Success ~on_click "success!!!"
        ; View.button theme ~intent:Warning ~on_click "hmmmm"
        ; View.button theme ~intent:Error ~on_click "oh no."
        ]]
  ;;

  let selector = Some "button"
  let filter_attrs = None
end

module Disabled_buttons_with_intent = struct
  let name = "Disabled buttons with Intent"

  let description =
    {| When buttons with Intent are disabled, they are no longer clickable
      and their visuals change. |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo
      let on_click = Effect.Ignore
      and disabled = true in
      hbox
        [ View.button theme ~disabled ~on_click ?intent:None "no intent"
        ; View.button theme ~disabled ~on_click ~intent:Info "informational"
        ; View.button theme ~disabled ~on_click ~intent:Success "success!!!"
        ; View.button theme ~disabled ~on_click ~intent:Warning "hmmmm"
        ; View.button theme ~disabled ~on_click ~intent:Error "oh. oh no."
        ]]
  ;;

  let selector = Some "button"
  let filter_attrs = None
end

module Basic_vbox = struct
  let name = "Basic vbox"
  let description = {| The vbox function builds a vertically stacking container.  |}

  let make_box c w h =
    Vdom.Node.div
      ~attr:(Vdom.Attr.style Css_gen.(background_color c @> min_width w @> min_height h))
      []
  ;;

  let red = make_box (`Name "red") (`Px 20) (`Px 30)
  let green = make_box (`Name "green") (`Px 30) (`Px 50)
  let blue = make_box (`Name "blue") (`Px 50) (`Px 30)
  let view = Bonsai.const [%demo View.vbox [ red; green; blue ]]
  let selector = None

  let filter_attrs =
    Some
      (fun k _ ->
         (not (String.is_substring k ~substring:"width"))
         && not (String.is_substring k ~substring:"height"))
  ;;
end

module Basic_hbox = struct
  let name = "Basic hbox"
  let description = {| The vbox function builds a horizontally stacking container.  |}
  let red, green, blue, filter_attrs = Basic_vbox.(red, green, blue, filter_attrs)
  let view = Bonsai.const [%demo View.hbox [ red; green; blue ]]
  let selector = None
end

module Interactive_vbox = struct
  open Basic_vbox
  open View.Flex

  let name = "Vbox and Hbox with Arguments"

  let description =
    {| [vbox] and [hbox] take a number of optional parameters which you can use to control
       the direction of the stacking as well as how the items are distributed in any space
       that is left over in the container. |}
  ;;

  let white_space_pre = Vdom.Attr.style Css_gen.(white_space `Pre)

  let dropdown (type a) ?init name (module M : Bonsai.Enum with type t = a) =
    Bonsai.Computation.map
      (Form.Elements.Dropdown.enumerable_opt ?init (module M))
      ~f:(fun form ->
        let value = Or_error.ok_exn (Form.value form) in
        let view = Form.View.to_vdom_plain (Form.view form) |> List.hd_exn in
        let view =
          View.hbox
            ~main_axis_alignment:End
            ~gap:(`Px 10)
            [ Vdom.Node.label ~attr:white_space_pre [ Vdom.Node.text name ]
            ; Vdom.Node.div ~attr:(Vdom.Attr.style (Css_gen.width (`Px 200))) [ view ]
            ]
        in
        let pattern = String.Search_pattern.create ("?" ^ name) in
        let new_text =
          match value with
          | None -> sprintf "?%s:None" name
          | Some value -> sprintf "~%s:%s" name (M.sexp_of_t value |> Sexp.to_string)
        in
        let modify_program s =
          String.Search_pattern.replace_first pattern ~in_:s ~with_:new_text
        in
        view, value, modify_program)
  ;;

  module Style =
    [%css.raw
      {|
    .controls {
      background: white;
      border-radius: 3px;
      padding: 1em;
      border: 1px solid darkgrey;
    }

    .target {
      border: 1px dashed black;
      padding: 5px;
      width: 150px;
      height: 150px;
    }
  |}]

  module Axis = struct
    type t =
      | Hbox
      | Vbox
    [@@deriving sexp, enumerate, equal, compare]
  end

  let view =
    let%map.Computation axis_v, axis, _ =
      dropdown "function" ~init:`First_item (module Axis)
    and v_direction_v, v_direction, v_f1 = dropdown "direction" (module Vertical_dir)
    and h_direction_v, h_direction, h_f1 = dropdown "direction" (module Horizontal_dir)
    and main_axis_alignment_v, main_axis_alignment, f2 =
      dropdown "main_axis_alignment" (module Main_axis_alignment)
    and cross_axis_alignment_v, cross_axis_alignment, f3 =
      dropdown "cross_axis_alignment" (module Cross_axis_alignment)
    in
    let axis = Option.value axis ~default:Hbox in
    let (view, text), direction_v, f1 =
      match axis with
      | Hbox ->
        let direction = h_direction in
        let demo =
          [%demo
            View.hbox
              ~attr:Vdom.Attr.(many [ id "target"; class_ Style.target ])
              ?direction
              ?main_axis_alignment
              ?cross_axis_alignment
              [ red; green; blue ]]
        in
        demo, h_direction_v, h_f1
      | Vbox ->
        let direction = v_direction in
        let demo =
          [%demo
            View.vbox
              ~attr:Vdom.Attr.(many [ id "target"; class_ Style.target ])
              ?direction
              ?main_axis_alignment
              ?cross_axis_alignment
              [ red; green; blue ]]
        in
        demo, v_direction_v, v_f1
    in
    let view =
      View.hbox
        ~attr:(Vdom.Attr.style (Css_gen.create ~field:"flex-grow" ~value:"1"))
        ~gap:(`Em 1)
        ~main_axis_alignment:Space_between
        ~cross_axis_alignment:Start
        [ View.vbox
            ~attr:(Vdom.Attr.class_ Style.controls)
            ~gap:(`Px 3)
            [ axis_v; direction_v; main_axis_alignment_v; cross_axis_alignment_v ]
        ; view
        ]
    in
    let text =
      String.split_lines text
      |> List.filter ~f:(Fn.non (String.is_substring ~substring:"target"))
      |> String.concat_lines
      |> fun init -> List.fold [ f1; f2; f3 ] ~init ~f:(fun s f -> f s)
    in
    view, text
  ;;

  let selector = Some "#target"

  let filter_attrs =
    Some
      (fun k v ->
         (not (String.equal k "id"))
         && (not (String.is_substring k ~substring:"padding"))
         && (not (String.is_substring k ~substring:"border"))
         && (not (String.is_substring v ~substring:"target"))
         && (not (String.is_substring k ~substring:"width"))
         && not (String.is_substring k ~substring:"height"))
  ;;
end

module Basic_tabs = struct
  let name = "Basic Tabs"
  let description = ""

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo
      let on_change ~from:_ ~to_:_ = Effect.Ignore in
      View.tabs
        theme
        ~equal:[%equal: int]
        ~active:0
        ~on_change
        [ 0, Vdom.Node.text "home"
        ; 1, Vdom.Node.text "about"
        ; 2, Vdom.Node.text "user preferences"
        ]]
  ;;

  let selector = None
  let filter_attrs = None
end

module Advanced_tabs = struct
  let name = "Customized Tabs"

  let description =
    {| Thought the theme should be doing most of the visual styling, you can also customize it. |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    let grey_background = Vdom.Attr.style (Css_gen.background_color (`Name "grey")) in
    let red_border =
      Vdom.Attr.style
        (Css_gen.border_bottom ~width:(`Px 3) ~color:(`Name "red") ~style:`Solid ())
    in
    [%demo
      let on_change ~from:_ ~to_:_ = Effect.Ignore in
      View.tabs
        theme
        ~attr:grey_background
        ~per_tab_attr:(fun _i ~is_active ->
          if is_active then red_border else Vdom.Attr.empty)
        ~on_change
        ~equal:[%equal: int]
        ~active:0
        [ 0, Vdom.Node.text "home"
        ; 1, Vdom.Node.text "about"
        ; 2, Vdom.Node.text "user preferences"
        ]]
  ;;

  let selector = None
  let filter_attrs = None
end

module Enumerable_tabs = struct
  let name = "Enumerable Tabs"

  let description =
    {| If you have an enumerable type, tabs_enum can be used to generate tabs for it |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    let module T =
      [%demo
        module Pages = struct
          type t =
            | Home
            | About
            | User_preferences
          [@@deriving enumerate, equal, sexp_of]
        end

        let tabs =
          let on_change ~from:_ ~to_:_ = Effect.Ignore in
          View.tabs_enum theme (module Pages) ~active:Home ~on_change
        ;;]
    in
    T.tabs, T.ppx_demo_string
  ;;

  let selector = None
  let filter_attrs = None
end

module Devbar = struct
  let name = "Basic Devbar"
  let description = {| |}

  let view =
    let%map.Computation theme = View.Theme.current in
    let view, text = [%demo View.devbar theme "DEV"] in
    Vdom.Node.div ~attr:(Vdom.Attr.style (Css_gen.max_width (`Px 500))) [ view ], text
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component =
  Gallery.make_sections
    [ ( "Text"
      , {| An amalgom of Vdom.Node.text and Vdom.Node.span, View.text is used to put
           (optionally formatted) text on the screen. |}
      , [ Gallery.make_demo (module Text); Gallery.make_demo (module Text_with_intent) ] )
    ; ( "Buttons"
      , {| |}
      , [ Gallery.make_demo (module Button)
        ; Gallery.make_demo (module Disabled_button)
        ; Gallery.make_demo (module Buttons_with_intent)
        ; Gallery.make_demo (module Disabled_buttons_with_intent)
        ] )
    ; ( "Flex Boxes"
      , {| The vbox and hbox functions use CSS's flexbox layout algorithm to build vertically
           and horizontally stacking containers.  Because they are purely for layout, themes
           do no have the ability to change their behavior. |}
      , [ Gallery.make_demo (module Basic_hbox)
        ; Gallery.make_demo (module Basic_vbox)
        ; Gallery.make_demo (module Interactive_vbox)
        ] )
    ; ( "Tabs"
      , {| Used for navigating between pages within an application, the tabs component allows
           for zero or one active tabs. |}
      , [ Gallery.make_demo (module Basic_tabs)
        ; Gallery.make_demo (module Advanced_tabs)
        ; Gallery.make_demo (module Enumerable_tabs)
        ] )
    ; ( "Devbar"
      , {| Used to convey that an application is in some kind of dev or test mode, the devbar
          is intentionally large and bright and attention-grabbing |}
      , [ Gallery.make_demo (module Devbar) ] )
    ]
;;

let (_ : _ Start.Handle.t) =
  Async_js.init ();
  Auto_reload.refresh_on_build ();
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
