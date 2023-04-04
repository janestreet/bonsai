open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module Gallery = Bonsai_web_ui_gallery

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
    [%demo View.themed_text theme "hello world"]
  ;;

  let selector = None
  let filter_attrs = None
end

module Text_with_style_and_size = struct
  let name = "Text With Style And Size"

  let description =
    {| You can use themed text to easily render text in a range of font styles and sizes
    that are themable and consistent accross different components. |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo
      vbox
        [ View.themed_text theme ?style:None ?size:None "default style and size"
        ; View.themed_text theme ~style:Bold "bold text!"
        ; View.themed_text theme ~style:Italic "italic text ..."
        ; View.themed_text theme ~style:Underlined "this text is underlined"
        ; View.themed_text theme ~size:Large "This text is large"
        ; View.themed_text theme ~size:Small "This text is small"
        ]]
  ;;

  let selector = Some "span"
  let filter_attrs k _ = not (String.is_substring k ~substring:"padding")
  let filter_attrs = Some filter_attrs
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
        [ View.themed_text theme ?intent:None "no intent"
        ; View.themed_text theme ~intent:Info "informational"
        ; View.themed_text theme ~intent:Success "success!!!"
        ; View.themed_text theme ~intent:Warning "hmmmm"
        ; View.themed_text theme ~intent:Error "oh no."
        ]]
  ;;

  let selector = Some "span"
  let filter_attrs k _ = not (String.is_substring k ~substring:"padding")
  let filter_attrs = Some filter_attrs
end

module Table = struct
  let name = "Plain Table"

  let description =
    {|
    Tables are created by specifying the columns for the table alongside a list of data that
    will populate the table. Each column is defined with a name, a "getter" function that
    extracts the cell-specific data, and a renderer for that cell-specific data. |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    let module T =
      [%demo
        [@@@ocamlformat "disable"]

        type t = { sym : string ; bid : float ; ask : float } [@@deriving fields]

        [@@@ocamlformat "enable"]

        let columns =
          let render_text _ string = View.text string in
          let render_float _ = Vdom.Node.textf "%.3f" in
          [ View.Table.Col.make "sym" ~get:sym ~render:render_text
          ; View.Table.Col.make "bid" ~get:bid ~render:render_float
          ; View.Table.Col.make "ask" ~get:ask ~render:render_float
          ]
        ;;

        let data =
          [ { sym = "aapl"; bid = 1.0; ask = 2.3 }
          ; { sym = "msft"; bid = 8.2; ask = 9.8 }
          ; { sym = "tsla"; bid = 3.3; ask = 7.2 }
          ]
        ;;

        let my_table = View.Table.render theme columns data]
    in
    let code =
      (* Get rid of the lines that disable ocamlformat *)
      T.ppx_demo_string
      |> Core.String.split_lines
      |> List.filter ~f:(fun s ->
        not (Core.String.is_substring s ~substring:"ocamlformat"))
      |> String.concat ~sep:"\n"
      |> String.strip
      |> String.substr_replace_first ~pattern:"\n\n" ~with_:"\n"
    in
    T.my_table, code
  ;;

  let selector = None
  let filter_attrs = None
end

module Table_with_group = struct
  let name = "Table with group"

  let description =
    {| To make column groups, simply call the "group" function with a list of sub-columns. |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    let module T = struct
      type t =
        { sym : string
        ; bid : float
        ; ask : float
        }
      [@@deriving fields]

      include
        [%demo
          let columns =
            let render_text _ string = View.text string in
            let render_float _ = Vdom.Node.textf "%.3f" in
            [ View.Table.Col.make "sym" ~get:sym ~render:render_text
            ; View.Table.Col.group
                "prices"
                [ View.Table.Col.make "bid" ~get:bid ~render:render_float
                ; View.Table.Col.make "ask" ~get:ask ~render:render_float
                ]
            ]
          ;;]

      let data =
        [ { sym = "aapl"; bid = 1.0; ask = 2.3 }
        ; { sym = "msft"; bid = 8.2; ask = 9.8 }
        ; { sym = "tsla"; bid = 3.3; ask = 7.2 }
        ]
      ;;

      let my_table = View.Table.render theme columns data
    end
    in
    T.my_table, T.ppx_demo_string
  ;;

  let selector = None
  let filter_attrs = None
end

module Table_with_empty_cells = struct
  let name = "Table with empty cells"

  let description =
    {|
    Sometimes the cell contains some optional value.  If you want to handle that case yourself, you can
    use the "make" function like before. But you can also call "make_opt" and the table will get a line
    through it indicating that it's properly empty (as opposed to containing - for example - the empty string)|}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    let module T = struct
      type t =
        { sym : string
        ; trader : string option
        }
      [@@deriving fields]

      include
        [%demo
          let columns =
            let render_text _ string = View.text string in
            [ View.Table.Col.make "symbol" ~get:sym ~render:render_text
            ; View.Table.Col.make_opt "trader" ~get:trader ~render:render_text
            ]
          ;;

          let data =
            [ { sym = "aapl"; trader = None }
            ; { sym = "msft"; trader = None }
            ; { sym = "tsla"; trader = Some "emusk" }
            ]
          ;;]

      let my_table = View.Table.render theme columns data
    end
    in
    T.my_table, T.ppx_demo_string
  ;;

  let selector = None
  let filter_attrs = None
end

module Tooltip = struct
  let name = "Basic Tooltip"

  let description =
    {| The standard tooltip function allows annotating some text with a tooltip containing another string |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo
      View.tooltip
        theme
        ~tooltip:"saying 'hello world' every morning is a common programmer ritual"
        "hello world"]
  ;;

  let selector = None
  let filter_attrs = None
end

module Tooltip_directions = struct
  let name = "Tooltip directions"
  let description = {| Tooltips can be positioned relative to the annotated content |}

  let view =
    let%map.Computation theme = View.Theme.current in
    let vbox = View.vbox ~gap:(`Em 1) in
    let hbox = View.hbox ~gap:(`Em 1) in
    [%demo
      let tooltip = "tooltip content" in
      vbox
        [ hbox
            [ View.tooltip theme ~direction:Top ~tooltip "top"
            ; View.tooltip theme ~direction:Bottom ~tooltip "bottom"
            ]
        ; hbox
            [ View.tooltip theme ~direction:Left ~tooltip "left"
            ; View.tooltip theme ~direction:Right ~tooltip "right"
            ]
        ]]
  ;;

  let selector = None
  let filter_attrs = None
end

let rickroll =
  Effect.of_sync_fun
    (fun () ->
       let url = Js_of_ocaml.Js.string "https://www.youtube.com/watch?v=dQw4w9WgXcQ" in
       Js_of_ocaml.Dom_html.window##.location##assign url)
    ()
;;

module Tooltip_with_arbitrary_content = struct
  let name = "Tooltips with arbitrary content"

  let description =
    {| With `tooltip'` you can add a tooltip to abitrary content, and the tooltip itself can contain arbitrary content |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    [%demo
      let tooltip =
        View.vbox
          [ Vdom.Node.text "do not click this button"
          ; View.button theme ~intent:Error ~on_click:rickroll "no clicky!"
          ]
      in
      View.tooltip' theme ~tooltip ~direction:Right (Vdom.Node.text "cursed knowledge")]
  ;;

  let selector = None
  let filter_attrs = None
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
      ~attrs:
        [ Vdom.Attr.style Css_gen.(background_color c @> min_width w @> min_height h) ]
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
            [ Vdom.Node.label ~attrs:[ white_space_pre ] [ Vdom.Node.text name ]
            ; Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.width (`Px 200)) ] [ view ]
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
    [%css
      stylesheet
        {|
    .controls {
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
    and theme = View.Theme.current in
    let axis = Option.value axis ~default:Hbox in
    let (view, text), direction_v, f1 =
      match axis with
      | Hbox ->
        let direction = h_direction in
        let demo =
          [%demo
            View.hbox
              ~attrs:[ Vdom.Attr.(many [ id "target"; Style.target ]) ]
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
              ~attrs:[ Vdom.Attr.(many [ id "target"; Style.target ]) ]
              ?direction
              ?main_axis_alignment
              ?cross_axis_alignment
              [ red; green; blue ]]
        in
        demo, v_direction_v, v_f1
    in
    let controls_bg = (View.primary_colors theme).background in
    let controls_border = View.extreme_primary_border_color theme in
    let view =
      View.hbox
        ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"flex-grow" ~value:"1") ]
        ~gap:(`Em 1)
        ~main_axis_alignment:Space_between
        ~cross_axis_alignment:Start
        [ View.vbox
            ~attrs:
              [ Style.controls
              ; Vdom.Attr.style (Css_gen.background_color controls_bg)
              ; Vdom.Attr.style
                  (Css_gen.border ~width:(`Px 1) ~color:controls_border ~style:`Solid ())
              ]
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
    Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.max_width (`Px 500)) ] [ view ], text
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Devbar_intent = struct
  let name = "Devbar with Intent"
  let description = {| |}

  let view =
    let%map.Computation theme = View.Theme.current in
    let view, text =
      [%demo
        View.vbox
          ~gap:(`Em_float 0.5)
          [ View.devbar theme ?intent:None "DEV"
          ; View.devbar theme ~intent:Info "DEV"
          ; View.devbar theme ~intent:Success "DEV"
          ; View.devbar theme ~intent:Warning "DEV"
          ; View.devbar theme ~intent:Error "DEV"
          ]]
    in
    Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.max_width (`Px 500)) ] [ view ], text
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Basic_card = struct
  let name = "Basic Card"
  let description = {| |}

  let view =
    let%map.Computation theme = View.Theme.current in
    let view, text = [%demo View.card theme "The message is: You are great!"] in
    Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.max_width (`Px 500)) ] [ view ], text
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Card_with_title = struct
  let name = "Card with a title"
  let description = {| |}

  let view =
    let%map.Computation theme = View.Theme.current in
    let view, text =
      [%demo View.card theme ~title:"New message!!" "The message is: You are great!"]
    in
    Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.max_width (`Px 500)) ] [ view ], text
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Cards_with_intent = struct
  let name = "Cards with intent"
  let description = {| |}

  let view =
    let%map.Computation theme = View.Theme.current in
    let view, text =
      [%demo
        View.vbox
          ~gap:(`Em_float 0.5)
          [ View.card theme ?intent:None ~title:"no intent" "[message content here]"
          ; View.card theme ~intent:Info ~title:"information" "FYI: some stuff"
          ; View.card theme ~intent:Success ~title:"success!!!" "a good thing happened"
          ; View.card theme ~intent:Warning ~title:"hmmm" "please be alerted"
          ; View.card theme ~intent:Error ~title:"UH OH" "something bad happened!"
          ]]
    in
    Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.max_width (`Px 500)) ] [ view ], text
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Cards_with_fieldset = struct
  let name = "Cards with fieldset styling"
  let description = {| |}

  let view =
    let%map.Computation theme = View.Theme.current in
    let view, text =
      [%demo
        View.vbox
          ~gap:(`Em_float 0.5)
          [ View.card
              theme
              ?intent:None
              ~title:"no intent"
              ~title_kind:Discreet
              "[message content here]"
          ; View.card
              theme
              ~intent:Info
              ~title:"information"
              ~title_kind:Discreet
              "FYI: some stuff"
          ; View.card
              theme
              ~intent:Success
              ~title:"success!!!"
              ~title_kind:Discreet
              "a good thing happened"
          ; View.card
              theme
              ~intent:Warning
              ~title:"hmmm"
              ~title_kind:Discreet
              "please be alerted"
          ; View.card
              theme
              ~intent:Error
              ~title:"UH OH"
              ~title_kind:Discreet
              "something bad happened!"
          ]]
    in
    Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.max_width (`Px 500)) ] [ view ], text
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Card_with_rows = struct
  let name = "Card with rows"

  let description =
    {| View.card' allows for the content to be a list of arbitrary vdom nodes which are separated vertically from one another. |}
  ;;

  let view =
    let%map.Computation theme = View.Theme.current in
    let view, text =
      [%demo
        hbox
          [ View.card'
              theme
              ~intent:Info
              ~title:[ View.text "New message!!" ]
              [ View.text "debug message 1 "; View.text "more debug message" ]
          ; View.card'
              theme
              ~intent:Info
              ~title_kind:Discreet
              ~title:[ View.text "New message!!" ]
              [ View.text "debug message 1 "; View.text "more debug message" ]
          ]]
    in
    Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.max_width (`Px 500)) ] [ view ], text
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Text"
         , {| An amalgom of Vdom.Node.text and Vdom.Node.span, View.text is used to put
           (optionally formatted) text on the screen. |}
         , [ Gallery.make_demo (module Text)
           ; Gallery.make_demo (module Text_with_style_and_size)
           ; Gallery.make_demo (module Text_with_intent)
           ] )
       ; ( "Tooltip"
         , {|Tooltips are a great way to make extra information available to a user when they hover over an element.|}
         , [ Gallery.make_demo (module Tooltip)
           ; Gallery.make_demo (module Tooltip_directions)
           ; Gallery.make_demo (module Tooltip_with_arbitrary_content)
           ] )
       ; ( "Buttons"
         , {| |}
         , [ Gallery.make_demo (module Button)
           ; Gallery.make_demo (module Disabled_button)
           ; Gallery.make_demo (module Buttons_with_intent)
           ; Gallery.make_demo (module Disabled_buttons_with_intent)
           ] )
       ; ( "Tables"
         , {| |}
         , [ Gallery.make_demo (module Table)
           ; Gallery.make_demo (module Table_with_group)
           ; Gallery.make_demo (module Table_with_empty_cells)
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
           ; Gallery.make_demo (module Enumerable_tabs)
           ] )
       ; ( "Devbar"
         , {| Used to convey that an application is in some kind of dev or test mode, the devbar
          is intentionally large and bright and attention-grabbing |}
         , [ Gallery.make_demo (module Devbar); Gallery.make_demo (module Devbar_intent) ]
         )
       ; ( "Card"
         , {| Cards are a great way of organizing the structure of your app or just
         grabbing your users attention and letting them know things are happening, not
         happening, happened, failed, kinda failed, kinda worked, and more!
              |}
         , [ Gallery.make_demo (module Basic_card)
           ; Gallery.make_demo (module Card_with_title)
           ; Gallery.make_demo (module Cards_with_intent)
           ; Gallery.make_demo (module Cards_with_fieldset)
           ; Gallery.make_demo (module Card_with_rows)
           ] )
       ])
;;

let () =
  Async_js.init ();
  Auto_reload.refresh_on_build ();
  Bonsai_web.Start.start component
;;
