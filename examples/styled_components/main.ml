open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery
module Form = Bonsai_web_ui_form.With_automatic_view

module Basic = struct
  let name = "Basic"

  let description =
    {| This lets you write css within an expression! Similar to styled components.|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        Vdom.Node.div
          ~attrs:
            [ {%css|
               background-color: tomato;
               width: 2rem;
               height: 2rem; |}
            ]
          []]
    in
    Bonsai.const (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Parameters = struct
  type t =
    { color : [ `Hex of string ]
    ; width : int
    ; height : int
    }
  [@@deriving typed_fields]

  open Bonsai.Let_syntax

  let tomato_color = `Hex "#ff6347"

  let default_int_field =
    let%sub form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    Form.Dynamic.with_default (Value.return 2) form
  ;;

  let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
    | Color ->
      let%sub form = Form.Elements.Color_picker.hex () in
      Form.Dynamic.with_default (Value.return tomato_color) form
    | Width -> default_int_field
    | Height -> default_int_field
  ;;

  let label_for_field = `Inferred
end

module Interpolation = struct
  let name = "Interpolation"

  let description =
    {|You can interpolate variables using the same syntax as [ppx_string].|}
  ;;

  let view =
    let f ~color ~width ~height =
      [%demo
        Vdom.Node.div
          ~attrs:
            [ {%css|
               background-color: %{color};
               width: %{width};
               height: %{height}; |}
            ]
          []]
    in
    let%sub form = Form.Typed.Record.make (module Parameters) in
    let%sub data =
      let%arr form = form in
      Form.value_or_default
        form
        ~default:{ Parameters.color = Parameters.tomato_color; width = 2; height = 2 }
    in
    let%arr form = form
    and { color; width; height } = data in
    let color = Css_gen.Color.to_string_css color in
    let width = [%string "%{width#Int}rem"] in
    let height = [%string "%{height#Int}rem"] in
    let vdom, demo = f ~color ~width ~height in
    ( View.vbox
        ~cross_axis_alignment:Center
        ~gap:(`Rem 1.0)
        [ Form.view_as_vdom form; vdom ]
    , demo )
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Typed_interpolation = struct
  let name = "Typed interpolation"

  let description =
    {|Just like [ppx_string], you can specify the type of each
  interpolated variable. It will call that module's [to_string_css]. |}
  ;;

  let view =
    let f ~color ~width ~height =
      [%demo
        Vdom.Node.div
          ~attrs:
            [ {%css|
               background-color: %{color#Css_gen.Color};
               width: %{width#Css_gen.Length};
               height: %{height#Css_gen.Length}; |}
            ]
          []]
    in
    let%sub form = Form.Typed.Record.make (module Parameters) in
    let%sub data =
      let%arr form = form in
      Form.value_or_default
        form
        ~default:{ Parameters.color = Parameters.tomato_color; width = 2; height = 2 }
    in
    let%arr form = form
    and { color; width; height } = data in
    let color : Css_gen.Color.t =
      let (`Hex c) = color in
      `Hex c
    in
    let width = `Rem (Int.to_float width) in
    let height = `Rem (Int.to_float height) in
    let vdom, demo = f ~color ~width ~height in
    ( View.vbox
        ~cross_axis_alignment:Center
        ~gap:(`Rem 1.0)
        [ Form.view_as_vdom form; vdom ]
    , demo )
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Nested_css = struct
  let name = {|Nested CSS|}
  let description = {|You can use css's relatively new nesting feature with this too.|}

  let view =
    Bonsai.const
      [%demo
        Vdom.Node.div
          ~attrs:
            [ {%css|
               background-color: tomato;
               height: 2rem;
               width: 2rem;

               &:hover {
                 background-color: white;
               }
             |}
            ]
          []]
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Stylesheet_interpolation = struct
  let name = "Stylesheet interpolation"

  let description =
    {| The [ppx_string] syntax also works on [%css stylesheet], letting you target pseudoselectors+more. |}
  ;;

  let view =
    let f ~color ~width ~height =
      [%demo
        let module Style =
        [%css
        stylesheet
          {|
              .square {
                background-color: %{color#Css_gen.Color};
                width: %{width#Css_gen.Length};
                height: %{height#Css_gen.Length};
                transition: 1s;
              }

              .square:hover {
                transform: rotate(180deg);
              }
            |}]
        in
        Vdom.Node.div ~attrs:[ Style.square ] []]
    in
    let%sub form = Form.Typed.Record.make (module Parameters) in
    let%sub data =
      let%arr form = form in
      Form.value_or_default
        form
        ~default:{ Parameters.color = Parameters.tomato_color; width = 2; height = 2 }
    in
    let%arr form = form
    and { color; width; height } = data in
    let color : Css_gen.Color.t =
      let (`Hex c) = color in
      `Hex c
    in
    let width = `Rem (Int.to_float width) in
    let height = `Rem (Int.to_float height) in
    let vdom, demo = f ~color ~width ~height in
    ( View.vbox
        ~cross_axis_alignment:Center
        ~gap:(`Rem 1.0)
        [ Form.view_as_vdom form; vdom ]
    , demo )
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component =
  let%sub theme, theme_picker = Gallery.Theme_picker.component ~default:Kado () in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Styled components"
         , {|[ppx_css] allows you to write your css as an attribute directly. It has a
         syntax similar to "styled components".|}
         , [ Gallery.make_demo (module Basic)
           ; Gallery.make_demo (module Interpolation)
           ; Gallery.make_demo (module Typed_interpolation)
           ; Gallery.make_demo (module Nested_css)
           ; Gallery.make_demo (module Stylesheet_interpolation)
           ] )
       ])
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component
;;
