open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Svg_example = struct
  let name = "PPX HTML used with virtual_dom_svg."
  let description = {|This lets you write svg Vdom.Node.t's with HTML syntax!|}
  let size = `Px 24

  let view =
    let viewbox =
      Virtual_dom_svg.Attr.viewbox ~min_x:0. ~min_y:0. ~width:24. ~height:24.
    in
    let vdom, demo =
      [%demo
        [%html.Virtual_dom_svg
          {|
            <svg
              %{[%tailwind "animate-spin"]}
              style="width: %{size#Css_gen.Length}; height: %{size#Css_gen.Length}"
              fill=%{`Name "none"}
              %{viewbox}
            >
              <circle
                %{[%tailwind "opacity-25"]}
                cx=%{12.0}
                cy=%{12.0}
                r=%{10.0}
                stroke=%{`Name "currentColor"}
                stroke_width=%{4.0}
              ></circle>
              <path
                %{[%tailwind "opacity-75"]}
                fill=%{`Name "currentColor"}
                %{Virtual_dom.Vdom.Attr.string_property
                "d"
                "M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 \
                 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647"
            }
              ></path>
            </svg>
          |}]]
    in
    Bonsai.const (vdom, demo)
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
       [ ( "PPX HTML Internal testing"
         , {|Internal testing example to test out ppx_html syntax we may not want to show to newcomers.|}
         , [ Gallery.make_demo (module Svg_example) ] )
       ])
;;

let () = Bonsai_web.Start.start component
