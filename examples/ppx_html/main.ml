open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Basic = struct
  let name = "PPX HTML"
  let description = {|This lets you write Vdom.Node.t's with HTML syntax!|}

  let image_url =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e1/Cattle_tyrant_%28Machetornis_rixosa%29_on_Capybara.jpg/1920px-Cattle_tyrant_%28Machetornis_rixosa%29_on_Capybara.jpg"
  ;;

  let centered =
    [%css
      {|display: flex; justify-content: center; align-items:center; flex-direction: column|}]
  ;;

  let view =
    let vdom, demo =
      [%demo
        [%html
          {|
            <div %{centered}>
              <p>Capybaras are the worlds largest living rodent.</p>
              <br />
              <img style="width: 50%" src=%{image_url} />
            </div>
          |}]]
    in
    fun _graph -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module With_tailwind = struct
  let name = "PPX HTML + Tailwind"
  let description = {|This let's you use tailwind in combination with ppx_html!|}

  let view =
    let vdom, demo =
      [%demo [%html {| <div tailwind="w-16 h-16 bg-amber-400"></div> |}]]
    in
    fun _graph -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Svg_example = struct
  let name = "PPX HTML with Virtual_dom_svg"
  let description = {|This lets you write svg Vdom.Node.t's with HTML syntax.|}

  let view =
    let vdom, demo =
      [%demo
        [%html.Virtual_dom_svg
          {|
            <svg width=%{100.0} height=%{100.0}>
              <circle
                cx=%{50.0}
                cy=%{50.0}
                r=%{40.0}
                stroke=%{`Name "rebeccapurple"}
                stroke_width=%{4.0}
                fill=%{`Name "tomato"}
              ></circle>
            </svg>
          |}]]
    in
    fun _graph -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component graph =
  let%sub theme, theme_picker = Gallery.Theme_picker.component ~default:Kado () graph in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "PPX HTML"
         , {|[ppx_html] allows you to write your virtual dom nodes with HTML syntax, spiritually similar to JSX.|}
         , [ Gallery.make_demo (module Basic)
           ; Gallery.make_demo (module Svg_example)
           ; Gallery.make_demo (module With_tailwind)
           ] )
       ])
    graph
;;

let () = Bonsai_web.Start.start component
