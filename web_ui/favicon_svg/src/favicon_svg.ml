open Core

(* Implementation is generally inspired by [0], but a bunch of other tricks are used.

   [0] https://css-tricks.com/svg-favicons-and-all-the-fun-things-we-can-do-with-them/ *)

type t = string

let percent_to_string p =
  !Css_gen.Private.float_to_string_with_fixed 2 (Percent.to_percentage p)
;;

let of_unicode
  ~font_size
  ?(background_color = `Name "transparent")
  ?(font_color = `Name "black")
  ?(pos_x = Percent.of_mult 0.5)
  ?(pos_y = Percent.of_mult 0.5)
  text
  =
  let font_size = percent_to_string font_size in
  let pos_x = percent_to_string pos_x in
  let pos_y = percent_to_string pos_y in
  let text_style =
    let background_color = Css_gen.Color.to_string_css background_color in
    let color = Css_gen.Color.to_string_css font_color in
    [%string
      {|style="background-color: %{background_color}; color: %{color}; fill: currentColor"|}]
  in
  let svg_style =
    let background_color = Css_gen.Color.to_string_css background_color in
    [%string {|style="background-color: %{background_color}"|}]
  in
  [%string
    {|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" %{svg_style}>
  <text x="%{pos_x}" y="%{pos_y}"
     %{text_style}
     dominant-baseline="central"
     text-anchor="middle"
     font-size="%{font_size}"
     font-family="'Open Sans', 'Noto Color Emoji', sans-serif">
       %{text}
  </text>
</svg>|}]
;;

let to_svg_file_content t = t

let to_embedded_url t =
  let t = Base64.encode_exn t in
  Uri.of_string ("data:image/svg+xml;base64," ^ t)
;;
