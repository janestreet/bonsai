open! Core
open! Bonsai_web
open Vdom

let brightness_suffix : Tailwind_colors.Brightness.t -> string = function
  | `_50 -> "50"
  | `_100 -> "100"
  | `_200 -> "200"
  | `_300 -> "300"
  | `_400 -> "400"
  | `_500 -> "500"
  | `_600 -> "600"
  | `_700 -> "700"
  | `_800 -> "800"
  | `_900 -> "900"
;;

let abs_position_px t b l r =
  Css_gen.position ~top:(`Px t) ~bottom:(`Px b) ~left:(`Px l) ~right:(`Px r) `Absolute
;;

let component =
  Bonsai.const
    (Node.div
       (List.map Tailwind_colors.Hue.all ~f:(fun hue ->
          Node.div
            ~attr:(Attr.style (Css_gen.flex_container ~direction:`Row ()))
            (List.map Tailwind_colors.Brightness.all ~f:(fun brightness ->
               let palette_color = Tailwind_colors.create hue brightness in
               Node.div
                 ~attr:
                   (Attr.style
                      Css_gen.(
                        position `Relative @> width (`Px 100) @> height (`Px 100)))
                 [ Node.div
                     ~attr:
                       (Attr.style
                          Css_gen.(
                            background_color palette_color @> abs_position_px 0 0 0 0))
                     []
                 ; Node.div
                     ~attr:
                       (Attr.many
                          [ Attr.style
                              Css_gen.(
                                font ~size:(`Px 15) ~family:[ "monospace" ] ()
                                @> color palette_color
                                @> text_align `Center
                                @> abs_position_px 0 0 0 0)
                          ; Attr.class_ "invert-on-hover"
                          ])
                     [ Node.div
                         ~attr:(Attr.style (abs_position_px 40 60 0 0))
                         [ Node.textf
                             "%s%s"
                             (Tailwind_colors.Hue.to_string hue)
                             (brightness_suffix brightness)
                         ]
                     ]
                 ])))))
;;

let () = Bonsai_web.Start.start component
