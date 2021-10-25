open! Core
open! Bonsai_web
open Vdom
open Tailwind_colors

let gray =
  [ gray50
  ; gray100
  ; gray200
  ; gray300
  ; gray400
  ; gray500
  ; gray600
  ; gray700
  ; gray800
  ; gray900
  ]
;;

let red =
  [ red50; red100; red200; red300; red400; red500; red600; red700; red800; red900 ]
;;

let yellow =
  [ yellow50
  ; yellow100
  ; yellow200
  ; yellow300
  ; yellow400
  ; yellow500
  ; yellow600
  ; yellow700
  ; yellow800
  ; yellow900
  ]
;;

let green =
  [ green50
  ; green100
  ; green200
  ; green300
  ; green400
  ; green500
  ; green600
  ; green700
  ; green800
  ; green900
  ]
;;

let blue =
  [ blue50
  ; blue100
  ; blue200
  ; blue300
  ; blue400
  ; blue500
  ; blue600
  ; blue700
  ; blue800
  ; blue900
  ]
;;

let indigo =
  [ indigo50
  ; indigo100
  ; indigo200
  ; indigo300
  ; indigo400
  ; indigo500
  ; indigo600
  ; indigo700
  ; indigo800
  ; indigo900
  ]
;;

let purple =
  [ purple50
  ; purple100
  ; purple200
  ; purple300
  ; purple400
  ; purple500
  ; purple600
  ; purple700
  ; purple800
  ; purple900
  ]
;;

let pink =
  [ pink50
  ; pink100
  ; pink200
  ; pink300
  ; pink400
  ; pink500
  ; pink600
  ; pink700
  ; pink800
  ; pink900
  ]
;;

let all_palettes =
  [ "gray", gray
  ; "red", red
  ; "yellow", yellow
  ; "green", green
  ; "blue", blue
  ; "indigo", indigo
  ; "purple", purple
  ; "pink", pink
  ]
;;

let name_suffixes =
  [ "50"; "100"; "200"; "300"; "400"; "500"; "600"; "700"; "800"; "900" ]
;;

let abs_position_px t b l r =
  Css_gen.position ~top:(`Px t) ~bottom:(`Px b) ~left:(`Px l) ~right:(`Px r) `Absolute
;;

let component =
  Bonsai.const
    (Node.div
       (List.map all_palettes ~f:(fun (palette_name, palette) ->
          Node.div
            ~attr:(Attr.style (Css_gen.flex_container ~direction:`Row ()))
            (List.map
               (List.zip_exn name_suffixes palette)
               ~f:(fun (name_suffix, palette_color) ->
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
                           [ Node.textf "%s%s" palette_name name_suffix ]
                       ]
                   ])))))
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
