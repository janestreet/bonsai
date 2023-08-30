open! Core
open! Bonsai_web
open Css_gen

let full_width = width Length.percent100

let typeahead =
  concat
    [ font_family [ "var(--js-main-font)"; "sans-serif" ]
    ; border ~width:(`Px 2) ~color:(`Hex "#e0e0e0") ~style:`Solid ()
    ; color (`Var "--js-text-color")
    ; height (`Px 38)
    ; outline ~style:`None ()
    ; padding ~right:(`Px 40) ~left:(`Px 3) ()
    ; create
        ~field:"transition"
        ~value:"border-color 150ms cubic-bezier(0.4, 0, 0.2, 1) 0ms"
    ]
;;

let pill_container =
  concat [ margin ~top:(`Rem 0.5) ~right:(`Rem 0.) ~bottom:(`Rem 0.5) ~left:(`Rem 0.) () ]
;;

let pill =
  concat
    [ padding ~top:(`Rem 0.) ~right:(`Rem 0.6) ~bottom:(`Rem 0.) ~left:(`Rem 0.6) ()
    ; create ~field:"cursor" ~value:"pointer"
    ; border_radius (`Rem 10.)
    ; background_color (`Var "--js-default-button-active-color")
    ; margin ~right:(`Px 10) ~bottom:(`Px 10) ()
    ; display `Inline_block
    ]
;;
