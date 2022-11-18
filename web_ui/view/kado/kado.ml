open! Core
open! Import

module Style = struct
  type t = Dark
end

module Contrast = struct
  type t = Standard
end

module Version = struct
  type t =
    | V1
    | Bleeding
end

let dark_mode_constants =
  let c ~fg ~bg = { Fg_bg.foreground = fg; background = bg } in
  let _placeholder = c ~fg:(`Name "blue") ~bg:(`Name "red") in
  let primary = c ~fg:(`Hex "#d1d2d3") ~bg:(`Hex "#1a1d21") in
  let extreme = c ~fg:(`Name "#e2e3e4") ~bg:(`Hex "#14181c") in
  let table_even = c ~bg:(`Hex "#21242a") ~fg:primary.foreground in
  let extreme_primary_border = `Hex "#313943" in
  let header = c ~bg:(`Hex "#0b0e11") ~fg:primary.foreground in
  { primary
  ; extreme
  ; extreme_primary_border
  ; intent =
      { info = c ~fg:(`Name "black") ~bg:(`Hex "#1BA1F2")
      ; success = c ~fg:(`Name "black") ~bg:(`Hex "#57c961")
      ; warning = c ~fg:(`Name "black") ~bg:(`Hex "#ffbe01")
      ; error = c ~fg:(`Name "white") ~bg:(`Hex "#f2581b")
      }
  ; table =
      { body_row_even = table_even
      ; body_row_odd = primary
      ; header_row = header
      ; header_header_border = extreme_primary_border
      ; header_body_border = extreme_primary_border
      ; body_body_border = extreme_primary_border
      }
  }
;;

let app_attr ~color =
  Vdom.Attr.many [ App.Variables.set () ~bg:(color |> Color.to_string_css); App.app ]
;;

let v1 =
  View.Expert.override_theme View.Expert.default_theme ~f:(fun (module M) ->
    (module struct
      class c =
        object (self)
          inherit M.c
          method! theme_name = "kado v1"
          method! constants = dark_mode_constants
          method! app_attr = app_attr ~color:self#constants.primary.background
          method! devbar = Devbar.make self#constants
          method! tabs = Tabs.make
          method! button = Buttons.make self#constants
          method! use_intent_fg_or_bg_for_highlighting = `Bg
          method! codemirror_theme = Some Nord
        end
    end))
;;

let theme ?(contrast = Contrast.Standard) ?(style = Style.Dark) ~version () =
  let Standard, Dark = contrast, style in
  match version with
  | Version.V1 -> v1
  | Bleeding -> v1
;;
