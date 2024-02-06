open! Core
open! Import

module Fg_bg = struct
  type t =
    { foreground : Color.t
    ; background : Color.t
    }
end

module Intent = struct
  type t =
    | Info
    | Success
    | Warning
    | Error
  [@@deriving sexp, equal, compare, enumerate]

  type colors =
    { info : Fg_bg.t
    ; success : Fg_bg.t
    ; warning : Fg_bg.t
    ; error : Fg_bg.t
    }

  let lookup { info; success; warning; error } = function
    | Info -> info
    | Success -> success
    | Warning -> warning
    | Error -> error
  ;;
end

module Font_style = struct
  type t =
    | Regular
    | Bold
    | Italic
    | Underlined
  [@@deriving sexp, equal, compare, enumerate]
end

module Font_size = struct
  type t =
    | Small
    | Regular
    | Large
  [@@deriving sexp, equal, compare, enumerate]
end

module Table = struct
  type t =
    { body_row_even : Fg_bg.t
    ; body_row_odd : Fg_bg.t
    ; body_row_focused : Fg_bg.t
    ; body_cell_focused : Fg_bg.t
    ; header_row : Fg_bg.t
    ; header_header_border : Color.t
    ; header_body_border : Color.t
    ; body_body_border : Color.t
    ; body_row_focused_border : Color.t
    }
end

module Form = struct
  type t =
    { error_message : Fg_bg.t
    ; error_toggle_text : Color.t
    ; error_border : Color.t
    ; tooltip_message : Fg_bg.t
    ; tooltip_toggle_text : Color.t
    ; tooltip_border : Color.t
    }
end

type t =
  { primary : Fg_bg.t
  ; extreme : Fg_bg.t
  ; extreme_primary_border : Color.t
  ; intent : Intent.colors
  ; table : Table.t
  ; form : Form.t
  ; small_font_size : Css_gen.Length.t
  ; large_font_size : Css_gen.Length.t
  ; is_dark : bool
  }

module Card_title_kind = struct
  type t =
    | Prominent
    | Discreet
end
