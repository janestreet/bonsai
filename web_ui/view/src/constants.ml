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

module Table = struct
  type t =
    { body_row_even : Fg_bg.t
    ; body_row_odd : Fg_bg.t
    ; header_row : Fg_bg.t
    ; header_header_border : Color.t
    ; header_body_border : Color.t
    ; body_body_border : Color.t
    }
end

type t =
  { primary : Fg_bg.t
  ; extreme : Fg_bg.t
  ; extreme_primary_border : Color.t
  ; intent : Intent.colors
  ; table : Table.t
  }
