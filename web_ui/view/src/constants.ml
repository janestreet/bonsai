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

type t =
  { primary : Fg_bg.t
  ; intent : Intent.colors
  }
