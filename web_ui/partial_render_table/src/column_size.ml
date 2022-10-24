open! Core

type t =
  | Visible of { width_px : float }
  | Hidden of { prev_width_px : float option }
[@@deriving sexp, equal]
