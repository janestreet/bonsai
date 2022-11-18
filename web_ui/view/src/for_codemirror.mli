open! Core

module Theme : sig
  type t =
    | Basic_dark
    | Basic_light
    | Gruvbox_dark
    | Nord
    | Solarized_dark
    | Solarized_light
    | Material_dark
  [@@deriving sexp_of]
end
