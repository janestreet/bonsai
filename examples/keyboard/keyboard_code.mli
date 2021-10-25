open! Core
include module type of Js_of_ocaml.Dom_html.Keyboard_code

include sig
  type t [@@deriving equal, sexp]
end
with type t := t
