open! Core
open Ppxlib

let print_expr expr = Pprintast.string_of_expression expr |> print_string
