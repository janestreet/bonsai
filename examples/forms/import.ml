open! Core
open! Bonsai_web

let view_t ~sexp_of t =
  t |> sexp_of |> Sexp.to_string_hum |> Vdom.Node.text |> List.return |> Vdom.Node.pre
;;
