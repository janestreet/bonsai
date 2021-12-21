open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Fruit = struct
  type t =
    | Apple
    | Blueberry
    | Banana
    | Pineapple
  [@@deriving sexp, equal, sexp_grammar]
end

module Query = struct
  type t = Fruit.t Blang.t [@@deriving sexp, equal, sexp_grammar]
end

let component =
  let%sub { view; _ } =
    Bonsai_web_ui_codemirror.with_sexp_grammar_autocompletion
      (Value.return Query.t_sexp_grammar)
  in
  return view
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
