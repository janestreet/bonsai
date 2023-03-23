open Core
open Import
open Gen_js_api

type t = string [@@deriving compare, equal, sexp]

let t_to_js = Ojs.string_to_js
let t_of_js = Ojs.string_of_js
let of_string s = s

let view ~tag t =
  Vdom.Node.inner_html
    ~tag
    ~attrs:[ Vdom.Attr.empty ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:t
    ()
;;
