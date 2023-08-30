open! Core
open Import

include Js_obj.Make (struct
  type t = Dom_html.element
end)
