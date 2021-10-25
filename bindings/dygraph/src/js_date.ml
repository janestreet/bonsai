open! Core
open  Import

include Js_obj.Make (struct
    type t = Js.date
  end)
