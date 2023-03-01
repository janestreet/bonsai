open! Core
open! Bonsai_web
module Lib = All_url_var_features_example

let url_var =
  Bonsai_web_ui_url_var.Typed.make
    (module Lib.T)
    ~fallback:Lib.fallback
    Lib.versioned_parser
;;

let component = Lib.component ~url_var
let () = Bonsai_web.Start.start component
