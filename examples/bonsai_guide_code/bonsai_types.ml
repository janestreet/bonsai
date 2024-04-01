open! Core
module Bonsai = Bonsai.Cont

let assoc = Bonsai.assoc

let state_machine0 ~default_model ~apply_action graph =
  Bonsai.state_machine0 ~default_model ~apply_action graph
;;

let peek = Bonsai.peek

module Url_var = Bonsai_web_ui_url_var
