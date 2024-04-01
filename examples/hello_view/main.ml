open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let my_component graph =
  let theme = View.Theme.current graph in
  let%arr theme = theme in
  View.vbox
    ~cross_axis_alignment:Center
    ~gap:(`Em 1)
    [ View.themed_text theme ~intent:Info "hello"
    ; View.themed_text theme ~intent:Error "world"
    ]
;;

let app =
  let theme = Bonsai.return (Kado.theme ~version:Bleeding ()) in
  View.Theme.set_for_app theme my_component
;;

let () = Bonsai_web.Start.start app
