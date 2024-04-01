open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Extendy = Bonsai_web_ui_extendy

let component graph =
  let wrap_remove view remove_event =
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> remove_event) ]
          [ Vdom.Node.text "X" ]
      ; view
      ]
  in
  let%sub { contents; append; _ } =
    Extendy.component' Bonsai_web_counters_example.single_counter ~wrap_remove graph
  in
  let%arr contents = contents
  and append = append in
  let views = Map.data contents in
  Vdom.Node.div
    (Vdom.Node.button
       ~attrs:[ Vdom.Attr.on_click (fun _ -> append) ]
       [ Vdom.Node.text "Add" ]
     :: views)
;;

let () = Bonsai_web.Start.start component
