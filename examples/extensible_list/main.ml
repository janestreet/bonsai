open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Extendy = Bonsai_web_ui_extendy

let component =
  let wrap_remove view remove_event =
    Vdom.Node.div
      [ Vdom.Node.button
          ~attr:(Vdom.Attr.on_click (fun _ -> remove_event))
          [ Vdom.Node.text "X" ]
      ; view
      ]
  in
  let%sub { contents; append; _ } =
    Extendy.component' [%here] Bonsai_web_counters_example.single_counter ~wrap_remove
  in
  let%arr contents = contents
  and append = append in
  let views = Map.data contents in
  Vdom.Node.div
    (Vdom.Node.button
       ~attr:(Vdom.Attr.on_click (fun _ -> append))
       [ Vdom.Node.text "Add" ]
     :: views)
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
