open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let widget =
  Vdom.Node.widget_of_module
    (module struct
      type dom = Js_of_ocaml.Dom_html.divElement

      let name = "test"

      module Input = Bool
      module State = Int

      let create _ =
        let state = Random.int 100 in
        let element = Js_of_ocaml.Dom_html.createDiv Js_of_ocaml.Dom_html.document in
        element##.innerHTML := Js_of_ocaml.Js.string (sprintf "%d" state);
        state, element
      ;;

      let update ~prev_input:_ ~input:_ ~state ~element =
        element##.innerHTML := Js_of_ocaml.Js.string (sprintf "%d" state);
        state, element
      ;;

      let destroy ~prev_input:_ ~state:_ ~element:_ = ()
    end)
;;

let widget = unstage widget

let inner_html_a =
  Vdom.Node.inner_html
    ~tag:"div"
    ~attr:Vdom.Attr.empty
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:"hello"
;;

let inner_html_b =
  Vdom.Node.inner_html
    ~tag:"div"
    ~attr:Vdom.Attr.empty
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:"world"
;;

let component true_or_false ~toggle =
  return
  @@ let%map true_or_false = true_or_false in
  let widget = widget true_or_false in
  let ids =
    Vdom.Node.div
      [ widget
      ; widget
      ; Vdom.Node.button
          ~attr:(Vdom.Attr.on_click (fun _ -> toggle (not true_or_false)))
          [ Vdom.Node.text "click me" ]
      ]
  in
  let inner_html =
    Vdom.Node.div [ (if true_or_false then inner_html_a else inner_html_b) ]
  in
  Vdom.Node.div [ ids; inner_html ]
;;

let (_ : _ Start.Handle.t) =
  let var = Bonsai.Var.create true in
  let toggle = Bonsai.Effect.of_sync_fun (Bonsai.Var.set var) in
  let toggle a = toggle a in
  let value = Bonsai.Var.value var in
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (component value ~toggle)
;;
