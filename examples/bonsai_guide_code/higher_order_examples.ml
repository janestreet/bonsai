open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

(* $MDX part-begin=hoc_modal *)

type t =
  { view : Vdom.Node.t Bonsai.t
  ; open_modal : unit Effect.t Bonsai.t
  }

let modal
  ~(title : Vdom.Node.t Bonsai.t)
  ~(content : Bonsai.graph -> Vdom.Node.t Bonsai.t)
  graph
  : t
  =
  let is_open, set_is_open = Bonsai.state false graph in
  let open_modal =
    let%arr set_is_open = set_is_open in
    set_is_open true
  in
  let view =
    match%sub is_open with
    | false -> Bonsai.return Vdom.Node.none
    | true ->
      (* only instantiate [content] here in the [true] branch *)
      let%arr content = content graph
      and title = title
      and set_is_open = set_is_open in
      let close_button =
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> set_is_open false) ]
          [ Vdom.Node.text "X" ]
      in
      Vdom.Node.div
        ~attrs:
          [ [%css
              {|
            position: fixed;
            top: 0; bottom: 0; left: 0; right: 0;
            height: fit-content; width: fit-content;
            margin: auto;
            border: 1px solid black;
            background-color: white;|}]
          ]
        [ Vdom.Node.h1 [ title; close_button ]; content ]
  in
  { view; open_modal }
;;

(* $MDX part-end *)

(* $MDX part-begin=modal_example *)
let modal_example graph =
  let title = Bonsai.return (Vdom.Node.text "Hi there!") in
  let content graph =
    let count, set_count = Bonsai.state 0 graph in
    let on_activate =
      let%arr count = count
      and set_count = set_count in
      set_count (count + 1)
    in
    let () = Bonsai.Edge.lifecycle ~on_activate graph in
    let%arr count = count in
    Vdom.Node.div
      [ Vdom.Node.text [%string "This modal has been opened %{count#Int} times..."] ]
  in
  let { view = modal_view; open_modal } = modal ~title ~content graph in
  let%arr modal_view = modal_view
  and open_modal = open_modal in
  Vdom.Node.div
    ~attrs:[ [%css {|height: 400px;|}] ]
    [ modal_view
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> open_modal) ]
        [ Vdom.Node.text "open modal" ]
    ]
;;

(* $MDX part-end *)

let () = Util.run modal_example ~id:"modal_example"
