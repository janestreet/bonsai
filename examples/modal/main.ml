open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let center = Vdom.Attr.style (Css_gen.text_align `Center)

let modal_1_contents =
  Bonsai.const (Vdom.Node.div ~attrs:[ center ] [ Vdom.Node.text "Surprise!" ])
;;

let modal_2_contents n =
  let%sub got_ya =
    match%arr n with
    | 1 -> "Got ya!"
    | n -> sprintf "Got ya %d times!" n
  in
  let%arr got_ya = got_ya in
  Vdom.Node.div
    ~attrs:[ center ]
    [ Vdom.Node.text "Surprise!"; Vdom.Node.br (); Vdom.Node.text got_ya ]
;;

let app =
  let%sub modal_1 =
    Bonsai_web_ui_modal.create
      (module Unit)
      (fun _ ~hide_self:_ -> modal_1_contents)
      ~equal:[%equal: Unit.t]
  in
  let%sub modal_2 =
    Bonsai_web_ui_modal.create
      (module Int)
      (fun n ~hide_self:_ -> modal_2_contents n)
      ~equal:[%equal: Int.t]
  in
  let%sub state, set_state =
    Bonsai.state 1 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
  in
  let%arr state = state
  and set_state = set_state
  and modal_1   = modal_1
  and modal_2   = modal_2 in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> modal_1.show ()) ]
        [ Vdom.Node.text "Click me!" ]
    ; modal_1.view
    ; Vdom.Node.br ()
    ; Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.on_click (fun _ ->
              let%bind.Ui_effect () = set_state (state + 1) in
              modal_2.show state)
          ; Vdom.Attr.style    (Css_gen.margin_top (`Px 10))
          ]
        [ Vdom.Node.text "Click me multiple times!" ]
    ; modal_2.view
    ]
;;

let () = Bonsai_web.Start.start app
