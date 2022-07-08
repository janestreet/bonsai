open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component =
  let%sub height, increase_height =
    Bonsai.state_machine0
      (module Int)
      (module Unit)
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ old_model () -> old_model + 10)
  in
  let%sub append_effect =
    let%arr height = height in
    Effect.of_sync_fun
      Inline_css.Private.append
      [%string {|
        .my_box {
          height: %{height#Int}px;
        }
      |}]
  in
  let%sub effect =
    let%arr increase_height = increase_height
    and append_effect = append_effect in
    let%bind.Effect () = append_effect in
    increase_height ()
  in
  let%arr effect = effect in
  Vdom.Node.div
    [ Vdom.Node.div
        ~attr:
          (Vdom.Attr.many
             [ Vdom.Attr.class_ "my_box"
             ; Vdom.Attr.style
                 Css_gen.(background_color (`Hex "#000000") @> width (`Px 20))
             ])
        []
    ; Vdom.Node.button
        ~attr:(Vdom.Attr.on_click (fun _ -> effect))
        [ Vdom.Node.text "Append style!" ]
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
