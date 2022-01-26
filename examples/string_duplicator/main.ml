open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let string_duplicator input_string =
  let%sub duplication_count_state =
    Bonsai.state_machine0
      [%here]
      (module Int)
      (module Unit)
      ~default_model:1
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model () -> model + 1)
  in
  let%arr num_duplicated, inject_duplicate = duplication_count_state
  and input_string = input_string in
  let repeated_string =
    List.init num_duplicated ~f:(Fn.const input_string) |> String.concat ~sep:" "
  in
  (* [inject] is used to produce an [Event.t] which is handled by Bonsai, and
     the action comes back in to be processed by [apply_action]. *)
  let on_click = Vdom.Attr.on_click (fun _ -> inject_duplicate ()) in
  let button = Vdom.Node.button ~attr:on_click [ Vdom.Node.text "duplicate" ] in
  Vdom.Node.div [ button; Vdom.Node.text repeated_string ]
;;

let string_to_repeat =
  let%sub state = Bonsai.state [%here] (module String) ~default_model:"hello" in
  let%arr state, set_state = state in
  let view =
    Vdom.Node.textarea
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.string_property "value" state
           ; Vdom.Attr.on_input (fun _ -> set_state)
           ])
      []
  in
  state, view
;;

let app =
  (* let%sub can decompose the [(string * Vdom.Node.t) Value.t] into both a
     [string Value.t] and a [Vdom.Node.t Value.t]. *)
  let%sub string, textbox_view = string_to_repeat in
  let%sub duplicated = string_duplicator string in
  let%arr textbox_view = textbox_view
  and duplicated = duplicated in
  Vdom.Node.div [ textbox_view; duplicated ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app
;;
