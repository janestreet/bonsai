open! Core_kernel
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
  return
  @@ let%map num_duplicated, inject_duplicate = duplication_count_state
  and input_string = input_string in
  let repeated_string =
    List.init num_duplicated ~f:(Fn.const input_string) |> String.concat ~sep:" "
  in
  (* [inject] is used to produce an [Event.t] which is handled by Bonsai,
     and the action comes back in to be processed by [apply_action]. *)
  let on_click = Vdom.Attr.on_click (fun _ -> inject_duplicate ()) in
  let button = Vdom.Node.button [ on_click ] [ Vdom.Node.text "duplicate" ] in
  Vdom.Node.div [] [ button; Vdom.Node.text repeated_string ]
;;

let string_to_repeat =
  let open Bonsai.Let_syntax in
  let%sub state =
    Bonsai.state_machine0
      [%here]
      (module String)
      (module String)
      ~default_model:"hello"
      ~apply_action:(fun ~inject:_ ~schedule_event:_ _ -> Fn.id)
  in
  return
  @@ let%map state, set_state = state in
  let view =
    Vdom.Node.textarea
      [ Vdom.Attr.string_property "value" state
      ; Vdom.Attr.on_input (fun _ -> set_state)
      ]
      []
  in
  state, view
;;

let app =
  let open Bonsai.Let_syntax in
  let%sub string_to_repeat = string_to_repeat in
  (* Pattern-bind is used to extract the [(string * Vdom.Node.t) Bonsai.Value.t]
     into both a [string Bonsai.Value.t] and a [Vdom.Node.t Bonsai.Value.t]. *)
  let%pattern_bind string, textbox_view = string_to_repeat in
  let%sub duplicated = string_duplicator string in
  return
  @@ let%map textbox_view = textbox_view
  and duplicated = duplicated in
  Vdom.Node.div [] [ textbox_view; duplicated ]
;;

(* Start the app off with the text "hello" and the starting
   number of repetitions at 1. *)
let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app
;;
