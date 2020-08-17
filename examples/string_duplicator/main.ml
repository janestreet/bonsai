open! Core_kernel
open! Bonsai_web

module String_duplicator = struct
  (* Input is declared to be the string that we want to duplicate. *)
  module Input = String

  (* The result is the Vdom view. *)
  module Result = Vdom.Node

  (* The state machine is just a number representing the number of times we want to dupe
     our string. *)
  module Model = Int

  module Action = struct
    (* The only transition in the state machine increments the int. *)
    type t = Increment [@@deriving sexp_of]
  end

  (* When an [Increment] action is seen, compute the new state. *)
  let apply_action ~inject:_ ~schedule_event:_ _input model = function
    | Action.Increment -> model + 1
  ;;

  (* The view is computed here: it includes the repeated string alongside the
     button that injects actions in to be processed in [apply_action]. *)
  let compute ~inject input model =
    let repeated_string = List.init model ~f:(Fn.const input) |> String.concat ~sep:" " in
    (* [inject] is used to produce an [Event.t] which is handled by Bonsai,
       and the action comes back in to be processed by [apply_action]. *)
    let on_click = Vdom.Attr.on_click (fun _ -> inject Action.Increment) in
    let button = Vdom.Node.button [ on_click ] [ Vdom.Node.text "duplicate" ] in
    Vdom.Node.div [] [ button; Vdom.Node.text repeated_string ]
  ;;

  let name = "string repeater component"
end

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
  ( state
  , Vdom.Node.textarea
      [ Vdom.Attr.string_property "value" state
      ; Vdom.Attr.on_input (fun _ -> set_state)
      ]
      [] )
;;

let app =
  let open Bonsai.Let_syntax in
  let%sub string_to_repeat = string_to_repeat in
  let%pattern_bind string, textbox_view = string_to_repeat in
  let%sub duplicated =
    (Bonsai.of_module1 (module String_duplicator) ~default_model:1) string
  in
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
