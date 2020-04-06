open! Core_kernel
open! Bonsai_web

module String_duplicator = struct
  (* Input is declared to be the string that we want to duplicate. *)
  module Input = String

  (* The result is the VDom view. *)
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
    let repeated_string =
      List.init model ~f:(Fn.const input) |> String.concat ~sep:" "
    in
    (* [inject] is used to produce an [Event.t] which is handled by Bonsai,
       and the action comes back in to be processed by [apply_action]. *)
    let on_click = Vdom.Attr.on_click (fun _ -> inject Action.Increment) in
    let button = Vdom.Node.button [ on_click ] [ Vdom.Node.text "duplicate" ] in
    Vdom.Node.div [] [ button; Vdom.Node.text repeated_string ]
  ;;

  let name = "string repeater component"
end

(* Start the app off with the text "hello" and the starting
   number of repetitions at 1. *)
let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:"hello"
    ~bind_to_element_with_id:"app"
    (Bonsai.of_module (module String_duplicator) ~default_model:1)
;;
