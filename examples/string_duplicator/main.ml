open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let string_duplicator input_string graph =
  let duplication_count_state =
    Tuple2.uncurry Bonsai.both
    @@ Bonsai.state_machine0
         graph
         ~sexp_of_model:[%sexp_of: Int.t]
         ~equal:[%equal: Int.t]
         ~sexp_of_action:[%sexp_of: Unit.t]
         ~default_model:1
         ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model () -> model + 1)
  in
  let%arr num_duplicated, inject_duplicate = duplication_count_state
  and input_string = input_string in
  let repeated_string =
    List.init num_duplicated ~f:(Fn.const input_string) |> String.concat ~sep:" "
  in
  (* [inject] is used to produce an [Event.t] which is handled by Bonsai, and
     the action comes back in to be processed by [apply_action]. *)
  let on_click = Vdom.Attr.on_click (fun _ -> inject_duplicate ()) in
  let button = Vdom.Node.button ~attrs:[ on_click ] [ Vdom.Node.text "duplicate" ] in
  Vdom.Node.div [ button; Vdom.Node.text repeated_string ]
;;

let string_to_repeat graph =
  let state =
    Tuple2.uncurry Bonsai.both
    @@ Bonsai.state
         "hello"
         ~sexp_of_model:[%sexp_of: String.t]
         ~equal:[%equal: String.t]
         graph
  in
  let%arr state, set_state = state in
  let view =
    Vdom.Node.textarea
      ~attrs:
        [ Vdom.Attr.string_property "value" state
        ; Vdom.Attr.on_input (fun _ -> set_state)
        ]
      []
  in
  state, view
;;

let app
  (* let%sub can decompose the [(string * Vdom.Node.t) Value.t] into both a
         [string Value.t] and a [Vdom.Node.t Value.t]. *)
    graph
  =
  let%sub string, textbox_view = string_to_repeat graph in
  let duplicated = string_duplicator string graph in
  let%arr textbox_view = textbox_view
  and duplicated = duplicated in
  Vdom.Node.div [ textbox_view; duplicated ]
;;

let () = Bonsai_web.Start.start app
