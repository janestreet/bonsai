open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

(* $MDX part-begin=textbox *)
let textbox : (string * Vdom.Node.t) Computation.t =
  let%sub state, set_state = Bonsai.state [%here] (module String) ~default_model:"" in
  return
    (let%map state = state
     and set_state = set_state in
     let view =
       Vdom.Node.input
         ~attr:
           Vdom.Attr.(value_prop state @ on_input (fun _ new_text -> set_state new_text))
         []
     in
     state, view)
;;

(* $MDX part-end *)

let () = Util.run (textbox |> Computation.map ~f:snd) ~id:"textbox"

(* $MDX part-begin=two_textboxes *)
let two_textboxes : Vdom.Node.t Computation.t =
  let%sub textbox_a = textbox in
  let%sub textbox_b = textbox in
  return
    (let%map contents_a, view_a = textbox_a
     and contents_b, view_b = textbox_b in
     let display = Vdom.Node.textf "a: %s, b: %s" contents_a contents_b in
     Vdom.Node.div
       ~attr:(Vdom.Attr.style (Css_gen.display `Inline_grid))
       [ view_a; view_b; display ])
;;

(* $MDX part-end *)

let () = Util.run two_textboxes ~id:"two_textboxes"

(* $MDX part-begin=two_textboxes_shared_state *)
let two_textboxes_shared_state : Vdom.Node.t Computation.t =
  let%sub textbox_a = textbox in
  let textbox_b = textbox_a in
  return
    (let%map contents_a, view_a = textbox_a
     and contents_b, view_b = textbox_b in
     let display = Vdom.Node.textf "a: %s, b: %s" contents_a contents_b in
     Vdom.Node.div
       ~attr:(Vdom.Attr.style (Css_gen.display `Inline_grid))
       [ view_a; view_b; display ])
;;

(* $MDX part-end *)

let () = Util.run two_textboxes_shared_state ~id:"two_textboxes_shared_state"

(* $MDX part-begin=counter_state *)
let state_based_counter : Vdom.Node.t Computation.t =
  let%sub state, set_state = Bonsai.state [%here] (module Int) ~default_model:0 in
  return
    (let%map state = state
     and set_state = set_state in
     let decrement =
       Vdom.Node.button
         ~attr:(Vdom.Attr.on_click (fun _ -> set_state (state - 1)))
         [ Vdom.Node.text "-1" ]
     in
     let increment =
       Vdom.Node.button
         ~attr:(Vdom.Attr.on_click (fun _ -> set_state (state + 1)))
         [ Vdom.Node.text "+1" ]
     in
     Vdom.Node.div [ decrement; Vdom.Node.textf "%d" state; increment ])
;;

(* $MDX part-end *)

let () = Util.run state_based_counter ~id:"state_based_counter"

(* $MDX part-begin=counter_state_machine *)

module Action = struct
  type t =
    | Increment
    | Decrement
  [@@deriving sexp_of]
end

let counter_state_machine : Vdom.Node.t Computation.t =
  let%sub state, inject =
    Bonsai.state_machine0
      [%here]
      (module Int)
      (module Action)
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match action with
        | Increment -> model + 1
        | Decrement -> model - 1)
  in
  return
    (let%map state = state
     and inject = inject in
     let decrement =
       Vdom.Node.button
         ~attr:(Vdom.Attr.on_click (fun _ -> inject Decrement))
         [ Vdom.Node.text "-1" ]
     in
     let increment =
       Vdom.Node.button
         ~attr:(Vdom.Attr.on_click (fun _ -> inject Increment))
         [ Vdom.Node.text "+1" ]
     in
     Vdom.Node.div [ decrement; Vdom.Node.textf "%d" state; increment ])
;;

(* $MDX part-end *)

let () = Util.run counter_state_machine ~id:"counter_state_machine"
