open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let add_counter_component graph =
  let state, inject =
    Bonsai.state_machine0
      graph
      ~default_model:Int.Map.empty
      ~apply_action:(fun _ctx model () ->
      let key = Map.length model in
      Map.add_exn model ~key ~data:())
  in
  let view =
    let%map inject = inject in
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> inject ()) ]
      [ Vdom.Node.text "Add Another Counter" ]
  in
  state, view
;;

module Action = struct
  type t =
    | Increment
    | Decrement
  [@@deriving sexp_of]
end

let single_counter graph =
  let state, inject =
    Bonsai.state_machine0 graph ~default_model:0 ~apply_action:(fun _ctx model -> function
      | Action.Increment -> model + 1
      | Action.Decrement -> model - 1)
  in
  let%map state = state
  and inject = inject in
  let button label action =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> inject action) ]
      [ Vdom.Node.text label ]
  in
  Vdom.Node.div
    [ button "-1" Action.Decrement
    ; Vdom.Node.text (Int.to_string state)
    ; button "+1" Action.Increment
    ]
;;

let application graph =
  let map, add_button = add_counter_component graph in
  let counters =
    Bonsai.assoc (module Int) map graph ~f:(fun _key _data graph -> single_counter graph)
  in
  let%map add_button = add_button
  and counters = counters in
  Vdom.Node.div [ add_button; Vdom.Node.div (Map.data counters) ]
;;

let () = Bonsai_web.Start.start application
