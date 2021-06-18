open! Core
open Bonsai_web
open Bonsai.Let_syntax


(* [CODE_EXCERPT_BEGIN 2] *)
module Model = struct
  type t = unit Int.Map.t [@@deriving sexp, equal]
end

let add_counter_component =
  let%sub add_counter_state =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module Unit)
      ~default_model:Int.Map.empty
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model () ->
        let key = Map.length model in
        Map.add_exn model ~key ~data:())
  in
  return
  @@ let%map state, inject = add_counter_state in
  let on_click = Vdom.Attr.on_click (fun _ -> inject ()) in
  let view =
    Vdom.Node.button ~attr:on_click [ Vdom.Node.text "Add Another Counter" ]
  in
  state, view
;;

(* [CODE_EXCERPT_END 2] *)

(* [CODE_EXCERPT_BEGIN 1] *)

module Action = struct
  type t =
    | Increment
    | Decrement
  [@@deriving sexp_of]
end

let single_counter =
  let%sub counter_state =
    Bonsai.state_machine0
      [%here]
      (module Int)
      (module Action)
      ~default_model:0
      ~apply_action:
        (fun ~inject:_ ~schedule_event:_ model -> function
           | Action.Increment -> model + 1
           | Action.Decrement -> model - 1)
  in
  return
  @@ let%map state, inject = counter_state in
  let button label action =
    let on_click = Vdom.Attr.on_click (fun _ -> inject action) in
    Vdom.Node.button ~attr:on_click [ Vdom.Node.text label ]
  in
  Vdom.Node.div
    [ button "-1" Action.Decrement
    ; Vdom.Node.text (Int.to_string state)
    ; button "+1" Action.Increment
    ]
;;

(* [CODE_EXCERPT_END 1] *)

(* [CODE_EXCERPT_BEGIN 3] *)
let application =
  let open Bonsai.Let_syntax in
  let%sub map, add_button = add_counter_component in
  let%sub counters =
    Bonsai.assoc (module Int) map ~f:(fun _key _data -> single_counter)
  in
  return
  @@ let%map add_button = add_button
  and counters = counters in
  Vdom.Node.div [ add_button; Vdom.Node.div (Map.data counters) ]
;;

(* [CODE_EXCERPT_END 3] *)

let _application_sugar_free =
  let open Bonsai.Let_syntax in
  Let_syntax.sub add_counter_component ~f:(fun add_counter ->
    let map = Value.map add_counter ~f:(fun (map, _) -> map) in
    let add_button = Value.map add_counter ~f:(fun (_, add_button) -> add_button) in
    Let_syntax.sub
      (Bonsai.assoc (module Int) map ~f:(fun _key _data -> single_counter))
      ~f:(fun counters ->
        return
          (Value.map2 add_button counters ~f:(fun add_button counters ->
             Vdom.Node.div [ add_button; Vdom.Node.div (Map.data counters) ]))))
;;
