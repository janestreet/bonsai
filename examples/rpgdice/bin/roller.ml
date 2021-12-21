open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Rpgdice = Bonsai_web_rpgdice_example

module Model = struct
  type t = (Rpgdice.Roll_spec.t * Rpgdice.Roll_result.t) option [@@deriving equal, sexp]
end

let roller_state =
  Bonsai.state_machine1
    [%here]
    (module Model)
    (module Unit)
    ~default_model:None
    ~apply_action:(fun ~inject:_ ~schedule_event:_ roll_spec _model () ->
      match roll_spec with
      | Ok spec -> Some (spec, Rpgdice.Roll_spec.roll spec)
      | Error _ -> None)
;;

let component roll_spec =
  let%sub roller_state = roller_state roll_spec in
  let%arr model, inject = roller_state
  and roll_spec = roll_spec in
  let roll_result =
    match roll_spec, model with
    | Ok roll_spec, Some (spec, roll) when phys_equal spec roll_spec ->
      [ Vdom.Node.pre [ Vdom.Node.text (Rpgdice.Roll_result.to_string_hum roll) ]
      ; Vdom.Node.div
          [ Vdom.Node.text (sprintf "Total: %d" (Rpgdice.Roll_result.to_int roll)) ]
      ]
    | _, None | _, Some _ -> []
  in
  Vdom.Node.div
    ~attr:(Vdom.Attr.id "roller")
    (Vdom_input_widgets.Button.simple ~on_click:(fun () -> inject ()) "reroll"
     :: roll_result)
;;
