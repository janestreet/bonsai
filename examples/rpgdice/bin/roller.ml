open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Rpgdice = Bonsai_web_rpgdice_example

module Model = struct
  type t = (Rpgdice.Roll_spec.t * Rpgdice.Roll_result.t) option [@@deriving equal, sexp]
end

let roller_state =
  Bonsai.state_machine1
    ~sexp_of_model:[%sexp_of: Model.t]
    ~equal:[%equal: Model.t]
    ~sexp_of_action:[%sexp_of: Unit.t]
    ~default_model:None
    ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) roll_spec model () ->
      match roll_spec with
      | Active roll_spec ->
        (match roll_spec with
         | Ok spec -> Some (spec, Rpgdice.Roll_spec.roll spec)
         | Error _ -> None)
      | Inactive ->
        eprint_s
          [%message
            [%here]
              "An action sent to a [state_machine1] has been dropped because its input was \
               not present. This happens when the [state_machine1] is inactive when it \
               receives a message."];
        model)
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
    ~attrs:[ Vdom.Attr.id "roller" ]
    (Vdom_input_widgets.Button.simple
       ~merge_behavior:Legacy_dont_merge
       ~on_click:(fun () -> inject ())
       "reroll"
     :: roll_result)
;;
