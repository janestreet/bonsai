open! Core_kernel
open! Async_kernel
open! Import

module Input = struct
  type t = Rpgdice.Roll_spec.t Or_error.t
end

module Model = struct
  type t = (Rpgdice.Roll_spec.t * Rpgdice.Roll_result.t) option

  let init = None
end

module Action = struct
  type t = Reroll [@@deriving sexp_of]
end

module Result = Vdom.Node

let apply_action ~inject:_ ~schedule_event:_ input _model (action : Action.t) =
  match action with
  | Reroll ->
    input |> Or_error.ok |> Option.map ~f:(fun spec -> spec, Rpgdice.Roll_spec.roll spec)
;;

let compute ~inject input (model : Model.t) =
  let roll_result =
    match input, model with
    | Ok input, Some (spec, roll) when phys_equal spec input ->
      [ Vdom.Node.pre [] [ Vdom.Node.text (Rpgdice.Roll_result.to_string_hum roll) ]
      ; Vdom.Node.div
          []
          [ Vdom.Node.text (sprintf "Total: %d" (Rpgdice.Roll_result.to_int roll)) ]
      ]
    | _, None | _, Some _ -> []
  in
  Vdom.Node.div
    [ Vdom.Attr.id "roller" ]
    (Vdom_input_widgets.Button.simple ~on_click:(fun () -> inject Action.Reroll) "reroll"
     :: roll_result)
;;

let name = Source_code_position.to_string [%here]
let initial_model = Model.init
