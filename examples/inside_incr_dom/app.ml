open Core
open Incr_dom
open Incr.Let_syntax

module Model = struct
  type t =
    { subcomponent_model : My_bonsai_component.Model.t
    ; other_model : string
    }
  [@@deriving fields]

  let cutoff = phys_equal
end

module State = My_bonsai_component.State

module Action = struct
  type t =
    | Subcomponent_action of My_bonsai_component.Action.t
    | Update_string of string
  [@@deriving sexp_of]
end

let initial_model =
  { Model.subcomponent_model = My_bonsai_component.Model.default
  ; other_model = "type into this box"
  }
;;

let on_startup ~schedule_action:_ _model =
  Async_kernel.Deferred.return (My_bonsai_component.State.create ())
;;

let apply_action model bonsai_subcomponent =
  let%map bonsai_subcomponent = bonsai_subcomponent
  and model = model in
  fun action state ~schedule_action ->
    match action with
    | Action.Subcomponent_action a ->
      let schedule_action a = schedule_action (Action.Subcomponent_action a) in
      let new_subcomponent_model =
        Component.apply_action bonsai_subcomponent a state ~schedule_action
      in
      { model with Model.subcomponent_model = new_subcomponent_model }
    | Action.Update_string s -> { model with other_model = s }
;;

let view model bonsai_subcomponent ~inject =
  let%map bonsai_subcomponent = bonsai_subcomponent
  and model = model in
  let on_change _ text = inject (Action.Update_string text) in
  Vdom.Node.div
    [ Vdom.Node.input ~attr:(Vdom.Attr.on_input on_change) []
    ; Vdom.Node.text (String.uppercase model.Model.other_model)
    ; Component.view bonsai_subcomponent
    ]
;;

let create model ~old_model ~inject =
  let bonsai_subcomponent =
    let input = Incr.const () in
    let model = model >>| Model.subcomponent_model in
    let old_model = old_model >>| Model.subcomponent_model >>| Option.some in
    let inject a = inject (Action.Subcomponent_action a) in
    My_bonsai_component.create ~input ~model ~old_model ~inject
  in
  let%map model = model
  and apply_action = apply_action model bonsai_subcomponent
  and view = view model bonsai_subcomponent ~inject in
  Component.create ~apply_action model view
;;
