open! Core
open! Bonsai_web
include Dropdown_menu_intf

module Make (Enum : Enum) = struct
  module T = struct
    module Input = Unit
    module Model = Enum

    module Action = struct
      type t = Set_to of Model.t [@@deriving sexp_of]
    end

    module Result = struct
      type t = Enum.t * Vdom.Node.t
    end

    let apply_action ~inject:_ ~schedule_event:_ () _ (Action.Set_to model) = model

    let compute ~inject () model =
      let dom_node =
        Vdom_input_widgets.Dropdown.of_enum
          (module Enum)
          ~selected:model
          ~on_change:(fun new_value -> inject (Action.Set_to new_value))
      in
      model, dom_node
    ;;

    let name = Enum.name
  end

  let component = Bonsai.of_module0 (module T)
end
