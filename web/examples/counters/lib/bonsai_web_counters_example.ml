open! Core_kernel
open Bonsai_web

module Model = struct
  type t = unit Int.Map.t [@@deriving sexp, equal]

  let default = Int.Map.empty
end

module Add_counter_component = struct
  module Input = Unit
  module Model = Model

  module Action = struct
    type t = Add_another_counter [@@deriving sexp]
  end

  module Result = struct
    type t = Model.t * Vdom.Node.t
  end

  let apply_action ~inject:_ ~schedule_event:_ () model = function
    | Action.Add_another_counter ->
      let key = Map.length model in
      Map.add_exn model ~key ~data:()
  ;;

  let compute ~inject () model =
    let on_click = Vdom.Attr.on_click (fun _ -> inject Action.Add_another_counter) in
    let view = Vdom.Node.button [ on_click ] [ Vdom.Node.text "Add Another Counter" ] in
    model, view
  ;;

  let name = Source_code_position.to_string [%here]
end

module Counter_component = struct
  module Input = Unit
  module Result = Vdom.Node
  module Model = Int

  module Action = struct
    type t =
      | Increment
      | Decrement
    [@@deriving sexp_of]
  end

  let apply_action ~inject:_ ~schedule_event:_ () model = function
    | Action.Increment -> model + 1
    | Action.Decrement -> model - 1
  ;;

  let compute ~inject () model =
    let button label action =
      let on_click = Vdom.Attr.on_click (fun _ -> inject action) in
      Vdom.Node.button [ on_click ] [ Vdom.Node.text label ]
    in
    Vdom.Node.div
      []
      [ button "-1" Action.Decrement
      ; Vdom.Node.text (Int.to_string model)
      ; button "+1" Action.Increment
      ]
  ;;

  let name = Source_code_position.to_string [%here]
end

let single_counter = Bonsai.of_module ~default_model:0 (module Counter_component)

let counters_component =
  single_counter
  |> Bonsai.Map.assoc_input (module Int)
  |> Bonsai.map ~f:(fun result_map -> Vdom.Node.div [] (Map.data result_map))
;;

let add_counter_component =
  Bonsai.of_module (module Add_counter_component) ~default_model:Model.default
;;

let application =
  let open Bonsai.Let_syntax in
  let%map counters, add_button =
    add_counter_component >>> Bonsai.Arrow.first counters_component
  in
  Vdom.Node.div [] [ add_button; counters ]
;;
