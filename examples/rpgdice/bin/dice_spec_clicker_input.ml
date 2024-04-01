open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Rpgdice = Bonsai_web_rpgdice_example

module Model = struct
  type t =
    { dice : int Int.Map.t
    ; const : int
    }
  [@@deriving equal, sexp]

  (* The standard RPG dice. *)
  let default_dice = Int.Set.of_list [ 4; 6; 8; 10; 12; 20; 100 ]
  let init = { dice = Map.of_key_set default_dice ~f:(Fn.const 0); const = 0 }

  let to_spec { dice; const } =
    let dice =
      Map.to_alist dice
      |> List.map ~f:(fun (num_faces, count) ->
           count, Rpgdice.Roll_spec.Die.of_int num_faces)
    in
    Rpgdice.Roll_spec.of_dice_and_const dice const
  ;;
end

module Action = struct
  type t =
    | Decrement_const
    | Increment_const
    | Increment of { num_faces : int }
    | Clear
  [@@deriving sexp_of]
end

let component graph =
  let dice_state =
    Tuple2.uncurry Bonsai.both
    @@ Bonsai.state_machine0
         graph
         ~sexp_of_model:[%sexp_of: Model.t]
         ~equal:[%equal: Model.t]
         ~sexp_of_action:[%sexp_of: Action.t]
         ~default_model:Model.init
         ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model -> function
         | Decrement_const -> { model with const = model.const - 1 }
         | Increment_const -> { model with const = model.const + 1 }
         | Increment { num_faces } ->
           { model with
             dice =
               Map.update model.dice num_faces ~f:(function
                 | None -> failwith "map keys shouldn't have changed"
                 | Some v -> v + 1)
           }
         | Clear -> { const = 0; dice = Map.map model.dice ~f:(Fn.const 0) })
  in
  let%arr model, inject = dice_state in
  let button = Vdom_input_widgets.Button.simple in
  let dice_button num_faces =
    button
      ~merge_behavior:Legacy_dont_merge
      ~on_click:(fun () -> inject (Action.Increment { num_faces }))
      (sprintf "d%d" num_faces)
  in
  let buttons =
    Vdom.Node.div
      (button
         ~merge_behavior:Legacy_dont_merge
         ~on_click:(fun () -> inject Action.Clear)
         "clear"
       :: Vdom.Node.br ()
       :: Vdom.Node.div
            [ Vdom.Node.label [ Vdom.Node.text "constant adjustment" ]
            ; button
                ~merge_behavior:Legacy_dont_merge
                ~on_click:(fun () -> inject Action.Decrement_const)
                "-1"
            ; button
                ~merge_behavior:Legacy_dont_merge
                ~on_click:(fun () -> inject Action.Increment_const)
                "+1"
            ]
       :: Vdom.Node.br ()
       :: (Map.keys model.dice |> List.map ~f:dice_button))
  in
  let spec = Model.to_spec model in
  let display = spec |> Rpgdice.Roll_spec.to_string_hum |> Vdom.Node.text in
  spec, Vdom.Node.div [ buttons; display ]
;;
