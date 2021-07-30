open! Core
open! Import

type t =
  { on_activate : unit Ui_effect.t option
  ; on_deactivate : unit Ui_effect.t option
  ; after_display : unit Ui_effect.t option
  }

module Collection = struct
  type nonrec t = t Path.Map.t

  let empty = Path.Map.empty
  let has_after_display t = Map.exists t ~f:(fun t -> Option.is_some t.after_display)

  let maybe_cons hd tl =
    let open Reversed_list in
    match hd with
    | Some a -> a :: tl
    | None -> tl
  ;;

  let diff old new_ =
    (* collect the activations and deactivations separately so that we can run them
       in a different order *)
    let after_displays =
      let collect ~key:_ ~data:{ after_display; _ } = maybe_cons after_display in
      Map.fold new_ ~init:Reversed_list.[] ~f:collect
    in
    let activations, deactivations =
      let collect (activations, deactivations) = function
        | _, `Left { on_deactivate; _ } ->
          activations, maybe_cons on_deactivate deactivations
        | _, `Right { on_activate; _ } ->
          maybe_cons on_activate activations, deactivations
        | _ -> activations, deactivations
      in
      let data_equal = phys_equal in
      Map.fold_symmetric_diff
        old
        new_
        ~data_equal
        ~init:(Reversed_list.[], Reversed_list.[])
        ~f:collect
    in
    Ui_effect.Many
      [ deactivations |> Reversed_list.rev |> Ui_effect.Many
      ; activations |> Reversed_list.rev |> Ui_effect.Many
      ; after_displays |> Reversed_list.rev |> Ui_effect.Many
      ]
  ;;
end
