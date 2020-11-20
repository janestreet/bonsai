open! Core_kernel
open! Import

type t =
  { on_activate : Ui_event.t option
  ; on_deactivate : Ui_event.t option
  ; after_display : Ui_event.t option
  }

module Collection = struct
  type nonrec t = t Path.Map.t

  let empty = Path.Map.empty

  let maybe_cons hd tl =
    match hd with
    | Some a -> a :: tl
    | None -> tl
  ;;

  let diff old new_ =
    (* collect the activations and deactivations separately so that we can run them
       in a different order *)
    let after_displays =
      let collect ~key:_ ~data:{ after_display; _ } = maybe_cons after_display in
      Map.fold new_ ~init:[] ~f:collect
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
      Map.fold_symmetric_diff old new_ ~data_equal ~init:([], []) ~f:collect
    in
    Ui_event.Many
      [ deactivations |> List.rev |> Ui_event.Many
      ; activations |> List.rev |> Ui_event.Many
      ; after_displays |> List.rev |> Ui_event.Many
      ]
  ;;
end
