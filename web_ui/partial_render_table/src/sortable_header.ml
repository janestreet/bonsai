open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_web_ui_partial_render_table_protocol

type 'col_id t =
  { order : 'col_id Order.t
  ; decorate : Vdom.Node.t -> 'col_id -> Vdom.Node.t
  }
[@@deriving fields]

let component
      (type col_id)
      ?(initial_order = Value.return Order.default)
      (module Col_id : Col_id with type t = col_id)
  =
  let module Action = struct
    type t = Col_id.t Order.Action.t [@@deriving sexp_of]
  end
  in
  let module State = struct
    type t = Col_id.t Order.t [@@deriving sexp, equal]

    let apply_action model action = Order.apply_action model (module Col_id) action
  end
  in
  let%sub state =
    Bonsai_extra.state_machine0_dynamic_model
      (module State)
      (module Action)
      ~model:(`Given initial_order)
      ~apply_action:(fun ~inject:_ ~schedule_event:_ -> State.apply_action)
    |> Bonsai.Incr.model_cutoff
  in
  let%arr order, inject = state in
  let decorate label col_id =
    let sort_marker =
      Vdom.Node.text
        (match List.Assoc.find order ~equal:Col_id.equal col_id with
         | Some `Asc -> Icons.ascending
         | Some `Desc -> Icons.descending
         | None -> Icons.neutral)
    in
    let attr =
      Vdom.Attr.(
        class_ Style.column_header
        @ on_click (fun mouse_event ->
          if Js_of_ocaml.Js.to_bool mouse_event##.shiftKey
          then inject (Add_sort col_id)
          else inject (Set_sort col_id)))
    in
    Vdom.Node.div ~attr [ Vdom.Node.span [ sort_marker; label ] ]
  in
  { order; decorate }
;;
