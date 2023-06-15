open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_web_ui_partial_render_table_protocol

type 'col_id t =
  { order : 'col_id Order.t
  ; decorate : Vdom.Node.t -> 'col_id -> Vdom.Node.t
  ; render :
      column_id:'col_id -> sortable:bool -> (Sort_state.t -> Vdom.Node.t) -> Vdom.Node.t
  ; inject : 'col_id Order.Action.t -> unit Effect.t
  }
[@@deriving fields]

let assoc_findi ~f list =
  match List.findi list ~f:(fun _i (key, _) -> f key) with
  | None -> None
  | Some (index, (_k, v)) -> Some (index, v)
;;

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
  let%sub order, inject =
    Bonsai_extra.state_machine0_dynamic_model
      ~sexp_of_model:[%sexp_of: State.t]
      ~equal:[%equal: State.t]
      (module Action)
      ~model:(`Given initial_order)
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) -> State.apply_action)
  in
  let%sub order = return (Value.cutoff ~equal:[%equal: State.t] order) in
  let state = Value.both order inject in
  let%arr order, inject = state in
  let handle_click col_id =
    Vdom.Attr.on_click (fun mouse_event ->
      if Js_of_ocaml.Js.to_bool mouse_event##.shiftKey
      then inject (Add_sort col_id)
      else inject (Set_sort col_id))
  in
  let decorate label col_id =
    let sort_marker =
      Vdom.Node.text
        (match List.Assoc.find order ~equal:Col_id.equal col_id with
         | Some `Asc -> Icons.ascending
         | Some `Desc -> Icons.descending
         | None -> Icons.neutral)
    in
    Vdom.Node.div
      ~attrs:[ Style.column_header; handle_click col_id ]
      [ Vdom.Node.span [ sort_marker; label ] ]
  in
  let render ~column_id ~sortable f =
    let (sort_state : Sort_state.t) =
      if not sortable
      then Not_sortable
      else (
        let col_state = assoc_findi ~f:(Col_id.equal column_id) order in
        match col_state with
        | None -> Not_sorted
        | Some (index, dir) ->
          if List.length order = 1
          then Single_sort dir
          else Multi_sort { dir; index = index + 1 })
    in
    let header_node = f sort_state in
    let attrs =
      if sortable then [ Style.column_header; handle_click column_id ] else []
    in
    Vdom.Node.div ~attrs [ header_node ]
  in
  { order; decorate; render; inject }
;;
