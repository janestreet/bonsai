open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_web_ui_partial_render_table_protocol

type 'col_id t =
  { order : 'col_id Order.t
  ; inject : 'col_id Order.Action.t -> unit Effect.t
  ; col_id : (module Col_id with type t = 'col_id)
  }

let order t = t.order
let inject t = t.inject

let state
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
  { order; inject; col_id = (module Col_id) }
;;

module Header = struct
  let assoc_findi ~f list =
    match List.findi list ~f:(fun _i (key, _) -> f key) with
    | None -> None
    | Some (index, (_k, v)) -> Some (index, v)
  ;;

  module Expert = struct
    let default_click_handler
      (type col_id)
      { order; inject; col_id }
      ~column_id
      ~sortable
      f
      =
      let module Col_id = (val col_id : Col_id with type t = col_id) in
      let handle_click =
        Vdom.Attr.on_click (fun mouse_event ->
          if Js_of_ocaml.Js.to_bool mouse_event##.shiftKey
          then inject (Add_sort column_id)
          else inject (Set_sort column_id))
      in
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
      Table_view.Header_label.wrap_clickable ~sortable ~handle_click header_node
    ;;
  end

  let with_icon = Table_view.Header_label.wrap_with_icon

  (* Not in `Table_view.ml`, because we don't add theming to legacy implementations. *)
  module Legacy = struct
    module Icons = struct
      (** White Diamond symbol (U+25C7) *)
      let neutral = "◇ "

      (** Diamond with Top Black Half symbol (U+2B18) *)
      let ascending = "⬘ "

      (** Diamond with Bottom Black Half symbol (U+2B19) *)
      let descending = "⬙ "
    end

    let wrap_with_icon (label : Vdom.Node.t) (sort_spec : Sort_state.t) =
      let get_icon = function
        | `None -> Icons.neutral
        | `Asc -> Icons.ascending
        | `Desc -> Icons.descending
      in
      let render ~dir = Vdom.Node.span [ Vdom.Node.text (get_icon dir); label ] in
      match sort_spec with
      | Not_sortable -> label
      | Not_sorted -> render ~dir:`None
      | Single_sort dir | Multi_sort { dir; _ } -> render ~dir
    ;;
  end
end
