open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Drag_and_drop = Bonsai_web_ui_drag_and_drop
module Node = Vdom.Node
module Attr = Vdom.Attr

module Column_name =
  String_id.Make_without_pretty_printer
    (struct
      let module_name = "Bonsai_drag_and_drop_example.Column_name"
    end)
    ()

module Item_id : sig
  include Identifiable.S

  val of_int : int -> t
end =
  Int

module Kanban_board = struct
  type t = (string * Column_name.t) Item_id.Map.t [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | Move of
        { item_id : Item_id.t
        ; new_column : Column_name.t
        }
  [@@deriving sexp]
end

let[@warning "-16"] kanban_column ?extra_dnd ~dnd ~items ~column_name ~title =
  let map =
    let%map items = items
    and model = dnd >>| Drag_and_drop.model in
    (* Only display items from this column or items that are being hovered
       over this column; exclude any items that have been dragged away and
       are hovered over a different column *)
    Map.filteri items ~f:(fun ~key ~data:(_, item_column) ->
      match model with
      | Not_dragging | Dragging { target = None; _ } ->
        [%equal: Column_name.t] item_column column_name
      | Dragging { source = item_id; target = Some target_column; _ } ->
        let from_this_column = Column_name.equal item_column column_name in
        let is_the_dragged_item = [%equal: Item_id.t] item_id key in
        let from_target_column = [%equal: Column_name.t] column_name target_column in
        (from_this_column && not is_the_dragged_item)
        || (is_the_dragged_item && from_target_column))
  in
  let%sub extra_source =
    match extra_dnd with
    | Some dnd -> return (dnd >>| Drag_and_drop.source)
    | None -> Bonsai.const (fun ~id:_ -> Attr.empty)
  in
  let%sub items =
    Bonsai.assoc
      (module Item_id)
      map
      ~f:(fun item_id item ->
        return
          (let%map item = item
           and item_id = item_id
           and source = dnd >>| Drag_and_drop.source
           and model = dnd >>| Drag_and_drop.model
           and extra_source = extra_source in
           let contents, _ = item in
           let extra =
             match model with
             | Not_dragging -> Attr.empty
             | Dragging { source = source_item_id; target = None; _ } ->
               if Item_id.equal item_id source_item_id
               then Attr.class_ "being-dragged"
               else Attr.empty
             | Dragging { source = source_item_id; target = Some target_column; _ } ->
               if Item_id.equal item_id source_item_id
               then
                 if Column_name.equal target_column column_name
                 then Attr.class_ "dragged-on-self"
                 else Attr.class_ "being-dragged"
               else Attr.empty
           in
           Node.div
             ~key:(Item_id.to_string item_id)
             ~attr:
               Attr.(
                 source ~id:item_id
                 @ class_ "kanban-item"
                 @ extra
                 @ extra_source ~id:item_id)
             [ Node.text contents ]))
  in
  return
    (let%map items = items
     and drop_target = dnd >>| Drag_and_drop.drop_target
     and model = dnd >>| Drag_and_drop.model in
     let is_active =
       match model with
       | Dragging { target = Some target_column; _ }
         when Column_name.equal column_name target_column -> true
       | _ -> false
     in
     Node.div
       ~attr:
         Attr.(
           drop_target ~id:column_name
           @ class_ "kanban-column"
           @ class_ [%string "kanban-column-%{column_name#Column_name}"]
           @ if is_active then class_ "kanban-column-active" else empty)
       [ Node.h3 [ Node.text title ]; Node.div (Map.data items) ])
;;

let board ?extra_dnd name =
  let todo, in_progress, finished =
    Column_name.(of_string "todo", of_string "in-progress", of_string "finished")
  in
  let%sub items, inject =
    Bonsai.state_machine0
      [%here]
      (module Kanban_board)
      (module Action)
      ~default_model:
        ([ "todo 1", todo
         ; "todo 2", todo
         ; "in progress 1", in_progress
         ; "in progress 2", in_progress
         ; "in progress 3", in_progress
         ; "finished 1", finished
         ; "finished 2", finished
         ; "finished 3", finished
         ]
         |> List.mapi ~f:(fun i item -> Item_id.of_int i, item)
         |> Map.of_alist_exn (module Item_id))
      ~apply_action:
        (fun ~inject:_ ~schedule_event:_ model (Move { item_id; new_column }) ->
           let change_col (contents, _) ~new_column = contents, new_column in
           Map.change model item_id ~f:(Option.map ~f:(change_col ~new_column)))
  in
  let%sub dnd =
    Drag_and_drop.create
      [%here]
      ~source_id:(module Item_id)
      ~target_id:(module Column_name)
      ~on_drop:
        (let%map inject = inject in
         fun item_id new_column -> inject (Move { item_id; new_column }))
  in
  let%sub todo = kanban_column ?extra_dnd ~dnd ~items ~column_name:todo ~title:"Todo" in
  let%sub in_progress =
    kanban_column ?extra_dnd ~dnd ~items ~column_name:in_progress ~title:"In Progress"
  in
  let%sub finished =
    kanban_column ?extra_dnd ~dnd ~items ~column_name:finished ~title:"Done"
  in
  let%sub dragged_element =
    Drag_and_drop.dragged_element dnd ~f:(fun item_id ->
      let%sub text =
        match%sub
          let%map item_id = item_id
          and items = items in
          Map.find items item_id
        with
        | Some (contents, _) -> return contents
        | None -> Bonsai.const "No item exists with that id"
      in
      return
        (let%map text = text in
         Node.div ~attr:(Attr.class_ "kanban-item") [ Node.text text ]))
  in
  let%sub sentinel = return (dnd >>| Drag_and_drop.sentinel) in
  let%sub view =
    return
      (let%map todo = todo
       and in_progress = in_progress
       and finished = finished
       and dragged_element = dragged_element
       and sentinel = sentinel in
       let sentinel = sentinel ~name in
       Node.div
         ~attr:Attr.(class_ "kanban-container" @ sentinel)
         [ todo; in_progress; finished; dragged_element ])
  in
  return (Bonsai.Value.both view dnd)
;;

let app =
  let%sub board1, dnd = board "board1" in
  let%sub board2, _ = board ~extra_dnd:dnd "board2" in
  return
    (let%map board1 = board1
     and board2 = board2 in
     Node.div
       [ Node.p
           [ Node.text
               "You can drag items between each of the columns. When dropped, an item \
                gets placed in a location determined by some ordering within the column, \
                rather than the position at which it is dropped (This was more \
                straightforward to implement). There are two drag-and-drop universes \
                corresponding to each of the kanban boards. Interestingly, the items in \
                the bottom board are draggable items of both universes; while the \
                resulting behavior is not intuitive for a kanban board, it does \
                demonstrate how multiple universes interact."
           ]
       ; Node.div [ board1 ]
       ; Node.div [ board2 ]
       ])
;;

let board name = Bonsai.Computation.map (board ?extra_dnd:None name) ~f:fst
