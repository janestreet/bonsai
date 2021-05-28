open! Core_kernel
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

let kanban_column ~dnd ~items ~column_name ~title =
  let map =
    let%map items = items
    and model = dnd >>| Drag_and_drop.model in
    (* Only display items from this column or items that are being hovered
       over this column; exclude any items that have been dragged away and
       are hovered over a different column *)
    Map.filteri items ~f:(fun ~key ~data:(_, item_column) ->
      match model with
      | Not_dragging | Dragging _ -> [%equal: Column_name.t] item_column column_name
      | Over_target (item_id, target_column) ->
        let from_this_column = Column_name.equal item_column column_name in
        let is_the_dragged_item = [%equal: Item_id.t] item_id key in
        let from_target_column = [%equal: Column_name.t] column_name target_column in
        (from_this_column && not is_the_dragged_item)
        || (is_the_dragged_item && from_target_column))
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
           and model = dnd >>| Drag_and_drop.model in
           let contents, _ = item in
           let extra =
             match model with
             | Not_dragging -> Attr.empty
             | Dragging source_item_id ->
               if Item_id.equal item_id source_item_id
               then Attr.class_ "being-dragged"
               else Attr.empty
             | Over_target (source_item_id, target_column) ->
               if Item_id.equal item_id source_item_id
               then
                 if Column_name.equal target_column column_name
                 then Attr.class_ "dragged-on-self"
                 else Attr.class_ "being-dragged"
               else Attr.empty
           in
           Node.div
             ~key:(Item_id.to_string item_id)
             Attr.[ source ~id:item_id @ class_ "kanban-item" @ extra ]
             [ Node.text contents ]))
  in
  return
    (let%map items = items
     and drop_target = dnd >>| Drag_and_drop.drop_target
     and model = dnd >>| Drag_and_drop.model in
     let is_active =
       match model with
       | Over_target (_, target_column) when Column_name.equal column_name target_column
         -> true
       | _ -> false
     in
     Node.div
       Attr.
         [ (drop_target ~id:column_name
            @ class_ "kanban-column"
            @ class_ [%string "kanban-column-%{column_name#Column_name}"]
            @ if is_active then class_ "kanban-column-active" else empty)
         ]
       [ Node.h3 [] [ Node.text title ]; Node.div [] (Map.data items) ])
;;

let board =
  let%sub dnd =
    Drag_and_drop.create
      [%here]
      ~source_id:(module Item_id)
      ~target_id:(module Column_name)
  in
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
  let%sub () =
    let inject_move =
      let%map inject = inject in
      fun item_id new_column ~source_element_position:_ ->
        inject (Move { item_id; new_column })
    in
    Drag_and_drop.on_drop (module Item_id) (module Column_name) dnd ~f:inject_move
  in
  let%sub todo = kanban_column ~dnd ~items ~column_name:todo ~title:"Todo" in
  let%sub in_progress =
    kanban_column ~dnd ~items ~column_name:in_progress ~title:"In Progress"
  in
  let%sub finished = kanban_column ~dnd ~items ~column_name:finished ~title:"Done" in
  return
    (let%map todo = todo
     and in_progress = in_progress
     and finished = finished in
     Node.div [ Attr.class_ "kanban-container" ] [ todo; in_progress; finished ])
;;

let app =
  let%sub board1 = board in
  let%sub board2 = board in
  return
    (let%map board1 = board1
     and board2 = board2 in
     Node.div
       []
       [ Node.p
           []
           [ Node.text
               "You can drag items between each of the columns. When dropped, an item \
                gets placed in a location determined by some ordering withing the \
                column, rather than the position at which it is dropped. (This was more \
                straightforward to implement). Items cannot be dragged between the two \
                rows. (to demonstrate how multiple universes interact)"
           ]
       ; Node.div [ Attr.id "top-board" ] [ board1 ]
       ; Node.div [ Attr.id "bottom-board" ] [ board2 ]
       ])
;;
