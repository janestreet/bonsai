open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Drag_and_drop = Bonsai_web_ui_drag_and_drop
module Node = Vdom.Node

module Style =
  [%css
    stylesheet
      {|
  .centered {
    text-align: center;
  }

  .kanban_container {
    display: flex;
    flex-direction: row;
  }

  .kanban_column {
    padding: 10px;
    margin: 10px;
    flex: 1;
    min-height : 200px;
    border: 3px solid white;
  }

  .kanban_column_active {
    border: 3px dashed black;
  }

  .kanban_column_todo {
    background-color: brown;
  }

  .kanban_column_in_progress {
    background-color: lightblue;
  }

  .kanban_column_finished {
    background-color: lightgreen;
  }

  .kanban_item {
    background-color: purple;
    color: white;
    margin: 10px;
    padding: 10px;
  }

  .being_dragged {
    opacity: 0.5;
    background-color: #990033;
  }

  .dragged_on_self {
    background-color: black;
  }

  .disable_child_pointer_events * {
    pointer-events: none;
  }
  |}]

module Column = struct
  type t =
    | Todo
    | In_progress
    | Finished
  [@@deriving sexp, equal]

  include Stringable
end

module Item_id : sig
  include Identifiable.S

  val of_int : int -> t
end =
  Int

module Kanban_board = struct
  type t = (string * Column.t) Item_id.Map.t [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | Move of
        { item_id : Item_id.t
        ; new_column : Column.t
        }
  [@@deriving sexp]
end

let kanban_column ~extra_dnd ~dnd ~items ~column ~title =
  let map =
    let%map items = items
    and model = dnd >>| Drag_and_drop.model in
    (* Only display items from this column or items that are being hovered
       over this column; exclude any items that have been dragged away and
       are hovered over a different column *)
    Map.filteri items ~f:(fun ~key ~data:(_, item_column) ->
      match model with
      | Not_dragging | Dragging { target = None; _ } ->
        [%equal: Column.t] item_column column
      | Dragging { source = item_id; target = Some target_column; _ } ->
        let from_this_column = [%equal: Column.t] item_column column in
        let is_the_dragged_item = [%equal: Item_id.t] item_id key in
        let from_target_column = [%equal: Column.t] column target_column in
        (from_this_column && not is_the_dragged_item)
        || (is_the_dragged_item && from_target_column))
  in
  let%sub extra_source =
    match extra_dnd with
    | Some dnd -> Bonsai.pure Drag_and_drop.source dnd
    | None -> Bonsai.const (fun ~id:_ -> Vdom.Attr.empty)
  in
  let%sub items =
    Bonsai.assoc
      (module Item_id)
      map
      ~f:(fun item_id item ->
        let%arr item = item
        and item_id = item_id
        and source = dnd >>| Drag_and_drop.source
        and model = dnd >>| Drag_and_drop.model
        and extra_source = extra_source in
        let contents, _ = item in
        let extra =
          match model with
          | Not_dragging -> Vdom.Attr.empty
          | Dragging { source = source_item_id; target = None; _ } ->
            if Item_id.equal item_id source_item_id
            then Style.being_dragged
            else Vdom.Attr.empty
          | Dragging { source = source_item_id; target = Some target_column; _ } ->
            if Item_id.equal item_id source_item_id
            then
              if [%equal: Column.t] target_column column
              then Style.dragged_on_self
              else Style.being_dragged
            else Vdom.Attr.empty
        in
        Node.div
          ~key:(Item_id.to_string item_id)
          ~attr:
            Vdom.Attr.(
              source ~id:item_id @ Style.kanban_item @ extra @ extra_source ~id:item_id)
          [ Node.text contents ])
  in
  let%arr items = items
  and drop_target = dnd >>| Drag_and_drop.drop_target
  and model = dnd >>| Drag_and_drop.model in
  let is_active =
    match model with
    | Dragging { target = Some target_column; _ }
      when [%equal: Column.t] column target_column -> true
    | _ -> false
  in
  let column_class =
    match column with
    | Todo -> Style.kanban_column_todo
    | In_progress -> Style.kanban_column_in_progress
    | Finished -> Style.kanban_column_finished
  in
  Node.div
    ~attr:
      Vdom.Attr.(
        drop_target ~id:column
        @ Style.kanban_column
        @ column_class
        @ if is_active then Style.kanban_column_active else empty)
    [ Node.h3 ~attr:Style.centered [ Node.text title ]; Node.div (Map.data items) ]
;;

let board ?extra_dnd name =
  let%sub items, inject =
    Bonsai.state_machine0
      (module Kanban_board)
      (module Action)
      ~default_model:
        ([ "todo 1", Column.Todo
         ; "todo 2", Todo
         ; "in progress 1", In_progress
         ; "in progress 2", In_progress
         ; "in progress 3", In_progress
         ; "finished 1", Finished
         ; "finished 2", Finished
         ; "finished 3", Finished
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
      ~source_id:(module Item_id)
      ~target_id:(module Column)
      ~on_drop:
        (let%map inject = inject in
         fun item_id new_column -> inject (Move { item_id; new_column }))
  in
  let%sub todo = kanban_column ~extra_dnd ~dnd ~items ~column:Todo ~title:"Todo" in
  let%sub in_progress =
    kanban_column ~extra_dnd ~dnd ~items ~column:In_progress ~title:"In Progress"
  in
  let%sub finished =
    kanban_column ~extra_dnd ~dnd ~items ~column:Finished ~title:"Done"
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
      let%arr text = text in
      Node.div ~attr:Style.kanban_item [ Node.text text ])
  in
  let%sub sentinel = Bonsai.pure Drag_and_drop.sentinel dnd in
  let%sub view =
    let%arr todo = todo
    and in_progress = in_progress
    and finished = finished
    and dragged_element = dragged_element
    and sentinel = sentinel in
    let sentinel = sentinel ~name in
    Node.div
      ~attr:Vdom.Attr.(Style.kanban_container @ sentinel)
      [ todo; in_progress; finished; dragged_element ]
  in
  return (Bonsai.Value.both view dnd)
;;

let app =
  let%sub board1, dnd = board "board1" in
  let%sub board2, _ = board ~extra_dnd:dnd "board2" in
  let%arr board1 = board1
  and board2 = board2 in
  Node.div
    [ Node.p
        [ Node.text
            {|
             You can drag items between each of the columns. When dropped, an item gets
             placed in a location determined by some ordering within the column, rather
             than the position at which it is dropped (This was more straightforward to
             implement). There are two drag-and-drop universes corresponding to each of
             the kanban boards. Interestingly, the items in the bottom board are
             draggable items of both universes; while the resulting behavior is not
             intuitive for a kanban board, it does demonstrate how multiple universes
             interact.
            |}
        ]
    ; Node.div [ board1 ]
    ; Node.div [ board2 ]
    ]
;;

let board name = Bonsai.Computation.map (board ?extra_dnd:None name) ~f:fst
