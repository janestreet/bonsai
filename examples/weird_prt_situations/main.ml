open! Core
open! Bonsai_web.Cont
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic
module Column = Table.Columns.Dynamic_cells
module Form = Bonsai_web_ui_form.With_automatic_view

let header text = Column.Sortable.Header.with_icon (Vdom.Node.text text) |> Bonsai.return

let columns =
  Column.lift
    [ Column.column
        ~header:(header "i")
        ~cell:(fun ~key ~data:_ _graph ->
          let%arr key = key in
          Vdom.Node.text (Int.to_string key))
        ()
    ; Column.column
        ~header:(header "i * 3")
        ~cell:(fun ~key ~data:_ _graph ->
          let%arr key = key in
          Vdom.Node.text (Int.to_string (key * 3)))
        ()
    ]
;;

module Css = [%css stylesheet {|
.table {
  height: 75vh;
  overflow: auto;
}
|}]

module Which = struct
  type t =
    | Stacked_tables
    | Separately_scrollable
  [@@deriving enumerate, sexp, equal, compare]
end

module Table_id = struct
  type t =
    | First_table
    | Second_table
  [@@deriving enumerate, sexp, equal, compare]
end

let component graph =
  let data = Bonsai.return (Int.Map.of_alist_exn (List.init 100 ~f:(fun i -> i, ()))) in
  let focused_table, set_focused_table =
    Bonsai.state
      First_table
      ~sexp_of_model:[%sexp_of: Table_id.t]
      ~equal:[%equal: Table_id.t]
      graph
  in
  let on_change which =
    let%map set_focused_table = set_focused_table in
    function
    | None -> Effect.Ignore
    | Some _ -> set_focused_table which
  in
  let%sub { view = table1; focus = focus1; _ } =
    Table.component
      (module Int)
      ~focus:(By_row { on_change = on_change First_table })
      ~row_height:(Bonsai.return (`Px 30))
      ~columns
      data
      graph
  in
  let%sub { view = table2; focus = focus2; _ } =
    Table.component
      (module Int)
      ~focus:(By_row { on_change = on_change Second_table })
      ~row_height:(Bonsai.return (`Px 30))
      ~columns
      data
      graph
  in
  let () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: Table_id.t]
      ~equal:[%equal: Table_id.t]
      focused_table
      ~callback:
        (let%map focus1 = focus1
         and focus2 = focus2 in
         function
         | Table_id.First_table -> Table.Focus.By_row.unfocus focus2
         | Second_table -> Table.Focus.By_row.unfocus focus1)
      graph
  in
  let which_form = Form.Elements.Dropdown.enumerable (module Which) graph in
  let which =
    let%arr which_form = which_form in
    Form.value_or_default ~default:Which.Stacked_tables which_form
  in
  let tables =
    match%sub which with
    | Stacked_tables ->
      let%arr table1 = table1
      and table2 = table2 in
      Vdom.Node.div [ Vdom.Node.div [ table1 ]; Vdom.Node.div [ table2 ] ]
    | Separately_scrollable ->
      let%arr table1 = table1
      and table2 = table2 in
      Vdom.Node.div
        [ Vdom.Node.div ~attrs:[ Css.table ] [ table1 ]
        ; Vdom.Node.div ~attrs:[ Css.table ] [ table2 ]
        ]
  in
  let%arr tables = tables
  and which_form = which_form
  and focused_table = focused_table
  and focus1 = focus1
  and focus2 = focus2 in
  let attr =
    Vdom.Attr.many
      [ Vdom.Attr.on_keydown (fun kbc ->
          let focus =
            match focused_table with
            | First_table -> focus1
            | Second_table -> focus2
          in
          let binding =
            let module Focus_control = Table.Focus.By_row in
            match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
            | ArrowDown | KeyJ -> Some (Focus_control.focus_down focus)
            | ArrowUp | KeyK -> Some (Focus_control.focus_up focus)
            | PageDown -> Some (Focus_control.page_down focus)
            | PageUp -> Some (Focus_control.page_up focus)
            | Escape -> Some (Focus_control.unfocus focus)
            | _ -> None
          in
          match binding with
          | Some b -> Effect.Many [ Effect.Prevent_default; b ]
          | None -> Effect.Ignore)
      ]
  in
  Vdom.Node.div ~attrs:[ attr ] [ Form.view_as_vdom which_form; tables ]
;;

let () = Bonsai_web.Start.start component
