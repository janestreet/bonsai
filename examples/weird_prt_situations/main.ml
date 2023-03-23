open! Core
open! Bonsai_web
open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic
module Column = Table.Columns.Dynamic_cells
module Form = Bonsai_web_ui_form

let columns =
  [ Column.column
      ~label:(Value.return (Vdom.Node.text "i"))
      ~cell:(fun ~key ~data:_ ->
        let%arr key = key in
        Vdom.Node.text (Int.to_string key))
      ()
  ; Column.column
      ~label:(Value.return (Vdom.Node.text "i * 3"))
      ~cell:(fun ~key ~data:_ ->
        let%arr key = key in
        Vdom.Node.text (Int.to_string (key * 3)))
      ()
  ]
  |> Column.lift
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

let component =
  let%sub data =
    Bonsai.const (Int.Map.of_alist_exn (List.init 100 ~f:(fun i -> i, ())))
  in
  let%sub focused_table, set_focused_table =
    Bonsai.state (module Table_id) ~default_model:First_table
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
      ~row_height:(`Px 30)
      ~columns
      data
  in
  let%sub { view = table2; focus = focus2; _ } =
    Table.component
      (module Int)
      ~focus:(By_row { on_change = on_change Second_table })
      ~row_height:(`Px 30)
      ~columns
      data
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module Table_id)
      focused_table
      ~callback:
        (let%map focus1 = focus1
         and focus2 = focus2 in
         function
         | Table_id.First_table -> focus2.unfocus
         | Second_table -> focus1.unfocus)
  in
  let%sub which_form = Form.Elements.Dropdown.enumerable (module Which) in
  let%sub which =
    let%arr which_form = which_form in
    Form.value_or_default ~default:Which.Stacked_tables which_form
  in
  let%sub tables =
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
            match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
            | ArrowDown | KeyJ -> Some focus.focus_down
            | ArrowUp | KeyK -> Some focus.focus_up
            | PageDown -> Some focus.page_down
            | PageUp -> Some focus.page_up
            | Escape -> Some focus.unfocus
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
