open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic
module Form = Bonsai_web_ui_form
module Row = Row

module Time_ns_option = struct
  type t = Time_ns.t option [@@deriving compare]

  let to_string = function
    | None -> "-"
    | Some t -> Time_ns.to_string_utc t
  ;;
end

module Column = Table.Columns.Dynamic_experimental

module Col_id = struct
  include Row.Typed_field.Packed
  include Comparator.Make (Row.Typed_field.Packed)
end

let component ?filter (data : Row.t String.Map.t Value.t) =
  let all = Value.return Row.Typed_field.Packed.all in
  let%sub form = Form.Elements.Typeahead.list (module Col_id) ~all_options:all in
  let%sub form = Form.Dynamic.with_default all form in
  let%sub columns =
    return (form >>| Form.value_or_default ~default:Row.Typed_field.Packed.all)
  in
  let%sub table =
    Table.component
      (module String)
      ?filter
      ~theming:`Themed
      ~focus:(By_row { on_change = Value.return (Fn.const Effect.Ignore) })
      ~row_height:(Value.return (`Px 30))
      ~columns:
        (Column.build
           (module Col_id)
           ~columns
           ~render_cell:(fun col _key data ->
             let%arr { f = T field } = col
             and data = data in
             let string, float, int =
               Vdom.Node.text, Vdom.Node.textf "%f", Vdom.Node.textf "%d"
             in
             let value = Row.Typed_field.get field data in
             match field with
             | Symbol -> string value
             | Edge -> float value
             | Max_edge -> float value
             | Bsize -> int value
             | Bid -> float value
             | Ask -> float value
             | Asize -> int value
             | Position -> int value
             | Last_fill -> Vdom.Node.text (Time_ns_option.to_string value)
             | Trader -> string value)
           ~render_header:(fun col ->
             let%arr { f = T field } = col in
             Table.Columns.Dynamic_columns.Sortable.Header.with_icon
               (Vdom.Node.text (Row.Typed_field.name field))))
      data
  in
  let%arr { Table.Result.view = table
          ; for_testing = _
          ; sortable_state = _
          ; num_filtered_rows
          ; focus
          }
    =
    table
  and form = form in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.on_keydown (fun kbc ->
          let binding =
            let module Focus_control = Bonsai_web_ui_partial_render_table.Focus_by_row in
            match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
            | ArrowDown | KeyJ -> Some (Focus_control.focus_down focus)
            | ArrowUp | KeyK -> Some (Focus_control.focus_up focus)
            | PageDown -> Some (Focus_control.page_down focus)
            | PageUp -> Some (Focus_control.page_up focus)
            | Escape -> Some (Focus_control.unfocus focus)
            | Home -> Some (Focus_control.focus_index focus 0)
            | End -> Some (Focus_control.focus_index focus num_filtered_rows)
            | _ -> None
          in
          match binding with
          | Some b -> Effect.Many [ Effect.Prevent_default; b ]
          | None -> Effect.Ignore)
      ]
    [ Vdom.Node.div [ Form.view_as_vdom form ]; table ]
;;

let () =
  let input = Value.return (Row.many_random 100_000) in
  component input
  |> View.Theme.set_for_app (Value.return (Kado.theme ~version:Bleeding ()))
  |> Bonsai_web.Start.start
;;
