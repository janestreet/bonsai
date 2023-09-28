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

module Style =
[%css
stylesheet
  {|
  .form_container {
    position: fixed;
    top: 0px;
    right: 0px;
    z-index: 9000;
  }
|}]

module type S = sig
  type t [@@deriving compare]

  val to_string : t -> string
end

module Column = Table.Columns.Dynamic_cells

let column_helper
  (type a)
  (module M : S with type t = a)
  ?(disable_sort = false)
  ?visible
  (field : (_, a) Field.t)
  =
  let sort =
    if disable_sort
    then None
    else
      Some
        (Value.return (fun (_, a) (_, b) ->
           M.compare (Field.get field a) (Field.get field b)))
  in
  let render_header text =
    Value.return (Column.Header_helpers.default (Vdom.Node.text text))
  in
  Column.column
    ?visible
    ~header:(render_header (Fieldslib.Field.name field))
    ?sort
    ~cell:(fun ~key:_ ~data ->
      let%arr data = data in
      Vdom.Node.text (M.to_string (Field.get field data)))
    ()
;;

let special_compare_option how compare_inner a b =
  match a, b with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some a, Some b ->
    (match how with
     | `Ascending -> compare_inner a b
     | `Descending -> -compare_inner a b)
;;

let columns ~should_show_position =
  let render_header text =
    Value.return (Column.Header_helpers.default (Vdom.Node.text text))
  in
  [ column_helper (module String) Row.Fields.symbol
  ; column_helper (module Float) Row.Fields.edge
  ; column_helper (module Float) Row.Fields.max_edge
  ; column_helper (module Int) Row.Fields.bsize
  ; column_helper (module Float) Row.Fields.bid
  ; column_helper (module Float) Row.Fields.ask
  ; column_helper (module Int) Row.Fields.asize
  ; Column.group
      ~label:(Value.return (Vdom.Node.text "some group"))
      [ Column.group
          ~label:(Value.return (Vdom.Node.text "small"))
          [ column_helper
              (module Int)
              Row.Fields.position
              ~disable_sort:true
              ~visible:should_show_position
          ]
      ; Column.column
          ~header:(render_header "last fill")
          ~sort:
            (Value.return (fun (_key1, a) (_key2, b) ->
               special_compare_option
                 `Ascending
                 [%compare: Time_ns.t]
                 a.Row.last_fill
                 b.Row.last_fill))
          ~sort_reversed:
            (Value.return (fun (_key1, a) (_key2, b) ->
               special_compare_option
                 `Descending
                 [%compare: Time_ns.t]
                 a.Row.last_fill
                 b.Row.last_fill))
          ~cell:(fun ~key:_ ~data ->
            let%arr data = data in
            Vdom.Node.text (Time_ns_option.to_string data.Row.last_fill))
          ()
      ]
  ; column_helper (module String) Row.Fields.trader
  ]
  |> Column.lift
;;

let component ?filter (data : Row.t String.Map.t Value.t) =
  let module Params = struct
    type t =
      { show_position : bool
      ; row_height : [ `Px of int ]
      }
    [@@deriving typed_fields]

    let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
      | Show_position -> Form.Elements.Toggle.bool ~default:true ()
      | Row_height ->
        let%sub form = Form.Elements.Range.int ~min:0 ~max:100 ~step:1 () ~default:30 in
        let%arr form = form in
        Form.project form ~parse_exn:(fun x -> `Px x) ~unparse:(fun (`Px x) -> x)
    ;;

    let label_for_field = `Inferred
  end
  in
  let%sub form = Form.Typed.Record.make (module Params) in
  let%sub { row_height; show_position = should_show_position } =
    let%arr form = form in
    Form.value_or_default form ~default:{ show_position = true; row_height = `Px 30 }
  in
  let%sub table =
    Table.component
      (module String)
      ?filter
      ~focus:(By_row { on_change = Value.return (Fn.const Effect.Ignore) })
      ~row_height
      ~columns:(columns ~should_show_position)
      data
  in
  let%arr { Table.Result.view = table
          ; for_testing = _
          ; sortable_header = _
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
            let module Focus_control = Table.Focus.By_row in
            match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
            | ArrowDown | KeyJ -> Some (Focus_control.focus_down focus)
            | ArrowUp | KeyK -> Some (Focus_control.focus_up focus)
            | PageDown -> Some (Focus_control.page_down focus)
            | PageUp -> Some (Focus_control.page_up focus)
            | Escape -> Some (Focus_control.unfocus focus)
            | Home -> Some ((Focus_control.focus_index focus) 0)
            | End -> Some ((Focus_control.focus_index focus) num_filtered_rows)
            | _ -> None
          in
          match binding with
          | Some b -> Effect.Many [ Effect.Prevent_default; b ]
          | None -> Effect.Ignore)
      ]
    [ Vdom.Node.div ~attrs:[ Style.form_container ] [ Form.view_as_vdom form ]; table ]
;;
