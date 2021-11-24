open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic
module Row = Row

module Time_ns_option = struct
  type t = Time_ns.t option [@@deriving compare]

  let to_string = function
    | None -> "-"
    | Some t -> Time_ns.to_string_utc t
  ;;
end

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
  Column.column
    ?visible
    ~label:(Value.return (Vdom.Node.text (Fieldslib.Field.name field)))
    ?sort
    ~cell:(fun ~key:_ ~data ->
      return
      @@ let%map data = data in
      Vdom.Node.text (M.to_string (Field.get field data)))
    ()
;;

let columns ~should_show_position =
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
      ; column_helper (module Time_ns_option) Row.Fields.last_fill
      ]
  ; column_helper (module String) Row.Fields.trader
  ]
  |> Column.lift
;;

let component ?filter (data : Row.t String.Map.t Value.t) =
  let%sub should_show_position, set =
    Bonsai.state [%here] (module Bool) ~default_model:true
  in
  let%sub table =
    Table.component
      (module String)
      ?filter
      ~focus:(By_row { on_change = Value.return (Fn.const Effect.Ignore) })
      ~row_height:(`Px 30)
      ~columns:(columns ~should_show_position)
      data
  in
  return
  @@ let%map should_show_position = should_show_position
  and set = set
  and { Table.Result.view = table; for_testing = _; focus } = table in
  let button_text =
    if should_show_position then "hide position" else "show position"
  in
  let button text action =
    Vdom.Node.button
      ~attr:(Vdom.Attr.on_click (fun _ -> action))
      [ Vdom.Node.text text ]
  in
  Vdom.Node.div
    ~attr:
      (Vdom.Attr.on_keydown (fun kbc ->
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
         | Some b -> Ui_effect.Many [ Vdom.Effect.Prevent_default; b ]
         | None -> Vdom.Effect.Ignore))
    [ Vdom.Node.div
        ~attr:(Vdom.Attr.style Css_gen.(position ~top:(`Px 0) `Fixed @> z_index 9000))
        [ button button_text (set (not should_show_position)) ]
    ; table
    ]
;;
