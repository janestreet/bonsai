open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic

module Time_ns_option = struct
  type t = Time_ns.t option [@@deriving compare]

  let to_string = function
    | None -> "-"
    | Some t -> Time_ns.to_string_utc t
  ;;
end

module Columns = struct
  module type S = sig
    type t [@@deriving compare]

    val to_string : t -> string
  end

  module Column = Table.Columns.Dynamic_cells

  let column_helper
        (type a)
        (module M : S with type t = a)
        ?visible
        (field : (_, a) Field.t)
    =
    Column.column
      ?visible
      ~label:(Value.return (Vdom.Node.text (Fieldslib.Field.name field)))
      ~sort:
        (Value.return (fun (_, a) (_, b) ->
           M.compare (Field.get field a) (Field.get field b)))
      ~cell:(fun ~key:_ ~data ->
        return
        @@ let%map data = data in
        Vdom.Node.text (M.to_string (Field.get field data)))
      ()
  ;;

  let all ~should_show_position =
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
            [ column_helper (module Int) Row.Fields.position ~visible:should_show_position
            ]
        ; column_helper (module Time_ns_option) Row.Fields.last_fill
        ]
    ; column_helper (module String) Row.Fields.trader
    ]
    |> Column.lift
  ;;
end

let component (data : Row.t String.Map.t Value.t) =
  let%sub should_show_position, set =
    Bonsai.state [%here] (module Bool) ~default_model:true
  in
  let%sub table =
    Table.component
      (module String)
      ~row_height:(`Px 30)
      ~columns:(Columns.all ~should_show_position)
      data
  in
  return
  @@ let%map should_show_position = should_show_position
  and set = set
  and { Table.Result.view = table; for_testing = _ } = table in
  let button_text =
    if should_show_position then "hide position" else "show position"
  in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attr:(Vdom.Attr.on_click (fun _ -> set (not should_show_position)))
        [ Vdom.Node.text button_text ]
    ; table
    ]
;;

let (_ : _ Start.Handle.t) =
  let input = Value.return (Row.many_random 100_000) in
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (component input)
;;
