open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Bonsai_web_ui_partial_render_table

module Row = struct
  module T = struct
    type t =
      { symbol : string
      ; edge : float
      ; max_edge : float
      ; bsize : int
      ; bid : float
      ; ask : float
      ; asize : int
      }
    [@@deriving compare, fields ~fields, sexp, typed_fields]
  end

  include T
  include Comparator.Make (T)

  let of_int i =
    { symbol = [%string "JANE%{i#Int}"]
    ; edge = Float.of_int i
    ; max_edge = Float.of_int i
    ; bsize = i
    ; bid = Float.of_int i
    ; ask = Float.of_int i
    ; asize = i
    }
  ;;

  let init_rows n = List.init n ~f:(fun x -> x, of_int x) |> Map.of_alist_exn (module Int)
end

module type S = sig
  val all : (int, Row.t) Expert.Columns.t
end

module Dynamic_cells : S = struct
  module type S = sig
    type t [@@deriving compare]

    val to_string : t -> string
  end

  module Column = Expert.Columns.Dynamic_cells

  let column_helper
    (type a)
    (module M : S with type t = a)
    ?visible
    (field : (_, a) Field.t)
    =
    Column.column
      ?visible
      ~header:(Value.return (Vdom.Node.text (Fieldslib.Field.name field)))
      ~cell:(fun ~key:_ ~data ->
        (* This [state] is just here to de-optimize the dynamic-cells column. *)
        let%sub state, _ =
          Bonsai.state () ~sexp_of_model:[%sexp_of: Unit.t] ~equal:[%equal: Unit.t]
        in
        let%arr data = data
        and () = state in
        Vdom.Node.text (M.to_string (Field.get field data)))
      ()
  ;;

  let all =
    [ column_helper (module String) Row.Fields.symbol
    ; column_helper (module Float) Row.Fields.edge
    ; column_helper (module Float) Row.Fields.max_edge
    ; column_helper (module Int) Row.Fields.bsize
    ; column_helper (module Float) Row.Fields.bid
    ; column_helper (module Float) Row.Fields.ask
    ; column_helper (module Int) Row.Fields.asize
    ]
    |> Column.lift
  ;;
end

module Dynamic_columns : S = struct
  module type S = sig
    type t [@@deriving compare]

    val to_string : t -> string
  end

  module Column = Expert.Columns.Dynamic_columns

  let column_helper
    (type a)
    (module M : S with type t = a)
    ?visible
    (field : (_, a) Field.t)
    =
    Column.column
      ?visible
      ~header:(Vdom.Node.text (Fieldslib.Field.name field))
      ~cell:(fun ~key:_ ~data -> Vdom.Node.text (M.to_string (Field.get field data)))
      ()
  ;;

  let all =
    [ column_helper (module String) Row.Fields.symbol
    ; column_helper (module Float) Row.Fields.edge
    ; column_helper (module Float) Row.Fields.max_edge
    ; column_helper (module Int) Row.Fields.bsize
    ; column_helper (module Float) Row.Fields.bid
    ; column_helper (module Float) Row.Fields.ask
    ; column_helper (module Int) Row.Fields.asize
    ]
    |> Value.return
    |> Column.lift
  ;;
end

module Dynamic_experimental : S = struct
  module Column = Expert.Columns.Dynamic_experimental

  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end

  let render_header col =
    let%arr { Row.Typed_field.Packed.f = T field } = col in
    Vdom.Node.text (Row.Typed_field.name field)
  ;;

  let render_cell col _k row =
    let%arr { Row.Typed_field.Packed.f = T field } = col
    and row = row in
    let string, float, int =
      ( Vdom.Node.text
      , (fun x -> Vdom.Node.text (Float.to_string x))
      , fun x -> Vdom.Node.text (Int.to_string x) )
    in
    let value = Row.Typed_field.get field row in
    match field with
    | Symbol -> string value
    | Edge -> float value
    | Max_edge -> float value
    | Bsize -> int value
    | Bid -> float value
    | Ask -> float value
    | Asize -> int value
  ;;

  let all =
    Column.build
      (module Col_id)
      ~render_header
      ~render_cell
      ~columns:(Value.return Row.Typed_field.Packed.all)
  ;;
end
