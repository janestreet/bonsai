open! Core
open! Import
module Constants = Constants
module Fg_bg = Constants.Fg_bg

module Col = struct
  type 'a t =
    | Group :
        { header : Vdom.Node.t option
        ; header_attr : Vdom.Attr.t
        ; children : 'a t list
        ; depth : int
        }
        -> 'a t
    | Leaf :
        { header : Vdom.Node.t
        ; get : 'a -> 'b option
        ; render : Theme.t -> 'b -> Vdom.Node.t
        ; cell_attr : 'b -> Vdom.Attr.t
        ; header_attr : Vdom.Attr.t
        }
        -> 'a t
    | Lift :
        { f : 'a -> 'b
        ; t : 'b t
        }
        -> 'a t

  let rec depth : type a. a t -> int = function
    | Leaf _ -> 1
    | Group { depth; _ } -> depth
    | Lift { t; _ } -> depth t
  ;;

  let lift t ~f = Lift { t; f }

  let make_opt'
        ?(cell_attr = fun _ -> Vdom.Attr.empty)
        ?(header_attr = Vdom.Attr.empty)
        header
        ~get
        ~render
    =
    Leaf { header; get; render; cell_attr; header_attr }
  ;;

  let make_opt ?cell_attr ?header_attr header =
    make_opt' ?cell_attr ?header_attr (Vdom.Node.text header)
  ;;

  let make' ?cell_attr ?header_attr header ~get =
    let get x = Some (get x) in
    make_opt' ?cell_attr ?header_attr header ~get
  ;;

  let make ?cell_attr ?header_attr header ~get =
    make' ?cell_attr ?header_attr (Vdom.Node.text header) ~get
  ;;

  let empty_group col =
    Group
      { header = None
      ; header_attr = Vdom.Attr.empty
      ; children = [ col ]
      ; depth = depth col + 1
      }
  ;;

  let group' ?(header_attr = Vdom.Attr.empty) header children =
    let max_depth =
      match List.max_elt (List.map children ~f:depth) ~compare:[%compare: int] with
      | None -> 0
      | Some max -> max
    in
    let children =
      List.map children ~f:(fun child ->
        let delta = max_depth - depth child in
        Fn.apply_n_times ~n:delta empty_group child)
    in
    Group { header = Some header; header_attr; children; depth = max_depth + 1 }
  ;;

  let group ?header_attr header children =
    group' ?header_attr (Vdom.Node.text header) children
  ;;

  let rec colspan : type a. a t -> int = function
    | Leaf _ -> 1
    | Lift { t; _ } -> colspan t
    | Group { children; _ } -> List.sum (module Int) children ~f:colspan
  ;;

  let rec headers_at_level : type a. level:_ -> a t -> _ =
    fun ~level col ->
      if depth col = level
      then (
        match col with
        | Group { header; header_attr; _ } -> [ header, header_attr, colspan col ]
        | Leaf { header; header_attr; _ } -> [ Some header, header_attr, 1 ]
        | Lift { t; _ } -> headers_at_level ~level t)
      else (
        match col with
        | Group { children; _ } -> List.bind children ~f:(headers_at_level ~level)
        | Leaf _ -> []
        | Lift { t; _ } -> headers_at_level ~level t)
  ;;

  let rec renderers : type a. a t -> (_ -> a -> _) list = function
    | Leaf { render; get; cell_attr; _ } ->
      [ (fun theme x ->
          match get x with
          | None -> None
          | Some x -> Some (render theme x, cell_attr x))
      ]
    | Group { children; _ } -> List.bind children ~f:renderers
    | Lift { t; f } ->
      List.map (renderers t) ~f:(fun render theme a -> render theme (f a))
  ;;
end

let render
      (((module T) : Theme.t) as theme)
      ?(table_attr = Vdom.Attr.empty)
      ?(row_attr = fun _ -> Vdom.Attr.empty)
      cols
      xs
  =
  let mega_group = Col.group' Vdom.Node.none cols in
  let depth = Col.depth mega_group in
  let header_cell_attr = T.singleton#table_header_cell in
  let header_row_attr = T.singleton#table_header_row in
  let body_cell_attr = T.singleton#table_body_cell in
  let body_row_attr = T.singleton#table_body_row in
  let body_cell_empty = T.singleton#table_body_cell_empty in
  let for_each_level level =
    Col.headers_at_level ~level mega_group
    |> List.filter_map ~f:(function
      (* empty groups have a colspan of 0, don't include them *)
      | _, _, 0 -> None
      | node, attr, colspan ->
        let node = Option.value node ~default:Vdom.Node.none in
        let cell_attr =
          Vdom.Attr.many [ Vdom.Attr.colspan colspan; header_cell_attr; attr ]
        in
        Some (Vdom.Node.th ~attr:cell_attr [ node ]))
    |> Vdom.Node.tr ~attr:header_row_attr
  in
  let header_rows = List.map (List.range ~stride:(-1) (depth - 1) 0) ~f:for_each_level in
  let renderers = Col.renderers mega_group in
  let for_each_row row =
    let extra_row_attr = row_attr row in
    renderers
    |> List.map ~f:(fun f ->
      match f theme row with
      | None ->
        Vdom.Node.td ~attr:(Vdom.Attr.many [ body_cell_attr; body_cell_empty ]) []
      | Some (cell, extra_cell_attr) ->
        Vdom.Node.td
          ~attr:(Vdom.Attr.many [ body_cell_attr; extra_cell_attr ])
          [ cell ])
    |> Vdom.Node.tr ~attr:(Vdom.Attr.many [ body_row_attr; extra_row_attr ])
  in
  let rows = List.map xs ~f:for_each_row in
  Vdom.Node.table
    ~attr:(Vdom.Attr.many [ T.singleton#table; table_attr ])
    [ Vdom.Node.thead ~attr:T.singleton#table_header header_rows
    ; Vdom.Node.tbody ~attr:T.singleton#table_body rows
    ]
;;

(* Default styling *)

module Default_table_styling =
  [%css.hash_variables
    stylesheet
      {|
.table {
  background-color: var(--bg);
  color: var(--fg);
  border-collapse: collapse;
}

.header {
  background-color: var(--header-bg);
  color: var(--header-fg);
}

.header_row { }

.header_cell {
  border: 1px solid var(--header-header-border, blue);
  font-weight: 300;
  font-size: 0.9em;
  padding: 0.3em 0.5em;
}

.header_row:last-child .header_cell {
  border-bottom:1px solid var(--header-body-border, red);
}

.body { }

.body_row:nth-child(even) {
  background: var(--row-even-bg);
  color: var(--row-even-fg);
}

.body_row:nth-child(odd) {
  background: var(--row-odd-bg);
  color: var(--row-odd-fg);
}

.body_cell {
  border: 1px solid var(--body-body-border, green);
}

.body_row:first-child .body_cell {
  border-top:unset;
}

.body_cell {
  padding: 0.3em 0.5em;
}

.body_cell.empty { }

.body_cell.empty::before {
  content: "";
  border-top: 1px solid var(--body-body-border);
  display: block;
  width: 100%;
}

|}]

let table_attr (constants : Constants.t) =
  let vars =
    Default_table_styling.Variables.set
      ~bg:(Css_gen.Color.to_string_css constants.primary.background)
      ~fg:(Css_gen.Color.to_string_css constants.primary.foreground)
      ~header_bg:(Css_gen.Color.to_string_css constants.table.header_row.background)
      ~header_fg:(Css_gen.Color.to_string_css constants.table.header_row.foreground)
      ~row_even_bg:(Css_gen.Color.to_string_css constants.table.body_row_even.background)
      ~row_even_fg:(Css_gen.Color.to_string_css constants.table.body_row_even.foreground)
      ~row_odd_bg:(Css_gen.Color.to_string_css constants.table.body_row_odd.background)
      ~row_odd_fg:(Css_gen.Color.to_string_css constants.table.body_row_odd.foreground)
      ~header_header_border:
        (Css_gen.Color.to_string_css constants.table.header_header_border)
      ~body_body_border:(Css_gen.Color.to_string_css constants.table.body_body_border)
      ~header_body_border:(Css_gen.Color.to_string_css constants.table.body_body_border)
      ()
  in
  Vdom.Attr.many [ vars; Default_table_styling.table ]
;;

let table_header_attr = Default_table_styling.header
let table_header_row = Default_table_styling.header_row
let table_header_cell = Default_table_styling.header_cell
let table_body_attr = Default_table_styling.body
let table_body_row = Default_table_styling.body_row
let table_body_cell = Default_table_styling.body_cell
let table_body_cell_empty = Default_table_styling.empty
