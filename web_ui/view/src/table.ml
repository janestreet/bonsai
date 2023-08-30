open! Core
open! Import
module Constants = Constants
module Fg_bg = Constants.Fg_bg

module Raw = struct
  module Header_cell = struct
    type t = Theme.t -> Vdom.Node.t
  end

  module Header_row = struct
    type t = Theme.t -> Vdom.Node.t
  end

  module Data_row = struct
    type t = Theme.t -> Vdom.Node.t
  end

  module Data_cell = struct
    type t = Theme.t -> Vdom.Node.t
  end

  let combine_attrs default optional =
    match optional with
    | None -> default
    | Some attrs -> Vdom.Attr.many (default :: attrs)
  ;;

  let table_node ?attrs rows ((module T) : Theme.t) =
    Vdom.Node.table ~attrs:[ combine_attrs T.singleton#table attrs ] rows
  ;;

  let thead ?attrs rows ((module T) : Theme.t) =
    Vdom.Node.thead ~attrs:[ combine_attrs T.singleton#table_header attrs ] rows
  ;;

  let tbody ?attrs rows ((module T) : Theme.t) =
    Vdom.Node.tbody ~attrs:[ combine_attrs T.singleton#table_body attrs ] rows
  ;;

  let header_row ?attrs cells ((module T) as theme : Theme.t) =
    Vdom.Node.tr
      ~attrs:[ combine_attrs T.singleton#table_header_row attrs ]
      (List.map cells ~f:(fun cell -> cell theme))
  ;;

  let data_row ?attrs cells ((module T) as theme : Theme.t) =
    Vdom.Node.tr
      ~attrs:[ combine_attrs T.singleton#table_body_row attrs ]
      (List.map cells ~f:(fun cell -> cell theme))
  ;;

  let header_cell ?attrs rows ((module T) : Theme.t) =
    Vdom.Node.th ~attrs:[ combine_attrs T.singleton#table_header_cell attrs ] rows
  ;;

  let data_cell ?attrs rows ((module T) : Theme.t) =
    Vdom.Node.td ~attrs:[ combine_attrs T.singleton#table_body_cell attrs ] rows
  ;;

  let table theme ?(table_attrs = []) ~header_rows ~data_rows () =
    table_node
      ~attrs:table_attrs
      [ thead (List.map header_rows ~f:(fun row -> row theme)) theme
      ; tbody (List.map data_rows ~f:(fun row -> row theme)) theme
      ]
      theme
  ;;
end

module Col = struct
  type 'a t =
    | Group :
        { header : Vdom.Node.t option
        ; header_attrs : Vdom.Attr.t list
        ; children : 'a t list
        ; depth : int
        }
        -> 'a t
    | Leaf :
        { header : Vdom.Node.t
        ; get : 'a -> 'b option
        ; render : Theme.t -> 'b -> Vdom.Node.t
        ; cell_attrs : 'b -> Vdom.Attr.t list
        ; header_attrs : Vdom.Attr.t list
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

  let make_opt' ?(cell_attrs = fun _ -> []) ?(header_attrs = []) header ~get ~render =
    Leaf { header; get; render; cell_attrs; header_attrs }
  ;;

  let make_opt ?cell_attrs ?header_attrs header =
    make_opt' ?cell_attrs ?header_attrs (Vdom.Node.text header)
  ;;

  let make' ?cell_attrs ?header_attrs header ~get =
    let get x = Some (get x) in
    make_opt' ?cell_attrs ?header_attrs header ~get
  ;;

  let make ?cell_attrs ?header_attrs header ~get =
    make' ?cell_attrs ?header_attrs (Vdom.Node.text header) ~get
  ;;

  let empty_group col =
    Group { header = None; header_attrs = []; children = [ col ]; depth = depth col + 1 }
  ;;

  let group' ?(header_attrs = []) header children =
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
    Group { header = Some header; header_attrs; children; depth = max_depth + 1 }
  ;;

  let group ?header_attrs header children =
    group' ?header_attrs (Vdom.Node.text header) children
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
      | Group { header; header_attrs; _ } -> [ header, header_attrs, colspan col ]
      | Leaf { header; header_attrs; _ } -> [ Some header, header_attrs, 1 ]
      | Lift { t; _ } -> headers_at_level ~level t)
    else (
      match col with
      | Group { children; _ } -> List.bind children ~f:(headers_at_level ~level)
      | Leaf _ -> []
      | Lift { t; _ } -> headers_at_level ~level t)
  ;;

  let rec renderers : type a. a t -> (_ -> a -> _) list = function
    | Leaf { render; get; cell_attrs; _ } ->
      [ (fun theme x ->
          match get x with
          | None -> None
          | Some x -> Some (render theme x, cell_attrs x))
      ]
    | Group { children; _ } -> List.bind children ~f:renderers
    | Lift { t; f } ->
      List.map (renderers t) ~f:(fun render theme a -> render theme (f a))
  ;;
end

let render
  (((module T) : Theme.t) as theme)
  ?(table_attrs = [])
  ?(row_attrs = fun _ -> [])
  cols
  xs
  =
  let mega_group = Col.group' Vdom.Node.none cols in
  let depth = Col.depth mega_group in
  let body_cell_empty = T.singleton#table_body_cell_empty in
  let for_each_level level =
    Col.headers_at_level ~level mega_group
    |> List.filter_map ~f:(function
         (* empty groups have a colspan of 0, don't include them *)
         | _, _, 0 -> None
         | node, attrs, colspan ->
           let node = Option.value node ~default:Vdom.Node.none in
           let cell_attr = [ Vdom.Attr.colspan colspan; Vdom.Attr.many attrs ] in
           Some (Raw.header_cell ~attrs:cell_attr [ node ]))
    |> Raw.header_row
  in
  let header_rows = List.map (List.range ~stride:(-1) (depth - 1) 0) ~f:for_each_level in
  let renderers = Col.renderers mega_group in
  let for_each_row row =
    let extra_row_attrs = row_attrs row in
    renderers
    |> List.map ~f:(fun f ->
         match f theme row with
         | None -> Raw.data_cell ~attrs:[ body_cell_empty ] []
         | Some (cell, extra_cell_attrs) -> Raw.data_cell ~attrs:extra_cell_attrs [ cell ])
    |> Raw.data_row ~attrs:extra_row_attrs
  in
  let data_rows = List.map xs ~f:for_each_row in
  Raw.table theme ~table_attrs ~header_rows ~data_rows ()
;;

(* Default styling *)

module Default_table_styling =
[%css
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
      ~header_body_border:(Css_gen.Color.to_string_css constants.table.header_body_border)
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
