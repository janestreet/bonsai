open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Theming = struct
  type t =
    [ `Legacy_don't_use_theme
    | `Themed
    ]
end

module Themed = struct
  type t =
    { header_cell : Vdom.Attr.t
    ; header_row : Vdom.Attr.t
    ; header : Vdom.Attr.t
    ; cell : Vdom.Attr.t
    ; row : Vdom.Attr.t
    ; row_focused : Vdom.Attr.t
    ; body : Vdom.Attr.t
    ; table : Vdom.Attr.t
    }

  module Legacy_style =
  [%css
  stylesheet {|
  .header_cell {
    text-align: center;
    font-weight: bold;
  }|}]

  module Prt_view = Bonsai_web_ui_view.For_components.Prt

  let create theme = function
    | `Legacy_don't_use_theme ->
      { header_cell = Legacy_style.header_cell
      ; header_row = Vdom.Attr.empty
      ; header = Vdom.Attr.class_ "prt-table-header"
      ; cell = Vdom.Attr.class_ "prt-table-cell"
      ; row = Vdom.Attr.class_ "prt-table-row"
      ; row_focused = Vdom.Attr.class_ "prt-table-row-selected"
      ; body = Vdom.Attr.empty
      ; table = Vdom.Attr.empty
      }
    | `Themed ->
      let styling = Prt_view.styling theme in
      { header_cell = styling.header_cell
      ; header_row = styling.header_row
      ; header = styling.header
      ; cell = styling.cell
      ; row = styling.row
      ; row_focused = styling.row_focused
      ; body = styling.body
      ; table = styling.table
      }
  ;;
end

(* These styles make the table functional and interactive;
   they are applied regardless of theme. *)
module Functional_style =
[%css
stylesheet
  {|

/* The default value for the [overflow-anchor] CSS property is [auto], which
   permits the browser to scroll the page in order to minimize content shifts.
   This interacts poorly with the PRT because our virtual-dom diff-and-patch
   algorithm often removes and re-inserts elements. To fix this, we disable
   overflow-anchor for all elements that contain a partial render table. */
:has(.partial_render_table_container) {
  overflow-anchor: none;
}

.partial_render_table_container {
  width: max-content;
  position: relative;
}

.partial_render_table_container * {
  box-sizing: border-box;
}

.partial_render_table_body {
  position: relative;
}

.sortable_header_cell {
  white-space: pre;
  cursor: pointer;
}

.header_label {
  user-select: none;
}

.leaf_header {
  resize: horizontal;
  overflow: hidden;
  box-sizing: border-box;
  padding-right:10px; /* Space for the resizer */
}

.partial_render_table_header {
  position: sticky;
  top: 0px;
  z-index: 99;
  border-collapse: collapse;
}

.row {
  contain: strict;
}

.cell {
  overflow:hidden;
  display:inline-block;
  contain: strict;
}
|}]

(* This function takes a vdom node and if it's an element, it adds extra attrs, classes, key,
   and style info to it, but if it's not an element, it wraps that node in a div that has those
   attributes.  This can be useful if you get a vdom node from the
   user of this API, and want to avoid excessive node wrapping. *)
let set_or_wrap ~attrs =
  let open Vdom.Node in
  function
  | Element e -> Element (Element.map_attrs e ~f:(fun a -> Vdom.Attr.(a @ many attrs)))
  | other -> div ~attrs [ other ]
;;

let int_to_px_string px = Int.to_string px ^ "px"
let float_to_px_string px = Virtual_dom.Dom_float.to_string_fixed 8 px ^ "px"

module Header_label = struct
  let wrap_clickable ~sortable ~handle_click contents =
    let attrs =
      if sortable then [ Functional_style.sortable_header_cell; handle_click ] else []
    in
    Vdom.Node.div ~attrs [ contents ]
  ;;

  (* As an externally exposed component with no prior style overrides,
     we don't allow opting out of theming to keep user code simpler. *)
  let wrap_with_icon
    ?(sort_indicator_attrs = [])
    (label : Vdom.Node.t)
    (sort_state : Bonsai_web_ui_partial_render_table_protocol.Sort_state.t)
    =
    match sort_state with
    | Not_sortable -> Vdom.Node.div [ Vdom.Node.span [ label ] ]
    | _ ->
      let get_arrow = function
        | `Asc -> "▲"
        | `Desc -> "▼"
      in
      let sort_indicator =
        let%map.Option indicator =
          match sort_state with
          | Not_sortable | Not_sorted -> None
          | Single_sort dir -> Some (get_arrow dir)
          | Multi_sort { dir; index } -> Some [%string "%{get_arrow dir} %{index#Int}"]
        in
        Vdom.Node.span ~attrs:sort_indicator_attrs [ Vdom.Node.text indicator ]
      in
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.style
              (Css_gen.flex_container ~column_gap:(`Px 6) ~align_items:`Baseline ())
          ]
        [ Vdom.Node.span [ label ]
        ; sort_indicator |> Option.value ~default:Vdom.Node.none
        ]
  ;;
end

module Header = struct
  let attr_colspan i =
    match i with
    | 0 -> Vdom.Attr.style (Css_gen.display `None)
    | 1 -> Vdom.Attr.empty
    | i -> Vdom.Attr.create_float "colspan" (Int.to_float i)
  ;;

  module Header_cell = struct
    type t = Vdom.Node.t

    let leaf_view
      (themed_attrs : Themed.t)
      ~column_width
      ~set_column_width
      ~visible
      ~label
      ()
      =
      Vdom.Node.td
        ~attrs:
          [ themed_attrs.header_cell
          ; Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
              (fun ~width ~height:_ -> set_column_width (`Px_float width))
          ; Vdom.Attr.colspan 1
          ; Functional_style.header_label
          ; Functional_style.leaf_header
          ; Vdom.Attr.style
              Css_gen.(width column_width @> if visible then empty else display `None)
          ]
        [ label ]
    ;;

    let spacer_view (themed_attrs : Themed.t) ~colspan () =
      Vdom.Node.td ~attrs:[ themed_attrs.header_cell; attr_colspan colspan ] []
    ;;

    let group_view (themed_attrs : Themed.t) ~colspan ~label () =
      Vdom.Node.td
        ~attrs:
          [ themed_attrs.header_cell
          ; attr_colspan colspan
          ; Functional_style.header_label
          ]
        [ label ]
    ;;
  end

  module Header_row = struct
    type t = Vdom.Node.t

    let view (themed_attrs : Themed.t) contents =
      Vdom.Node.tr ~attrs:[ themed_attrs.header_row ] contents
    ;;
  end

  type t = Vdom.Node.t

  (* Fun fact: the header is the only part of partial_render_table that is displayed
     as an actual HTML table! *)
  let view (themed_attrs : Themed.t) ~set_header_client_rect header_rows =
    Vdom.Node.table
      ~attrs:
        [ themed_attrs.header
        ; Bonsai_web_ui_element_size_hooks.Visibility_tracker.detect
            ()
            ~client_rect_changed:set_header_client_rect
        ; Functional_style.partial_render_table_header
        ]
      [ Vdom.Node.tbody header_rows ]
  ;;
end

module Cell = struct
  module Col_styles = struct
    type t = Vdom.Attr.t list

    (* Css_gen is really slow, so we need to re-use the results of all these functions
       whenever possible.  The difference between non-cached and cached css is the
       difference between 200ms stabilizations and 0.2ms stabiliations while scrolling.

       The reason that Css_gen is so slow is because apparently "sprintf" is _really_
       slow. *)
    let create ~(themed_attrs : Themed.t) ~row_height ~col_widths ~cols_visible =
      let styles_arr =
        List.map2_exn col_widths cols_visible ~f:(fun width is_visible ->
          (* We use the previous width even when hidden, so that the rendering engine has
             less work to do if re-adding a column. Columns that are not currently visible
             are hidden via `display: None`. *)
          let width =
            match width with
            | `Visible w | `Hidden w -> w
          in
          let h = int_to_px_string row_height in
          let w = float_to_px_string width in
          let open Css_gen in
          (create ~field:"height" ~value:h
           @> create ~field:"min-height" ~value:h
           @> create ~field:"max-height" ~value:h
           @> create ~field:"width" ~value:w
           @> create ~field:"min-width" ~value:w
           @> create ~field:"max-width" ~value:w
           @> if is_visible then Css_gen.empty else display `None)
          |> fun x -> [ themed_attrs.cell; Vdom.Attr.style x ])
        |> Array.of_list
      in
      fun i -> Array.get styles_arr i
    ;;
  end

  type t = Vdom.Node.t

  let view ~col_styles content =
    set_or_wrap content ~attrs:(col_styles @ [ Functional_style.cell ])
  ;;
end

module Row = struct
  module Styles = struct
    type t = Css_gen.t

    let create ~row_height ~row_width =
      let h = int_to_px_string row_height in
      let w = float_to_px_string row_width in
      let open Css_gen in
      create ~field:"height" ~value:h
      @> create ~field:"width" ~value:w
      @> flex_container ()
    ;;
  end

  type t = Vdom.Node.t

  let view (themed_attrs : Themed.t) ~styles ~is_focused ~on_row_click cells =
    let focused_attr = if is_focused then themed_attrs.row_focused else Vdom.Attr.empty in
    Vdom.Node.div
      ~attrs:
        [ themed_attrs.row
        ; Vdom.Attr.style styles
        ; focused_attr
        ; Vdom.Attr.on_click (fun _ -> on_row_click)
        ; Functional_style.row
        ]
      cells
  ;;
end

module Body = struct
  type t = Vdom.Node.t

  let view_impl (themed_attrs : Themed.t) ~padding_top ~padding_bottom ~rows =
    let style =
      Vdom.Attr.style
        (Css_gen.concat
           [ Css_gen.padding_top (`Px padding_top)
           ; Css_gen.padding_bottom (`Px padding_bottom)
           ])
    in
    Vdom.Node.div
      ~attrs:[ themed_attrs.body; style ]
      [ Vdom_node_with_map_children.make ~tag:"div" rows ]
  ;;

  let view themed_attrs ~padding_top ~padding_bottom ~rows =
    Vdom.Node.lazy_ (lazy (view_impl themed_attrs ~padding_top ~padding_bottom ~rows))
  ;;
end

module Table = struct
  let view
    (themed_attrs : Themed.t)
    ~private_body_classname
    ~vis_change_attr
    ~total_height
    head
    body
    =
    let body_container =
      Vdom.Node.div
      (* If the number is large enough, it will use scientific notation for unknown reasons.
           However, the number is accurate, and scientific notation is in spec.
           https://developer.mozilla.org/en-US/docs/Web/CSS/number *)
        ~attrs:
          [ Vdom.Attr.(
              many
                [ Functional_style.partial_render_table_body
                ; class_ private_body_classname
                ; Vdom.Attr.style Css_gen.(height (`Px total_height))
                ; vis_change_attr
                ])
          ]
        [ body ]
    in
    Vdom.Node.div
      ~attrs:[ themed_attrs.table; Functional_style.partial_render_table_container ]
      [ head; body_container ]
  ;;
end
