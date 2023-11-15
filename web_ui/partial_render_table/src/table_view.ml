open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

(* This function takes a vdom node and if it's an element, it adds extra attrs, classes, key,
   and style info to it, but if it's not an element, it wraps that node in a div that has those
   attributes, styles, key, and style.  This can be useful if you get a vdom node from the
   user of this API, and want to avoid excessive node wrapping. *)
let set_or_wrap ~classes ~style =
  let open Vdom.Node in
  function
  | Element e -> Element (Element.add_style (Element.add_classes e classes) style)
  | other -> div ~attrs:[ Vdom.Attr.style style; Vdom.Attr.classes classes ] [ other ]
;;

let int_to_px_string px = Int.to_string px ^ "px"
let float_to_px_string px = Virtual_dom.Dom_float.to_string_fixed 8 px ^ "px"

module Header_label = struct
  let wrap_clickable ~sortable ~handle_click contents =
    let attrs = if sortable then [ Style.column_header; handle_click ] else [] in
    Vdom.Node.div ~attrs [ contents ]
  ;;

  let wrap_with_icon
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
        Vdom.Node.span
          ~attrs:[ Vdom.Attr.class_ "prt-sort-indicator" ]
          [ Vdom.Node.text indicator ]
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

    let leaf_view ~column_width ~set_column_width ~visible ~label () =
      Vdom.Node.td
        ~attrs:
          [ Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
              (fun ~width ~height:_ -> set_column_width (`Px_float width))
          ; Vdom.Attr.colspan 1
          ; Style.header_label
          ; Style.leaf_header
          ; Vdom.Attr.style
              Css_gen.(width column_width @> if visible then empty else display `None)
          ]
        [ label ]
    ;;

    let spacer_view ~colspan () = Vdom.Node.td ~attrs:[ attr_colspan colspan ] []

    let group_view ~colspan ~label () =
      Vdom.Node.td ~attrs:[ attr_colspan colspan; Style.header_label ] [ label ]
    ;;
  end

  module Header_row = struct
    type t = Vdom.Node.t

    let view contents = Vdom.Node.tr contents
  end

  type t = Vdom.Node.t

  (* Fun fact: the header is the only part of partial_render_table that is displayed
     as an actual HTML table! *)
  let view ~set_header_client_rect header_rows =
    Vdom.Node.table
      ~attrs:
        [ Bonsai_web_ui_element_size_hooks.Visibility_tracker.detect
            ()
            ~client_rect_changed:set_header_client_rect
        ; Style.partial_render_table_header
        ; Vdom.Attr.class_ "prt-table-header"
        ]
      [ Vdom.Node.tbody header_rows ]
  ;;
end

module Cell = struct
  module Col_styles = struct
    type t = Css_gen.t

    (* Css_gen is really slow, so we need to re-use the results of all these functions
       whenever possible.  The difference between non-cached and cached css is the
       difference between 200ms stabilizations and 0.2ms stabiliations while scrolling.

       The reason that Css_gen is so slow is because apparently "sprintf" is _really_
       slow. *)
    let create ~row_height ~col_widths ~cols_visible =
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
          create ~field:"height" ~value:h
          @> create ~field:"min-height" ~value:h
          @> create ~field:"max-height" ~value:h
          @> create ~field:"width" ~value:w
          @> create ~field:"min-width" ~value:w
          @> create ~field:"max-width" ~value:w
          @> if is_visible then Css_gen.empty else display `None)
        |> Array.of_list
      in
      fun i -> Array.get styles_arr i
    ;;
  end

  type t = Vdom.Node.t

  let cell_classes = [ "prt-table-cell"; Style.For_referencing.cell ]

  let view ~col_styles content =
    set_or_wrap content ~classes:cell_classes ~style:col_styles
  ;;

  let empty_content = Vdom.Node.div []
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

  let row_classes ~is_selected =
    let classes = [ "prt-table-row" ] in
    if is_selected then "prt-table-row-selected" :: classes else classes
  ;;

  let view ~styles ~is_selected ~on_row_click cells =
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.classes (row_classes ~is_selected)
        ; Vdom.Attr.style styles
        ; Vdom.Attr.on_click (fun _ -> on_row_click)
        ]
      cells
  ;;
end

module Body = struct
  type t = Vdom.Node.t

  let view_impl ~padding_top ~padding_bottom ~rows =
    let style =
      Vdom.Attr.style
        (Css_gen.concat
           [ Css_gen.padding_top (`Px padding_top)
           ; Css_gen.padding_bottom (`Px padding_bottom)
           ])
    in
    Vdom.Node.div ~attrs:[ style ] [ Vdom_node_with_map_children.make ~tag:"div" rows ]
  ;;

  let view ~padding_top ~padding_bottom ~rows =
    Vdom.Node.lazy_ (lazy (view_impl ~padding_top ~padding_bottom ~rows))
  ;;
end

module Table = struct
  let view ~private_body_classname ~vis_change_attr ~total_height head body =
    let body_container =
      Vdom.Node.div
      (* If the number is large enough, it will use scientific notation for unknown reasons.
           However, the number is accurate, and scientific notation is in spec.
           https://developer.mozilla.org/en-US/docs/Web/CSS/number *)
        ~attrs:
          [ Vdom.Attr.(
              many
                [ Style.partial_render_table_body
                ; class_ private_body_classname
                ; Vdom.Attr.style Css_gen.(height (`Px total_height))
                ; vis_change_attr
                ])
          ]
        [ body ]
    in
    Vdom.Node.div ~attrs:[ Style.partial_render_table_container ] [ head; body_container ]
  ;;
end
