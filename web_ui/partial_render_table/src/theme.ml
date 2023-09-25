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

  type t =
    { col_styles : Col_styles.t
    ; content : Vdom.Node.t
    }

  let cell_classes = [ "prt-table-cell"; Style.For_referencing.cell ]

  let view { col_styles; content } =
    set_or_wrap content ~classes:cell_classes ~style:col_styles
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

  let row_classes ~is_selected =
    let classes = [ "prt-table-row" ] in
    if is_selected then "prt-table-row-selected" :: classes else classes
  ;;

  let view ~styles ~is_selected ~on_row_click ~col_styles ~cell_contents =
    let cell_params =
      List.mapi cell_contents ~f:(fun i content ->
        { Cell.col_styles = col_styles i; content })
    in
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.classes (row_classes ~is_selected)
        ; Vdom.Attr.style styles
        ; Vdom.Attr.on_click (fun _ -> on_row_click)
        ]
      (List.map cell_params ~f:Cell.view)
  ;;
end

module Body = struct
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
