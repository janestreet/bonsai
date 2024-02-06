open! Core
open! Import

type t =
  { header_cell : Vdom.Attr.t
  ; header_row : Vdom.Attr.t
  ; header : Vdom.Attr.t
  ; cell : Vdom.Attr.t
  ; cell_focused : Vdom.Attr.t
  ; row : Vdom.Attr.t
  ; row_focused : Vdom.Attr.t
  ; body : Vdom.Attr.t
  ; table : Vdom.Attr.t
  }

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
  border: 1px solid var(--header-header-border);
  text-align: center;
  font-weight: bold;
  font-size: 0.9em;
  padding: 0.3em 0.5em;
}

.header_row:last-child .header_cell {
  border-bottom:1px solid var(--header-body-border);
}

.body { }

.body_cell {
  padding: 0.3em 0.5em;
  font-size: 0.8em;
}


/* Backgrounds */
.body_row:nth-child(even) {
  background: var(--row-even-bg);
  color: var(--row-even-fg);
}

.body_row:nth-child(odd) {
  background: var(--row-odd-bg);
  color: var(--row-odd-fg);
}

.body_row.body_row_focused {
  background: var(--row-focused-bg);
  color: var(--row-focused-fg);
}

.body_cell.body_cell_focused {
  background: var(--cell-focused-bg);
  color: var(--cell-focused-fg);
}

/* Borders. Probably due to a browser bug, if we use full borders
  AND contain:paint (subset of strict) in PRT, we'll get weird, glitchy double borders.
  Instead, cells/rows (except for the last cell/row) only paint their top/left borders.
*/
.body_row {
  border-width: 1px 0 0 0;
}

.body_row:last-child {
  border-width: 1px 0 1px 0;
}

.body_cell {
  border-width: 0 0 0 1px;
}

.body_cell:last-child {
  border-width: 0 1px 0 1px;
}

.body_row, .body_cell {
  border-style: solid;
  border-color: var(--body-body-border);
}

.body_row.body_row_focused,
.body_row_focused > .body_cell {
  border-color: var(--row-focused-border);
}

/* `.body_row:has(+ .body_row_focused)`, or other uses of the css `+` selector applied
to rows, result in a massively slow "Recalculate Styles" step.
For some reason, this does not have the same performance problem.
This is likely a chrome bug. */
.body_row_focused ~ .body_row {
  border-color: var(--row-focused-border);
}

.body_row_focused ~ .body_row ~ .body_row {
  border-color: var(--body-body-border);
}
.body_row_focused ~ .body_row:last-child {
  border-bottom-color: var(--body-body-border);
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
      ~cell_focused_bg:
        (Css_gen.Color.to_string_css constants.table.body_cell_focused.background)
      ~cell_focused_fg:
        (Css_gen.Color.to_string_css constants.table.body_cell_focused.foreground)
      ~row_focused_bg:
        (Css_gen.Color.to_string_css constants.table.body_row_focused.background)
      ~row_focused_fg:
        (Css_gen.Color.to_string_css constants.table.body_row_focused.foreground)
      ~row_focused_border:
        (Css_gen.Color.to_string_css constants.table.body_row_focused_border)
      ~header_header_border:
        (Css_gen.Color.to_string_css constants.table.header_header_border)
      ~body_body_border:(Css_gen.Color.to_string_css constants.table.body_body_border)
      ~header_body_border:(Css_gen.Color.to_string_css constants.table.header_body_border)
      ()
  in
  Vdom.Attr.many [ vars; Default_table_styling.table ]
;;

let default constants =
  { header_cell = Default_table_styling.header_cell
  ; header_row = Default_table_styling.header_row
  ; header = Default_table_styling.header
  ; cell = Default_table_styling.body_cell
  ; cell_focused = Default_table_styling.body_cell_focused
  ; row = Default_table_styling.body_row
  ; row_focused = Default_table_styling.body_row_focused
  ; body = Default_table_styling.body
  ; table = table_attr constants
  }
;;
