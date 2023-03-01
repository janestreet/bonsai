open! Core
open! Bonsai_web
module Table = Bonsai_web_ui_partial_render_table

val table_to_string
  :  include_stats:bool
  -> ?include_num_column:bool
  -> ?selected_header:string
  -> ?num_filtered_rows:int
  -> ('a, int option) Table.Focus_by_row.t option
  -> Table.For_testing.t
  -> unit
  -> string
