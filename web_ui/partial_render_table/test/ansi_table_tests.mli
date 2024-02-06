open! Core
open! Bonsai_web
module Table = Bonsai_web_ui_partial_render_table

val table_to_string
  :  include_stats:bool
  -> ?include_num_column:bool
  -> ?selected_header:string
  -> ?additional_summary:string
  -> Table.For_testing.t
  -> unit
  -> string
