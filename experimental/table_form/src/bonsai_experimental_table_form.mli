open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form

module Column : sig
  type t

  val create : ?initial_width:[ `Px of int ] -> string -> t
end

(** Builds a table where each row in the table is built out of the constituent form
    elements for a record built from the ['a Form.t Computation.t] parameter.

    Adding a new row is done by filling out a textbox with the sexp-representation of that
    key and either pressing [ENTER] or hitting the "+" button.

    Removing a row is done by hitting the [x] button in that rows "key" column.

    You also need to pass the names for the columns in the same order as they would have
    appeared in the standard rendering of the Form.t *)
val table_form
  :  ?key_column_initial_width:[ `Px of int ]
  -> ('key, 'cmp) Bonsai.comparator
  -> 'a Form.t Computation.t
  -> columns:Column.t list Value.t
  -> ('key, 'a, 'cmp) Base.Map.t Form.t Computation.t
