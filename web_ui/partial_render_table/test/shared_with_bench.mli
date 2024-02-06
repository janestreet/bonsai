open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Table := Bonsai_web_ui_partial_render_table

module Row : sig
  type t =
    { symbol : string
    ; edge : float
    ; max_edge : float
    ; bsize : int
    ; bid : float
    ; ask : float
    ; asize : int
    }

  include Comparator.S with type t := t

  val of_int : int -> t
  val init_rows : int -> t Int.Map.t
end

module type S = sig
  type column_id

  val first_column : column_id
  val all : (int, Row.t, column_id) Table.Expert.Columns.t
end

module Dynamic_cells : S
module Dynamic_columns : S
module Dynamic_experimental : S
