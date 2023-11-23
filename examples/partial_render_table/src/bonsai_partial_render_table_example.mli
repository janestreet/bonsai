open! Core
open! Bonsai_web
module Row = Row

val table_and_focus_attr
  :  ?filter:(key:string -> data:Row.t -> bool) Value.t
  -> row_height:[ `Px of int ] Value.t
  -> theming:[ `Legacy_don't_use_theme | `Themed ]
  -> should_show_position:bool Value.t
  -> (string, Row.t, Base.String.comparator_witness) Base.Map.t Value.t
  -> (Vdom.Node.t * Vdom.Attr.t) Computation.t

module Layout_form : sig
  module Params : sig
    type t =
      { themed : bool
      ; show_position : bool
      ; row_height : [ `Px of int ]
      ; num_rows : int
      }
  end

  val component : (Vdom.Node.t * Params.t) Computation.t
end
