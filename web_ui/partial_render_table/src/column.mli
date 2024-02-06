open! Core
open! Bonsai_web
module Sort_state := Bonsai_web_ui_partial_render_table_protocol.Sort_state

module Indexed_column_id : sig
  type t [@@deriving equal, sexp]

  val to_int : t -> int
  val of_int : int -> t
end

module Sort_kind : sig
  type ('key, 'data) sort := 'key * 'data -> 'key * 'data -> int

  type ('key, 'data) t = ('key, 'data) Column_intf.Sort_kind.t =
    { forward : ('key, 'data) sort
    ; reverse : ('key, 'data) sort
    }
end

module Dynamic_cells : sig
  type ('key, 'data) t

  val column
    :  ?initial_width:Css_gen.Length.t
    -> ?visible:bool Value.t
    -> header:Vdom.Node.t Value.t
    -> cell:(key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t Value.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t Value.t -> ('key, 'data) t -> ('key, 'data) t
  val lift : ('key, 'data) t list -> ('key, 'data, Indexed_column_id.t) Column_intf.t

  module Sortable = Sortable
end

module Dynamic_columns : sig
  type ('key, 'data) t

  val column
    :  ?initial_width:Css_gen.Length.t
    -> ?visible:bool
    -> header:Vdom.Node.t
    -> cell:(key:'key -> data:'data -> Vdom.Node.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t -> ('key, 'data) t -> ('key, 'data) t

  val lift
    :  ('key, 'data) t list Value.t
    -> ('key, 'data, Indexed_column_id.t) Column_intf.t

  module Sortable = Sortable
end

module Dynamic_experimental : sig
  val build
    :  ('column_id, _) Bonsai.comparator
    -> columns:'column_id list Value.t
    -> render_header:('column_id Value.t -> Vdom.Node.t Computation.t)
    -> render_cell:
         ('column_id Value.t
          -> 'key Value.t
          -> 'data Value.t
          -> Vdom.Node.t Computation.t)
    -> ('key, 'data, 'column_id) Column_intf.t

  module Sortable = Sortable
end

module Dynamic_cells_with_sorter : sig
  type ('key, 'data) t

  val column
    :  ?sort:('key * 'data -> 'key * 'data -> int) Value.t
    -> ?sort_reversed:('key * 'data -> 'key * 'data -> int) Value.t
    -> ?initial_width:Css_gen.Length.t
    -> ?visible:bool Value.t
    -> header:(Sort_state.t -> Vdom.Node.t) Value.t
    -> cell:(key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t Value.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t Value.t -> ('key, 'data) t -> ('key, 'data) t

  val lift
    :  ('key, 'data) t list
    -> ('key, 'data, Indexed_column_id.t) Column_intf.with_sorter

  module Sortable = Sortable
end

module Dynamic_columns_with_sorter : sig
  type ('key, 'data) t

  val column
    :  ?sort:('key * 'data -> 'key * 'data -> int)
    -> ?sort_reversed:('key * 'data -> 'key * 'data -> int)
    -> ?initial_width:Css_gen.Length.t
    -> ?visible:bool
    -> header:(Sort_state.t -> Vdom.Node.t)
    -> cell:(key:'key -> data:'data -> Vdom.Node.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t -> ('key, 'data) t -> ('key, 'data) t

  val lift
    :  ('key, 'data) t list Value.t
    -> ('key, 'data, Indexed_column_id.t) Column_intf.with_sorter

  module Sortable = Sortable
end

module Dynamic_experimental_with_sorter : sig
  val build
    :  ?sorts:('column_id Value.t -> ('key, 'data) Sort_kind.t option Computation.t)
    -> ('column_id, _) Bonsai.comparator
    -> columns:'column_id list Value.t
    -> render_header:('column_id Value.t -> (Sort_state.t -> Vdom.Node.t) Computation.t)
    -> render_cell:
         ('column_id Value.t
          -> 'key Value.t
          -> 'data Value.t
          -> Vdom.Node.t Computation.t)
    -> ('key, 'data, 'column_id) Column_intf.with_sorter

  module Sortable = Sortable
end
