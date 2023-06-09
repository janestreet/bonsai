open! Core
open! Bonsai_web
module Sort_state := Bonsai_web_ui_partial_render_table_protocol.Sort_state

module Sort_kind : sig
  type ('key, 'data) sort := 'key * 'data -> 'key * 'data -> int

  type ('key, 'data) t =
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
  val lift : ('key, 'data) t list -> ('key, 'data) Column_intf.t

  module Header_helpers : Column_intf.Header_helpers
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
  val lift : ('key, 'data) t list Value.t -> ('key, 'data) Column_intf.t

  module Header_helpers : Column_intf.Header_helpers
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
  val lift : ('key, 'data) t list -> ('key, 'data) Column_intf.with_sorter

  module Header_helpers : Column_intf.Header_helpers
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
  val lift : ('key, 'data) t list Value.t -> ('key, 'data) Column_intf.with_sorter

  module Header_helpers : Column_intf.Header_helpers
end
