open! Core
open! Bonsai_web

module Dynamic_cells : sig
  type ('key, 'data) t

  val column
    :  ?initial_width:[ `Px of int ]
    -> ?visible:bool Value.t
    -> label:Vdom.Node.t Value.t
    -> cell:(key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t Value.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t Value.t -> ('key, 'data) t -> ('key, 'data) t
  val lift : ('key, 'data) t list -> ('key, 'data) Column_intf.t
end

module Dynamic_columns : sig
  type ('key, 'data) t

  val column
    :  ?initial_width:[ `Px of int ]
    -> ?visible:bool
    -> label:Vdom.Node.t
    -> cell:(key:'key -> data:'data -> Vdom.Node.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t -> ('key, 'data) t -> ('key, 'data) t
  val lift : ('key, 'data) t list Value.t -> ('key, 'data) Column_intf.t
end

module Dynamic_cells_with_sorter : sig
  type ('key, 'data) t

  val column
    :  ?sort:('key * 'data -> 'key * 'data -> int) Value.t
    -> ?initial_width:[ `Px of int ]
    -> ?visible:bool Value.t
    -> label:Vdom.Node.t Value.t
    -> cell:(key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t Value.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t Value.t -> ('key, 'data) t -> ('key, 'data) t
  val lift : ('key, 'data) t list -> ('key, 'data) Column_intf.with_sorter
end

module Dynamic_columns_with_sorter : sig
  type ('key, 'data) t

  val column
    :  ?sort:('key * 'data -> 'key * 'data -> int)
    -> ?initial_width:[ `Px of int ]
    -> ?visible:bool
    -> label:Vdom.Node.t
    -> cell:(key:'key -> data:'data -> Vdom.Node.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t -> ('key, 'data) t -> ('key, 'data) t
  val lift : ('key, 'data) t list Value.t -> ('key, 'data) Column_intf.with_sorter
end
