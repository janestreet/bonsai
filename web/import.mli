module Incr = Incr_dom.Incr
module Vdom = Virtual_dom.Vdom

module Event : sig
  type t = Vdom.Event.t

  val sequence : t list -> t
  val no_op : t
end

module Bonsai : Bonsai.S with module Incr = Incr and module Event = Event
