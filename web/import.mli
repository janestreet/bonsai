module Incr = Ui_incr
module Vdom = Virtual_dom.Vdom

module Event : sig
  type t = Ui_event.t

  val sequence : t list -> t
  val no_op : t
end

module Bonsai = Bonsai
