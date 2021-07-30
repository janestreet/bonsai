module Incr = Ui_incr
module Vdom = Virtual_dom.Vdom

module Event : sig
  type t = unit Ui_effect.t

  val sequence : t list -> t
  val no_op : t
end

module Bonsai = Bonsai
