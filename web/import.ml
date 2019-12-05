module Incr = Incr_dom.Incr
module Vdom = Virtual_dom.Vdom

module Event = struct
  type t = Vdom.Event.t

  let sequence e = Vdom.Event.Many e
  let no_op = Vdom.Event.Ignore
end

module Bonsai = Bonsai.Make (Incr) (Event)
