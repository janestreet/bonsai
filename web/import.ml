module Incr = Incr_dom.Incr
module Vdom = Virtual_dom.Vdom

module Event = struct
  type t = Vdom.Event.t

  let sequence e = Vdom.Event.Many e
  let no_op = Vdom.Event.Ignore
end

module Bonsai = struct
  module Event = Event
  module Incr = Incr
  include Bonsai.Make (Incr) (Event)
end
