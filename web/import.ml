module Incr = Ui_incr
module Vdom = Virtual_dom.Vdom

module Event = struct
  type t = Ui_event.t

  let sequence e = Vdom.Event.Many e
  let no_op = Vdom.Event.Ignore
end

module Bonsai = struct
  module Incr = Incr
  include Bonsai
end
