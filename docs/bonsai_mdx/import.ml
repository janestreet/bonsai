module Incr = Incremental.Make ()
module Vdom = Vdom

module Bonsai = struct
  module Incr = Incr
  module Event = Vdom.Event
  include Bonsai.Make (Incr) (Vdom.Event)
end
