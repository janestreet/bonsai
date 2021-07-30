module Incr = Ui_incr
module Vdom = Virtual_dom.Vdom

module Event = struct
  type t = unit Ui_effect.t

  let sequence e = Vdom.Effect.Many e
  let no_op = Vdom.Effect.Ignore
end

module Bonsai = Bonsai
