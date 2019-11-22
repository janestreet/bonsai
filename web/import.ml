module Incr = Incr_dom.Incr
module Vdom = Virtual_dom.Vdom
module Bonsai = Bonsai.Make (Incr) (Vdom.Event)
