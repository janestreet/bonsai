module Start = Start
module Bonsai = Import.Bonsai
module Incr = Import.Incr
module Vdom = Import.Vdom
module To_incr_dom = To_incr_dom
module Effect = Effect

module Future = struct
  module Effect = Effect
  module Vdom = Import.Vdom

  (* Proc stuff *)
  module Start = Start.Proc
  module Bonsai = Bonsai.Proc
end
