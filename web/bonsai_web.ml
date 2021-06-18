module Arrow_deprecated = struct
  module Effect = Effect
  module Vdom = Import.Vdom
  module Start = Start.Arrow_deprecated
  module Bonsai = Bonsai.Arrow_deprecated
end

module Start = Start.Proc
module Bonsai = Import.Bonsai
module Incr = Import.Incr
module Vdom = Import.Vdom
module To_incr_dom = To_incr_dom
module Effect = Effect
module Persistent_var = Persistent_var

(* new *)
module Computation = Bonsai.Computation
module Value = Bonsai.Value
