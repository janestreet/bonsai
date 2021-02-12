open! Core_kernel
open! Import

let lift ~f component = Bonsai.Arrow_deprecated.map component ~f:(Product.lift ~f)

module T = struct
  type ('result, 'input, 'parsed) t =
    ('input, ('result, 'parsed) Product.t) Bonsai.Arrow_deprecated.t

  include Applicative.Make3_using_map2 (struct
      type nonrec ('result, 'input, 'parsed) t = ('result, 'input, 'parsed) t

      let return value =
        Product.Fields.create ~value ~set:(const Bonsai.Event.Ignore)
        |> Bonsai.Arrow_deprecated.const
      ;;

      let map2 a b ~f =
        let open Bonsai.Arrow_deprecated.Let_syntax in
        let%map_open a = a
        and b = b in
        let value = f (Product.value a) (Product.value b) in
        let set parsed = Vdom.Event.Many [ Product.set a parsed; Product.set b parsed ] in
        Product.Fields.create ~value ~set
      ;;

      let map = `Define_using_map2
    end)
end

include T

module Open_on_rhs_intf = struct
  module type S = sig
    val lift
      :  f:('parsed2 -> 'parsed1)
      -> ('input, ('result, 'parsed1) Product.t) Bonsai.Arrow_deprecated.t
      -> ('input, ('result, 'parsed2) Product.t) Bonsai.Arrow_deprecated.t
  end
end

include
  Applicative.Make_let_syntax3 (T) (Open_on_rhs_intf)
    (struct
      let lift = lift
    end)
