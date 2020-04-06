open! Core_kernel
open! Import

type ('input, 'model, 'action, 'result, 'incr, 'event) Component.unpacked +=
  | Pure_input :
      ('input -> 'result)
      -> ('input, _, Nothing.t, 'result, _, _) Component.unpacked
  | Return_input : ('input, _, Nothing.t, 'input, _, _) Component.unpacked

val pure : f:('input -> 'result) -> ('input, 'result, _, _) Packed.t
val input : ('input, 'input, _, _) Packed.t
