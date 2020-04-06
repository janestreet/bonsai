open! Core_kernel
open! Import

type ('input, 'model, 'action, 'result, 'incr, 'event) Component.unpacked +=
  | C : 'result -> (_, _, Nothing.t, 'result, 'incr, _) Component.unpacked

val const : 'result -> (_, 'result, _, _) Packed.t
