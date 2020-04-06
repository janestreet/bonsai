open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t : ('i2, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; f : 'i1 -> 'i2
      }
      -> ('i1, 'model, 'action, 'result, 'incr, 'event) unpacked

val map_input
  :  ('i2, 'result, 'incr, 'event) Packed.t
  -> f:('i1 -> 'i2)
  -> ('i1, 'result, 'incr, 'event) Packed.t
