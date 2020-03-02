open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t : ('i2, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; f : 'i1 -> 'i2
      }
      -> ('i1, 'model, 'action, 'result, 'incr, 'event) unpacked
