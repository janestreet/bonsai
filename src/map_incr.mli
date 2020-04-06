open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t : ('input, 'model, 'action, 'r1, 'incr, 'event) unpacked
      ; f : ('r1, 'incr) Incremental.t -> ('r2, 'incr) Incremental.t
      }
      -> ('input, 'model, 'action, 'r2, 'incr, 'event) unpacked

val map_incr
  :  ('input, 'r1, 'incr, 'event) Packed.t
  -> f:(('r1, 'incr) Incremental.t -> ('r2, 'incr) Incremental.t)
  -> ('input, 'r2, 'incr, 'event) Packed.t
