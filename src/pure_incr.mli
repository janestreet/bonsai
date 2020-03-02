open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      (('input, 'incr) Incremental.t -> ('result, 'incr) Incremental.t)
      -> ('input, _, Nothing.t, 'result, 'incr, 'event) unpacked
