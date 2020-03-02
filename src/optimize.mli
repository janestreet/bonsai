open! Core_kernel
open! Import
open Component

val optimize
  :  ('input, 'model, 'result, 'incr, 'event) Packed.t
  -> ('input, 'model, 'result, 'incr, 'event) Packed.t
