open! Core_kernel
open! Import
open Component

val optimize
  :  ('input, 'result, 'incr, 'event) Packed.t
  -> ('input, 'result, 'incr, 'event) Packed.t
