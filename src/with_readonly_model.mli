open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      ('input * 'model, unit, 'action, 'result, 'incr, 'event) unpacked
      -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
