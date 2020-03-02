open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | Model :
      { t : ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; cutoff : 'model Incremental.Cutoff.t
      }
      -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
  | Value :
      'input Incremental.Cutoff.t
      -> ('input, _, Nothing.t, 'input, 'incr, 'event) unpacked
