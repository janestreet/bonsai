open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | Model :
      { t : ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; model_equal : 'model -> 'model -> bool
      }
      -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
  | Value :
      'input Incremental.Cutoff.t
      -> ('input, _, Nothing.t, 'input, 'incr, 'event) unpacked

val value_cutoff : cutoff:'input Incremental.Cutoff.t -> ('input, 'input, _, _) Packed.t

val model_cutoff
  :  ('input, 'result, 'incr, 'event) Packed.t
  -> ('input, 'result, 'incr, 'event) Packed.t
