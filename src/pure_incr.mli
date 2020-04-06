open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      (('input, 'incr) Incremental.t -> ('result, 'incr) Incremental.t)
      -> ('input, _, Nothing.t, 'result, 'incr, 'event) unpacked

(** Same as [Bonsai.pure] but allows the user to optimize using Incremental. *)
val pure_incr
  :  f:(('input, 'incr) Incremental.t -> ('result, 'incr) Incremental.t)
  -> ('input, 'result, 'incr, 'event) Packed.t
