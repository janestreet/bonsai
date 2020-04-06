open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t1 : ('i1, 'm1, 'a1, 'r1, 'incr, 'event) unpacked
      ; model1 : 'm1 Packed.model_info
      ; action_type_id1 : 'a1 Type_equal.Id.t
      ; t2 : ('r1, 'm2, 'a2, 'r2, 'incr, 'event) unpacked
      ; model2 : 'm2 Packed.model_info
      ; action_type_id2 : 'a2 Type_equal.Id.t
      }
      -> ('i1, 'm1 * 'm2, ('a1, 'a2) Either.t, 'r2, 'incr, 'event) unpacked

val compose
  :  ('input, 'intermediate, 'incr, 'event) Packed.t
  -> ('intermediate, 'result, 'incr, 'event) Packed.t
  -> ('input, 'result, 'incr, 'event) Packed.t
