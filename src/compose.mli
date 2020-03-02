open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t1 : ('i1, 'model, 'a1, 'r1, 'incr, 'event) unpacked
      ; action_type_id1 : 'a1 Type_equal.Id.t
      ; t2 : ('r1, 'model, 'a2, 'r2, 'incr, 'event) unpacked
      ; action_type_id2 : 'a2 Type_equal.Id.t
      }
      -> ('i1, 'model, ('a1, 'a2) Either.t, 'r2, 'incr, 'event) unpacked
