open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | Map1 :
      { t : ('input, 'model, 'action, 'r1, 'incr, 'event) unpacked
      ; f : 'r1 -> 'r2
      }
      -> ('input, 'model, 'action, 'r2, 'incr, 'event) unpacked
  | Map2 :
      { t1 : ('input, 'model, 'a1, 'r1, 'incr, 'event) unpacked
      ; action_type_id1 : 'a1 Type_equal.Id.t
      ; t2 : ('input, 'model, 'a2, 'r2, 'incr, 'event) unpacked
      ; action_type_id2 : 'a2 Type_equal.Id.t
      ; f : 'r1 -> 'r2 -> 'result
      }
      -> ('input, 'model, ('a1, 'a2) Either.t, 'result, 'incr, 'event) unpacked
