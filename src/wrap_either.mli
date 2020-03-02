open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t1 : ('input, 'm1, 'a1, 'r1, 'incr, 'event) unpacked
      ; action_type_id1 : 'a1 Type_equal.Id.t
      ; t2 : ('input, 'm2, 'a2, 'r2, 'incr, 'event) unpacked
      ; action_type_id2 : 'a2 Type_equal.Id.t
      ; on_action_for_other_component :
          ( [ `Action_for_first of 'm2 | `Action_for_second of 'm1 ]
          , ('m1, 'm2) Either.t )
            on_action_mismatch
      }
      -> ( 'input
         , ('m1, 'm2) Either.t
         , ('a1, 'a2) Either.t
         , ('r1, 'r2) Either.t
         , 'incr
         , 'event )
           unpacked
