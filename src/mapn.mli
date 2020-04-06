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
      { t1 : ('input, 'model1, 'action1, 'r1, 'incr, 'event) unpacked
      ; action_type_id1 : 'action1 Type_equal.Id.t
      ; model1 : 'model1 Packed.model_info
      ; t2 : ('input, 'model2, 'action2, 'r2, 'incr, 'event) unpacked
      ; action_type_id2 : 'action2 Type_equal.Id.t
      ; model2 : 'model2 Packed.model_info
      ; f : 'r1 -> 'r2 -> 'result
      }
      -> ( 'input
         , 'model1 * 'model2
         , ('action1, 'action2) Either.t
         , 'result
         , 'incr
         , 'event )
           unpacked

val map
  :  ('input, 'result1, 'incr, 'event) Packed.t
  -> f:('result1 -> 'result2)
  -> ('input, 'result2, 'incr, 'event) Packed.t

val map2
  :  ('input, 'result1, 'incr, 'event) Packed.t
  -> ('input, 'result2, 'incr, 'event) Packed.t
  -> f:('result1 -> 'result2 -> 'result3)
  -> ('input, 'result3, 'incr, 'event) Packed.t

val both
  :  ('input, 'result1, 'incr, 'event) Packed.t
  -> ('input, 'result2, 'incr, 'event) Packed.t
  -> ('input, 'result1 * 'result2, 'incr, 'event) Packed.t
