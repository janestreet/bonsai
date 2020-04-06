open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t : ('k * 'input * 'extra, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; action_type_id : 'action Type_equal.Id.t
      ; inner_model : 'model Packed.model_info
      ; comparator : ('k, 'cmp) comparator
      ; result_by_k : ('result_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
      ; input_by_k : ('input_by_k, ('k, 'input, 'cmp) Map.t) Type_equal.t
      ; model_by_k : ('model_by_k, ('k, 'model, 'cmp) Map.t) Type_equal.t
      }
      -> ( 'input_by_k * 'extra
         , 'model_by_k
         , 'k * 'action
         , 'result_by_k
         , 'incr
         , 'event )
           unpacked
  (** We need the Type_equal witnesses here because the typechecker's rules aren't
      powerful enough to just have the Comparator.t here. *)

val associ_input
  :  ('key, 'cmp) comparator
  -> ('key * 'input * 'extra, 'result, 'incr, 'event) Packed.t
  -> ( ('key, 'input, 'cmp) Map.t * 'extra
     , ('key, 'result, 'cmp) Map.t
     , 'incr
     , 'event )
       Packed.t
