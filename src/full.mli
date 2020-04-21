open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) t =
  input:('input, 'incr) Incremental.t
  -> old_model:('model option, 'incr) Incremental.t
  -> model:('model, 'incr) Incremental.t
  -> inject:('action -> 'event)
  -> environment:'incr Bonsai_types.Environment.t
  -> incr_state:'incr Incremental.State.t
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { f : ('input, 'model, 'action, 'result, 'incr, 'event) t
      ; constructed_at : Source_code_position.t
      }
      -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

val of_full
  :  Source_code_position.t
  -> f:('input, 'model, 'action, 'result, 'incr, 'event) t
  -> action_type_id:'action Type_equal.Id.t
  -> model_type_id:'model Type_equal.Id.t
  -> default_model:'model
  -> model_equal:('model -> 'model -> bool)
  -> sexp_of_model:('model -> Sexp.t)
  -> model_of_sexp:(Sexp.t -> 'model)
  -> ('input, 'result, 'incr, 'event) Packed.t
