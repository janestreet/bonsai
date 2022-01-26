open! Core
open! Import

type ('model, 'action) transition =
  schedule_event:(unit Effect.t -> unit) -> 'model -> 'action -> 'model

type ('model, 'action) t =
  | Incremental : ('model, 'action) transition Incr.t -> ('model, 'action) t
  | Impossible : ('action -> Nothing.t) -> (_, 'action) t
  | Join :
      { incr : ('model1, 'action1) transition Incr.t
      ; witness : 'action2 -> Nothing.t
      ; map_model : 'model -> 'model1 * 'model2
      ; unmap_model : 'model1 * 'model2 -> 'model
      ; map_action : 'action -> ('action1, 'action2) Either.t
      }
      -> ('model, 'action) t

val incremental : ('model, 'action) transition Incr.t -> ('model, 'action) t
val impossible : (_, Nothing.t) t
val to_incremental : ('model, 'action) t -> ('model, 'action) transition Incr.t

val merge
  :  ('model1, 'action1) t
  -> ('model2, 'action2) t
  -> ('model1 * 'model2, ('action1, 'action2) Base.Either.t) t

val map
  :  ('model_in, 'action_in) t
  -> f:(('model_in, 'action_in) transition -> ('model_out, 'action_out) transition)
  -> ('model_out, 'action_out) t

val iter_incremental : _ t -> f:(Incremental.Packed.t -> unit) -> unit
