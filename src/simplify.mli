open! Core
open! Import

(** [computation_to_function t ~key_id ~data_id] attempts to reduce [t] to a
    plain (stateless) function with [key] and [data] inputs.  Returns [None] if
    it fails to find such a function. The only pattern that this function
    attempts to optimize is [(return v)], where [v] is a [Value.t] built out of
    only constants, named values, and [Value]'s applicative interface. *)
val computation_to_function
  :  ('model, 'dynamic_action, 'static_action, 'result) Computation.t
  -> key_compare:('key -> 'key -> int)
  -> key_id:'key Type_equal.Id.t
  -> data_id:'data Type_equal.Id.t
  -> (Path.t -> 'key -> 'data -> 'result) option
