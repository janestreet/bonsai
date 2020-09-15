open! Core_kernel
open! Import

(** [simplify t ~key_id ~data_id] attempts to reduce [t] to a plain (stateless) function
    with [key] and [data] inputs.  Returns [None] if it fails to find such a function.
    The only pattern that this function attempts to optimize is [(return v)], where [v] is
    a [Value.t] built out of only constants, named values, and [Value]'s applicative
    interface. *)
val function_of_'return'_computation
  :  ('model, 'action, 'result) Computation.t
  -> key_id:'key Type_equal.Id.t
  -> data_id:'data Type_equal.Id.t
  -> ('key -> 'data -> 'result) option
