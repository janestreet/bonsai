open! Core_kernel
open! Async_kernel
open! Import
include module type of Bonsai.Effect

(** [of_deferred_fun] is a way to convert from a deferred-returning function
    to an effect-returning function.  This function is commonly used to wrap RPC
    calls.  Memory is allocated permanently every time that [of_deferred_fun] is
    called, so be sure to re-use the function inside the Staged.t! *)
val of_deferred_fun : ('query -> 'response Deferred.t) -> ('query -> 'response t) Staged.t
