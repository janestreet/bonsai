open! Core
open! Async_kernel
open! Import
open Bonsai.For_open
include module type of Virtual_dom.Vdom.Effect

(** [of_deferred_fun] is a way to convert from a deferred-returning function
    to an effect-returning function.  This function is commonly used to wrap RPC
    calls.  Memory is allocated permanently every time that [of_deferred_fun] is
    called, so be sure to re-use the function inside the Staged.t! *)
val of_deferred_fun : ('query -> 'response Deferred.t) -> 'query -> 'response t

(** [focus_handle] returns a [Vdom.Attr.t] and a [unit Effect.t] that focuses the
    [Vdom.Node.t] containing the [Vdom.Attr.t]. The attr should not be used on more than
    one [Vdom.Node.t], as only the first element will be focused when the effect runs. *)
val focus_handle : name_for_testing:string -> (Vdom.Attr.t * unit t) Computation.t
