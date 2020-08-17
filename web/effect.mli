open! Core_kernel
open! Async_kernel
open! Import

(** ['a Effect.t] represents some computation of type ['a] that can be performed
    outside of the typical computational/incremental structure of a Bonsai program.
    Examples of this computation might be:

    - Calling an RPC and getting the result back
    - Running expensive computation on a web-worker thread
    - Requesting some information from the imperative "Start.Handle"-holding code

    If you have a value of type ['a Effect.t], you can schedule it to be run
    by calling [inject] and providing a function that will be called when
    the callback completes. *)

type 'a t

include Monad.S with type 'a t := 'a t

(** Converts a [Vdom.Event.t] to an [Effect.t].  This can be useful if you want
    to use the monadic interface that is available via [Effect.t] in order to
    sequence events. *)
val of_event : Vdom.Event.t -> unit t


(** [of_deferred_fun] is a way to convert from a deferred-returning function
    to an effect-returning function.  This function is commonly used to wrap RPC
    calls.  Memory is allocated permanently every time that [of_deferred_fun] is
    called, so be sure to re-use the function inside the Staged.t! *)
val of_deferred_fun : ('query -> 'result Deferred.t) -> ('query -> 'result t) Staged.t

(** [of_sync_fun] is similar to [of_deferred_fun] but with a synchronous function
    instead of a deferred one.  This can be used for functions that are synchronous
    but side-effecting, or as a mock-function in tests that replace the usages of
    [of_deferred_fun] in the actual app. *)
val of_sync_fun : ('query -> 'result) -> ('query -> 'result t) Staged.t

(** An effect that never completes *)
val never : _ t

(** Produces an event which schedules the some computation to be run.  When that
    computation is complete, [on_response] is called with the value. *)
val inject : 'a t -> on_response:('a -> Vdom.Event.t) -> Vdom.Event.t

(** When the type being computed is unit, you are calling this Effect for
    side-effects that don't yield a value back.  [inject_ignoring_response]
    frees you from having to pass an [on_response] callback. *)
val inject_ignoring_response : unit t -> Vdom.Event.t

(** [inject_with_userdata] is the same as [inject], but you have the option
    of passing in some extra value which will also be available in the [on_response]
    callback. *)
val inject_with_userdata
  :  'a t
  -> userdata:'u
  -> on_response:('a -> 'u -> Vdom.Event.t)
  -> Vdom.Event.t

(** Transforms a result-returning [Effect.t] into an [Effect.t] that doesn't have
    it's error case, by providing a callback that handles the error condition. *)
val handle_error : ('ok, 'error) Result.t t -> f:('error -> Vdom.Event.t) -> 'ok t
