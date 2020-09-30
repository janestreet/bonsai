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
    [of_deferred_fun] in the actual app.

    Note that, unlike [of_deferred_fun], the function must return immediately, so it's not
    possible to test the behaviour of your app between calling the function and the effect
    becoming 'determined'. If you need to do this, see [of_svar] and
    [of_query_response_tracker] below.
*)
val of_sync_fun : ('query -> 'result) -> ('query -> 'result t) Staged.t

module For_testing : sig
  module Svar : sig
    (** You can think of an [Svar.t] as like an [Ivar.t] whose purpose is to allow us to
        implement [of_svar] below.

        (The difference between [Svar] and [Ivar] is that the former is synchronous. That
        is, when [fill_if_empty] is called, it will directly call all of the handlers rather
        than scheduling that they be called later. This semantics can be confusing to work
        with in large-scale programs, as it means the control flow of your application hops
        around a lot more. However, it does mean that you don't need a scheduler, so it's
        easier to implement.) *)

    type 'a t

    val create : unit -> 'a t
    val upon : 'a t -> ('a -> unit) -> unit
    val fill_if_empty : 'a t -> 'a -> unit
    val peek : 'a t -> 'a option
  end

  (** Create an effect from a function that returns an [Svar.t]. This is mostly useful in
      testing, to emulate a ['query -> 'result Deferred.t] function that does not return
      immediately. You may find [Query_response_tracker] a more convenient interface than
      using [of_svar] directly.
  *)
  val of_svar_fun : ('query -> 'result Svar.t) -> ('query -> 'result t) Staged.t

  module Query_response_tracker : sig
    (** [Query_response_tracker] is an interface designed to make [of_svar] more convenient
        to use. When the function returned by [of_query_response_tracker t] is called
        (typically by your bonsai app), the query passed is stored within [t]. Your test
        code can then call [maybe_handle_rpcs] to cause those effects to 'become
        determined'. *)
    type ('q, 'r) t

    val create : unit -> _ t

    type 'r maybe_respond =
      | No_response_yet
      | Respond of 'r

    val maybe_respond : ('q, 'r) t -> f:('q -> 'r maybe_respond) -> unit
    val queries_pending_response : ('q, _) t -> 'q list
  end

  val of_query_response_tracker
    :  ('query, 'result) Query_response_tracker.t
    -> ('query -> 'result t) Staged.t
end

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
