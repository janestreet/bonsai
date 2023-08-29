open! Core
open! Async_kernel
open! Import
open Bonsai.For_open
include module type of Virtual_dom.Vdom.Effect

(** [of_deferred_fun] is a way to convert from a deferred-returning function to an
    effect-returning function. This function is commonly used to wrap RPC calls. *)
val of_deferred_fun : ('query -> 'response Deferred.t) -> 'query -> 'response t

(** Like [of_deferred_fun] but with a pre-applied unit query. Side-effects in the function
    will be run every time that the resulting effect is scheduled *)
val of_deferred_thunk : (unit -> 'response Deferred.t) -> 'response t

module Focus : sig
  type nonrec t =
    { attr : Vdom.Attr.t
    ; focus : unit t
    ; blur : unit t
    }

  (** [on_effect] returns a [Vdom.Attr.t] and two [unit Effect.t]s that focus/blur the
      [Vdom.Node.t] containing the [Vdom.Attr.t]. The attr should not be used on more than
      one [Vdom.Node.t], as only the first element will be focused/blurred when the effect
      runs.

      When [name_for_testing] is provided, the focus and blur effects will print in test mode.
      They will be a no-op otherwise. *)
  val on_effect : ?name_for_testing:string -> unit -> t Computation.t

  (** [on_activate] will focus the element that the returned attr is attached to when
      this computation is activated.  See [Bonsai.Edge] for more details on the component
      lifecycle.

      When [name_for_testing] is provided, the focus will print in test mode.
      It will be a no-op otherwise. *)
  val on_activate : ?name_for_testing:string -> unit -> Vdom.Attr.t Computation.t
end

(** [reload_page] will cause a page reload if running normally in a browser. When running
    in node/benchmarks/tests, it will just print that a reload would normally occur. *)
val reload_page : unit Effect.t

(** [alert] will cause an alert box to pop up with your provided message. *)
val alert : string -> unit Effect.t
