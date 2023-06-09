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
  (** [focus_handle] returns a [Vdom.Attr.t] and a [unit Effect.t] that focuses the
      [Vdom.Node.t] containing the [Vdom.Attr.t]. The attr should not be used on more than
      one [Vdom.Node.t], as only the first element will be focused when the effect runs. *)
  val on_effect : ?name_for_testing:string -> unit -> (Vdom.Attr.t * unit t) Computation.t

  (** [on_activate] will focus the element that the returned attr is attached to when
      this computation is activated.  See [Bonsai.Edge] for more details on the component
      lifecycle. *)
  val on_activate : ?name_for_testing:string -> unit -> Vdom.Attr.t Computation.t
end

(** [reload_page] will cause a page reload if running normally in a browser. When running
    in node/benchmarks/tests, it will just print that a reload would normally occur. *)
val reload_page : unit Effect.t
