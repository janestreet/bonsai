open! Core
open! Async_kernel
open! Import

type 'a t



(** A Persistent_var.t is similar to Bonsai.Var.t, but the contents of
    the var are persisted into either local storage or session storage.

    Creating a persistent var requires that the type contained
    inside the var to be sexpable, so a first class module for the type
    should be passed in.

    You have a choice between Local Storage and Session Storage for
    actually persisting things;  The docs for which can be found here:

    - https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage
    - https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage

    A [unique_id] is required for the storage.  This value needs to be unique
    per application.

    Finally, a fallback is required for when the value can't be found, or it
    can't be deserialized. *)
val create
  :  (module Sexpable with type t = 'a)
  -> [ `Local_storage | `Session_storage ]
  -> unique_id:string
  -> default:'a
  -> 'a t

val update : 'a t -> f:('a -> 'a) -> unit
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a
val value : 'a t -> 'a Bonsai.Value.t

(** This will remove the value from localstorage or sessionstorage. (calling
    [set] or [update] afterwards will still store the value back again.) *)
val clear_persistence : 'a t -> unit

(** By asking for the vars effect, you get a function that can be easily
    threaded through your components and triggered inside an action-application
    or inside of an event listener. *)
val effect : 'a t -> 'a -> unit Effect.t

