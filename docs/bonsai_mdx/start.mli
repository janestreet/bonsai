open! Core_kernel
open! Async_kernel
open! Import

module Handle : sig
  type ('i, 'a) t

  val stop : _ t -> unit
  val started : _ t -> unit Deferred.t
  val schedule : (_, 'a) t -> 'a -> unit
  val set_input : ('i, _) t -> 'i -> unit
end

val start_standalone
  :  initial_input:'input
  -> initial_model:'model
  -> bind_to_element_with_id:string
  -> ('input, 'model, Vdom.Node.t) Bonsai.t
  -> ('input, Nothing.t) Handle.t

val start
  :  initial_input:'input
  -> initial_model:'model
  -> bind_to_element_with_id:string
  -> ('input, 'model, Vdom.Node.t * ('a -> Vdom.Event.t)) Bonsai.t
  -> ('input, 'a) Handle.t
