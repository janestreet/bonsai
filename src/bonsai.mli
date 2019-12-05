open! Core_kernel
open! Import

module type S = Component.S
module type Event = Component.Event

(** Bonsai can be used with any Incremental-style UI framework.  The parameters for the
    Bonsai component functor are an instance of Incremental (used to re-evaluate the UI
    only when the UI model has changed) and an opaque Event.t type (which is used to
    schedule actions).

    The recommended use of this functor is to bind the name [Bonsai] to its invocation.
    For example, [Bonsai_web]'s [import.ml] has:

    {[
      module Incr = Incr_dom.Incr
      module Vdom = Virtual_dom.Vdom
      module Bonsai = Bonsai.Make (Incr) (Vdom.Event)
    ]}

    [Bonsai_web] re-exports the contents of its [Import] module, which allows users to
    refer to the module [Bonsai] to construct components. *)
module Make (Incr : Incremental.S) (Event : Event) :
  S with module Incr := Incr with module Event := Event
