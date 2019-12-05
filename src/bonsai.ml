open! Core_kernel
open! Import

module type S = Component.S
module type Event = Component.Event

module Make (Incr : Incremental.S) (Event : Event) = Component.Make (Incr) (Event)
