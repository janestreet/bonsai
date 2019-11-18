open! Core_kernel
open! Import

module type S = Component.S

module Make (Incr : Incremental.S) (Event : T) = Component.Make (Incr) (Event)
