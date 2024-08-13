open! Core
open! Import

val make : ('a -> 'b Effect.t) -> 'a -> 'b Effect.t
