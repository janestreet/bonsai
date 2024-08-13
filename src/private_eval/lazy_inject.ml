open! Core
open! Import

let make f a = Effect.lazy_ (lazy (f a))
