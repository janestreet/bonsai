open! Core
open! Virtual_dom

type ('a, 'view) t =
  { value : 'a Or_error.t
  ; view : 'view
  ; set : 'a -> unit Vdom.Effect.t
  }
