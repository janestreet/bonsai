open! Core
open! Bonsai_web

type t =
  { view : Vdom.Node.t
  ; is_open : bool
  ; open_ : unit Effect.t
  ; close : unit Effect.t
  ; toggle : unit Effect.t
  }

val component
  :  ?extra_container_attr:Vdom.Attr.t Value.t
  -> ?extra_title_attr:Vdom.Attr.t Value.t
  -> ?extra_content_attr:Vdom.Attr.t Value.t
  -> starts_open:bool
  -> title:Vdom.Node.t Value.t
  -> content:Vdom.Node.t Computation.t
  -> unit
  -> t Computation.t
