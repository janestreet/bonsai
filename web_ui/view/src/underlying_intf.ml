open! Core
open! Import

module C = struct
  class type t =
    object
      method theme_name : string
      method constants : Constants.t

      method button :
        attr:Vdom.Attr.t
        -> disabled:bool
        -> intent:Constants.Intent.t option
        -> on_click:unit Effect.t
        -> Vdom.Node.t list
        -> Vdom.Node.t

      method tabs :
        attr:Vdom.Attr.t
        -> per_tab_attr:('a -> is_active:bool -> Vdom.Attr.t)
        -> on_change:(from:'a -> to_:'a -> unit Effect.t)
        -> equal:('a -> 'a -> bool)
        -> active:'a
        -> ('a * Vdom.Node.t) list
        -> Vdom.Node.t

      method devbar :
        attr:Vdom.Attr.t
        -> count:int
        -> intent:Constants.Intent.t
        -> string
        -> Vdom.Node.t

      method humanize_sexp : Sexp.t -> string

      method text :
        attr:Vdom.Attr.t -> intent:Constants.Intent.t option -> string -> Vdom.Node.t
    end
end

module type S = sig
  class c : C.t
end

type t = (module S)
