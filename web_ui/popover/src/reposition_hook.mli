open! Core

(** A hook that horizontally re-positions a DOM element to stay within the windows bounds
    when possible.

    This uses the CSS transform property to re-position the element, and so it can not be
    used on DOM nodes that otherwise want to make use of this propery. *)
val create : unit -> Virtual_dom.Vdom.Attr.Hooks.t
