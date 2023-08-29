open Bonsai_web

(** [make'] is like [make], but supports wrapping arbitrary nodes inside the link. *)
val make'
  :  ?attrs:Vdom.Attr.t list
  -> set_url:('a -> unit Effect.t)
  -> page_to_string:('a -> string)
  -> 'a
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** [make] builds a nav link, a link to navigates to another page within the app. It
    performs an on-page navigation when clicked directly, but also plays well with
    Mod-click, such as Ctrl-click to open in a new tab.

    When using this inside your app, you may want to create a wrapper that fixes the
    page_to_string argument. *)
val make
  :  ?attrs:Vdom.Attr.t list
  -> set_url:('a -> unit Effect.t)
  -> page_to_string:('a -> string)
  -> 'a
  -> string
  -> Vdom.Node.t
