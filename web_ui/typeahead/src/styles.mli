open! Core
open! Bonsai_web

(** This file exists to encapsulate all the styles used within these form elements. Ideally
    these form components would be web components [https://developer.mozilla.org/en-US/docs/Web/Web_Components]
    and styles would be encapsulated within a shadow dom. This doesn't appear to be supported in jsoo.
    (at least the bindings don't exist)

    The bindings won't land anytime soon - virtual-dom needs to be rewritten to support it.
*)

val full_width : Css_gen.t

(** [typeahead] is used for applying styles to input typeahead elements which should be styled
    in a manner consistent with Jane_web_style but aren't. *)
val typeahead : Css_gen.t

(** [pill] and [pill_container] are used to style the pills used to display choices made by the user
    in the multiselect. *)
val pill : Css_gen.t

val pill_container : Css_gen.t
