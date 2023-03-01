open Virtual_dom

module Colors : sig
  type t =
    { toggle_text : Css_gen.Color.t
    ; inner_background : Css_gen.Color.t
    ; inner_border : Css_gen.Color.t
    ; inner_text : Css_gen.Color.t
    }
end

(* [view] produces a "checkbox hack" toggle, which acts as a stateful component without
   actually needing to manage its own state. When [toggle] is hovered, a dialog box
   showing [inner] will be opened in [direction] relative to [toggle]. The user can toggle
   the dialog box to stay permanently open by clicking on [toggle]. The supplied
   [Colors.t] can be used to style the component. *)
val view
  :  Colors.t
  -> toggle:Vdom.Node.t
  -> inner:Vdom.Node.t
  -> direction:[ `Above | `Below | `Left | `Right ]
  -> Vdom.Node.t
