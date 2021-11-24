open! Core
open Bonsai_web

(** A textbox with suggested results, modeled off of Chrome's address bar.

    - "Enter" invokes [on_select] with the selected suggestion. If the suggestion
      list is closed, the callback is not invoked, because of course nothing is
      selected; instead the suggestion list is opened.
    - Focusing the the text input opens the suggestion list
    - "Escape" or unfocusing the text input closes the suggestion list
    - "Tab" and "Down Arrow" move the selected item down. If the suggestion
      list is closed, it gets opened, and the selected item is set to the top
      item.
    - "Shift-Tab" and "Up Arrow" move the selected item up. Again, if the list
      is closed, it gets opened, but selection is sent to the bottom item.
    - Editing the textbox content re-filters the items.
    - Changing the selected suggestion has no effect on the textbox content,
      since there might not be a string which exactly selects an item.

    Only [max_visible_items] are rendered at once, so even with thousands of
    suggestions, DOM updates should be very quick.
*)

module Suggestion_list_kind : sig
  type t =
    | Transient_overlay
    (** When set to Transient, the suggestion-list only shows up when the
        textbox is focused, and the list will float over other items on the
        page.*)
    | Permanent_fixture
    (** Permanent will make the suggestion-list always present, and it
        will take up space during the layout of the application. *)
  [@@deriving sexp, compare, enumerate, equal]
end

val create
  :  ('k, 'cmp) Bonsai.comparator
  -> ?initial_query:string
  -> ?max_visible_items:int Value.t
  (** The value defaults to Transient.  Read doc comment on the type for
      more info. *)
  -> ?suggestion_list_kind:Suggestion_list_kind.t Value.t
  (** If provided, the attributes in this value will be attached to
      the vdom node representing the currently selected item in the list. *)
  -> ?selected_item_attr:Vdom.Attr.t Value.t
  (** If provided, [extra_list_container_attr] will be added to the
      vdom node containing the list of suggestions. *)
  -> ?extra_list_container_attr:Vdom.Attr.t Value.t
  (** If provided, [extra_input_attr] will be added to the query text input. *)
  -> ?extra_input_attr:Vdom.Attr.t Value.t
  (** If provided [extra_attr] will be added to the outermost div of this component. *)
  -> ?extra_attr:Vdom.Attr.t Value.t
  (** [f] generates the set of completion options by returning
      a map from a user-provided key ['k] to the view for that element
      in the dropdown list.  The currently entered filter from the textbox
      part of the component is provided as an argument to the function
      and it is expected that you use this to do your own filtering and
      return the filtered map. *)
  -> f:(string Value.t -> ('k, Vdom.Node.t, 'cmp) Map.t Computation.t)
  (** [on_select] is called when [enter] is hit when an item is
      selected. *)
  -> on_select:('k -> unit Effect.t) Value.t
  -> unit
  -> Vdom.Node.t Computation.t


(** [stringable] is like [create] but takes a map with possible completion options,
    instead of a function to generate them. Completion options will be displayed if their
    string representation [Fuzzy_match]es the current query. *)
val stringable
  :  ('k, 'cmp) Bonsai.comparator
  -> ?initial_query:string
  -> ?max_visible_items:int Value.t
  -> ?suggestion_list_kind:Suggestion_list_kind.t Value.t
  -> ?selected_item_attr:Vdom.Attr.t Value.t
  -> ?extra_list_container_attr:Vdom.Attr.t Value.t
  -> ?extra_input_attr:Vdom.Attr.t Value.t
  -> ?extra_attr:Vdom.Attr.t Value.t
  -> ?to_view:('k -> string -> Vdom.Node.t)
  -> on_select:('k -> unit Effect.t) Value.t
  -> ('k, string, 'cmp) Map.t Value.t
  -> Vdom.Node.t Computation.t
