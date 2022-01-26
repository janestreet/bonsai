open! Core
open! Bonsai_web


(** A vertical list component which moves items into their proper place during
    drag and drop. Items use absolute positioning for explicit layout; that is,
    the nth item is [n * item_height] pixels from the top of the container.
    Items outside the list may be dragged into the list to extend it. *)
val list
  :  ('source, 'cmp) Bonsai.comparator
  -> dnd:('source, int) Bonsai_web_ui_drag_and_drop.t Bonsai.Value.t
  (** The drag-and-drop universe the list should operate in; other items in the
      universe may be dragged into the list *)
  -> ?enable_debug_overlay:bool
  (** Display a transparent overlay on targets to make it clear where an item
      may be dropped. *)
  -> ?extra_item_attrs:Vdom.Attr.t Bonsai.Value.t
  (** Extra attributes to put on the wrapper div for each item in the list. For
      example, you might want to make each item animate into and out of
      position. *)
  -> ?left:Css_gen.Length.t
  (** The space between the left edge of an item and the list container *)
  -> ?right:Css_gen.Length.t
  (** The space between the right edge of an item and the list container *)
  -> ?empty_list_placeholder:(item_is_hovered:bool Value.t -> Vdom.Node.t Computation.t)
  (** What to display when there are no items in the list. [item_is_hovered] is
      provided in case you wish to change the placeholder based on whether an
      item is being hovered above the empty list.  *)
  -> ?default_item_height:int
  (** The items and drop targets are spaced evenly every item_height. In order
      to look natural, each item should have height [item_height]. *)
  -> ('source, Vdom.Node.t * int, 'cmp) Map.t Value.t
  (** The items that should be displayed in the list. Each item should have its
      view and its current rank. Updating the rank of an item must be done via
      the [on_drop] callback of the drag-and-drop universe. *)
  -> Vdom.Node.t Bonsai.Computation.t

(** Similar to [list], but creates the drag-and-drop universe and handles the
    [on_drop] event, making it fully self-contained. *)
val simple
  :  ('key, 'cmp) Bonsai.comparator
  -> ?sentinel_name:string
  -> ?enable_debug_overlay:bool
  -> ?extra_item_attrs:Vdom.Attr.t Value.t
  -> ?left:Css_gen.Length.t
  -> ?right:Css_gen.Length.t
  -> ?empty_list_placeholder:(item_is_hovered:bool Value.t -> Vdom.Node.t Computation.t)
  -> ?default_item_height:int
  -> render:
       (index:int Value.t
        -> source:Vdom.Attr.t Value.t
        -> 'key Value.t
        -> ('data * Vdom.Node.t) Computation.t)
  -> ('key, 'cmp) Set.t Value.t
  -> (('key * 'data) list * Vdom.Node.t) Computation.t

module Action : sig
  type 'a item =
    | Move of 'a * int
    | Set of 'a
    | Remove of 'a
    | Overwrite of 'a list
  [@@deriving sexp]

  type 'a t = 'a item list [@@deriving sexp]
end

(** Similar to [simple], but exposes the components injection function. This is
    used by the [Bonsai_web_ui_form] wrapper of this library. *)
val with_inject
  :  ('key, 'cmp) Bonsai.comparator
  -> ?sentinel_name:string
  -> ?enable_debug_overlay:bool
  -> ?extra_item_attrs:Vdom.Attr.t Value.t
  -> ?left:Css_gen.Length.t
  -> ?right:Css_gen.Length.t
  -> ?empty_list_placeholder:(item_is_hovered:bool Value.t -> Vdom.Node.t Computation.t)
  -> ?default_item_height:int
  -> (index:int Value.t
      -> source:Vdom.Attr.t Value.t
      -> 'key Value.t
      -> ('data * Vdom.Node.t) Computation.t)
  -> (('key * 'data) list * Vdom.Node.t * ('key Action.t -> unit Effect.t)) Computation.t
