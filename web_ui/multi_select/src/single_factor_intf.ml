open! Core
open! Import
open Bonsai_web

module type Item = sig
  type t [@@deriving equal, sexp_of]

  include Comparable.S_plain with type t := t

  val to_string : t -> string
end

module type S = sig
  module Item : Item

  (** A widget for selecting multiple items from a list. A fairly rich interface is
      provided, including:
      - Using the arrow keys and Enter to select / deselect items;
      - The ability to filter the set of items displayed by searching;
      - "Select all" and "Select none" buttons
  *)

  module View_config : sig
    type t =
      { header : Vdom.Node.t
          (** The header will be displayed above the rest of the node. *)
      ; autofocus_search_box : bool
          (** If true, appends the autofocus attribute. This can cause the browser to
          automatically focus the search box when the page loads. (That probably isn't
          useful if there is more than one of these components on a given page.) *)
      ; search_box_id : string option (** The HTML ID to give the search box input *)
      ; extra_row_attrs : (is_focused:bool -> Vdom.Attr.t) option
          (** This attribute will be added to the selected row *)
      ; allow_updates_when_focused : [ `Always | `Never ]
          (** Determines whether or not the state should update the value of input
          elements, even if the input element is actively in focus *)
      }

    val create
      :  ?extra_row_attrs:(is_focused:bool -> Vdom.Attr.t)
      -> ?autofocus_search_box:bool
      -> ?id:string
      -> header:Vdom.Node.t
      -> allow_updates_when_focused:[ `Always | `Never ]
      -> unit
      -> t
  end

  module Action : sig
    type t =
      | Update_search_string of string
      | Set_item_selected of
          { item : Item.t
          ; status : Selection_status.t
          }
      | Set_all_selection_statuses of Selection_status.t Item.Map.t
          (** [Set_all_selection_statuses] sets the status for every item in the map, and any
          item not in the map gets [input.default_selection_status]. *)
      | Toggle_focused_item_selected
      | Set_focus of Item.t option
      | Move_focus of [ `Next | `Prev ]
      | Select_all
      | Select_none
    [@@deriving sexp_of]
  end

  module Result : sig
    type t =
      { view : Vdom.Node.t
      ; view_for_testing : string Lazy.t
      ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
      ; inject : Action.t -> unit Vdom.Effect.t
      ; selected_items : Item.Set.t
      }
  end

  module Initial_model_settings : sig
    type t

    val create
      :  ?search_string:string
      -> ?selection_status:Selection_status.t Item.Map.t
      -> ?focused_item:Item.t
      -> unit
      -> t
  end

  val bonsai
    :  ?initial_model_settings:Initial_model_settings.t
    -> ?default_selection_status:Selection_status.t Value.t
         (** [default_selection_status] controls whether items that have not been
        explicitly toggled by the user should be considered selected or not.
        For example, setting this to [Selected] has the effect of causing all
        items to show initially selected. *)
    -> view_config:View_config.t Value.t
    -> Item.Set.t Value.t
    -> Result.t Computation.t
end

module type Single_factor = sig
  module type Item = Item
  module type S = S

  module Make (Item : Item) : S with module Item := Item
end
