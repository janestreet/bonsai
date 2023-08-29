open! Core
open Bonsai_web

(** A textbox with suggested results, modeled off of Chrome's address bar.

    - "Enter" invokes [on_select] with the selected suggestion. If the suggestion
      list is closed, the callback is not invoked, because of course nothing is
      selected; instead the suggestion list is opened.
    - "Clicking" on the suggestion list invokes [on_select].
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

module Expand_direction : sig
  type t =
    | Down
    (** Autocomplete options appear below the textbox, with the first item at
        the top of the list. *)
    | Up
    (** Autocomplete options appear above the textbox, with the first item at
        the bottom of the list. *)
  [@@deriving sexp, compare, enumerate, equal]
end

type 'k t =
  { selected_item : 'k option
  ; view : Vdom.Node.t
  ; query : string
  ; set_query : string -> unit Effect.t
  ; focus_input : unit Effect.t
  }
[@@deriving fields ~getters]

val create
  :  ('k, 'cmp) Bonsai.comparator
  -> ?initial_query:string
  -> ?max_visible_items:int Value.t
  (** The value defaults to Transient.  Read doc comment on the type for
      more info. *)
  -> ?suggestion_list_kind:Suggestion_list_kind.t Value.t
  (** The value defaults to Down. Read doc comment on the type for more info. *)
  -> ?expand_direction:Expand_direction.t Value.t
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
  (** If provided [on_blur] will be called whenever a blur triggered outside of the
      query box (including both input and item list) occurs. *)
  -> ?on_blur:unit Ui_effect.t Value.t
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
  -> 'k t Computation.t

module Filter_strategy : sig
  type t =
    | Fuzzy_match
    | Fuzzy_search_and_score
  [@@deriving compare, enumerate, equal, sexp_of]
end

(** [stringable] is like [create] but takes a map with possible completion options,
    instead of a function to generate them. Items are filtered and sorted based on [filter_strategy].

    - [`Fuzzy_match]: uses the [fuzzy_match] library to only display items that
      match the current pattern. Items are displayed in the order they appear in
      the input map.

    - [`Fuzzy_search_and_score]: uses the [fuzzy_search] library to only display
      items that match the current pattern. It also scores how well each item
      matches in order to sort the matching items. *)
val stringable
  :  ('k, 'cmp) Bonsai.comparator
  -> ?initial_query:string
  -> ?max_visible_items:int Value.t
  -> ?suggestion_list_kind:Suggestion_list_kind.t Value.t
  -> ?expand_direction:Expand_direction.t Value.t
  -> ?selected_item_attr:Vdom.Attr.t Value.t
  -> ?extra_list_container_attr:Vdom.Attr.t Value.t
  -> ?extra_input_attr:Vdom.Attr.t Value.t
  -> ?extra_attr:Vdom.Attr.t Value.t
  -> ?to_view:('k -> string -> Vdom.Node.t)
  -> filter_strategy:Filter_strategy.t
  -> on_select:('k -> unit Effect.t) Value.t
  -> ('k, string, 'cmp) Map.t Value.t
  -> 'k t Computation.t

module Collate_map_with_score : sig
  module Scored_key : sig
    type 'k t = int * 'k

    include Comparator.Derived with type 'a t := 'a t

    module M (T : Comparator.S) : sig
      type nonrec t = T.t t [@@deriving sexp_of]

      include
        Comparator.S
        with type t := t
         and type comparator_witness = T.comparator_witness comparator_witness
    end

    module Map : sig
      type nonrec ('k, 'v, 'cmp) t = ('k t, 'v, 'cmp comparator_witness) Map.t
    end
  end


  (** [collate] sorts and filters the input map according to a [score] function
      (filtering a result out is done by returning 0 from [score], and
      transforms the data in the map according to a [to_result] function.

      The performance trade-off is specific: we assume that the input map
      doesn't change very often so that we can pre-process all the items in the
      map ahead of time. Thus, the goal of this function is not to be as
      incremental as possible, but rather to have as low of constants as
      possible.

      [query_is_as_strict] is used to determine whether a new query will filter
      out at least as many items as the previous query. If so, then we can skip
      running [score] on items that have already been filtered away.

      Each time the query changes, we remember a previous query and which
      items have been filtered away by that query. However, if the previous
      query limits the input to fewer than [stop_trimming_input_at_count], then
      we don't update the previous query. This allows the user to specify that
      when the number of results is small enough that it is more worthwhile to
      cache a more general query in order to allow backtracking through
      previous queries without abandoning caching altogether. *)
  val collate
    :  (module Comparator.S with type t = 'k and type comparator_witness = 'cmp)
    -> preprocess:(key:'k -> data:'v -> 'preprocessed)
    -> score:('query -> 'preprocessed -> int)
    -> query_is_as_strict:('query -> as_:'query -> bool)
    -> to_result:('preprocessed -> key:'k -> data:'v -> 'result)
    -> ('k, 'v, 'cmp) Map.t Value.t
    -> 'query Value.t
    -> ('k, 'result, 'cmp) Scored_key.Map.t Computation.t
end
