open! Core

(** A collection of helpers for working with data selection in Bonsai. *)

(** [One_of_many] is for selecting a single item from a list of options.

    This module helps you keep track of a selection (e.g., the active tab, a chosen row in
    a table, or a dropdown choice) as the underlying list of possible items changes over
    time. This is known as "mutually exclusive" selection.

    When the list of items changes, the selection is kept if it is still present;
    otherwise it is recomputed by a user-provided [selection_policy]. *)
module One_of_many : sig
  (** The current selected item and an effect to set the desired selection. The setter
      updates the stored selection immediately; the displayed selection will follow the
      rules described above. *)
  type 'a t = 'a Bonsai.t * ('a -> unit Bonsai.Effect.t) Bonsai.t

  module Selection_policy : sig
    (** Describes how to choose the visible selection when an initial value is needed or a
        previously stored selection is no longer valid.

        - [`Select_first_item] chooses the first element of [items].
        - [`Select_custom f] calls [f] with the previously stored selection (if any), and
          expects an item to display. Use this to implement policies like:
          - select an item based on some other Bonsai state (e.g. a form selection)
          - select an item based on an external datasource e.g. to implement "select last"
            behavior:
            {[
              fun _ -> Core.Nonempty_list.hd (Core.Nonempty_list.reverse items)
            ]} *)
    type 'a t =
      [ `Select_first_item
      | `Select_custom of 'a option -> 'a
      ]
  end

  (** Create a selection manager.

      Args:
      - [~equal]: item equality predicate
      - [~selection_policy]: strategy for computing the visible selection when an initial
        value is required or the stored selection is not present in [items]. Defaults to
        [`Select_first_item].
      - [items]: non-empty set of selectable candidates

      Returns [(selected, set_selected)], where [selected] is the current selection and
      [set_selected] updates the stored (requested) selection. *)
  val create
    :  equal:('a -> 'a -> bool)
    -> ?selection_policy:'a Selection_policy.t Bonsai.t
    -> 'a Nonempty_list.t Bonsai.t
    -> Bonsai.graph @ local
    -> 'a t
end

(** [Any_of_many] is for selecting any number of items from a list of options.

    Manage a set-like selection that can contain zero or more items. This is useful for
    checklists, multi-select tables, tag pickers, etc. The selection is exposed to the
    user as a list without duplicates.

    The set of possible items is "open"; it's not known at creation time and only applied
    for certain checks like [is_all_selected] and [select_all]. The internal list of
    selected items is not automatically pruned with [items] changes. *)
module Any_of_many : sig
  type 'a t = 'a list Bonsai.t * ('a list -> unit Bonsai.Effect.t) Bonsai.t

  (** Create a multiselect state.

      Args:
      - [~equal]: item equality predicate
      - [?init]: initial selection (defaults to empty list) *)
  val create
    :  equal:('a -> 'a -> bool)
    -> ?init:'a list Bonsai.t
    -> Bonsai.graph @ local
    -> 'a t

  (** Returns a predicate that tells whether [item] is currently selected in [t].

      Args:
      - [t]: multiselect state
      - [~equal]: item equality predicate *)
  val is_item_selected : 'a t -> equal:('a -> 'a -> bool) -> ('a -> bool) Bonsai.t

  (** Returns a function that sets selection state for an [item] in [t].

      Args:
      - [t]: multiselect state
      - [~equal]: item equality predicate

      The returned function takes [item] and [is_selected]. *)
  val set_item_selected
    :  'a t
    -> equal:('a -> 'a -> bool)
    -> ('a -> bool -> unit Bonsai.Effect.t) Bonsai.t

  (** Returns [true] if every element of [items] is selected in [t].

      Args:
      - [t]: multiselect state
      - [~equal]: item equality predicate
      - [items]: the universe to consider for "all" *)
  val is_all_selected
    :  'a list Bonsai.t * 'b
    -> equal:('a -> 'a -> bool)
    -> 'a list Bonsai.t
    -> bool Bonsai.t

  (** Returns [true] if any element of [items] is selected in [t].

      Args:
      - [t]: multiselect state
      - [~equal]: item equality predicate
      - [items]: the universe to check against *)
  val is_some_selected
    :  'a list Bonsai.t * 'b
    -> equal:('a -> 'a -> bool)
    -> 'a list Bonsai.t
    -> bool Bonsai.t

  (** Returns [true] if no items are selected in [t].

      Args:
      - [t]: multiselect state *)
  val is_none_selected : 'a t -> bool Bonsai.t

  (** Selects all [items] in [t].

      Args:
      - [t]: multiselect state
      - [items]: the universe to select *)
  val select_all : 'a t -> 'a list Bonsai.t -> unit Bonsai.Effect.t Bonsai.t

  (** Clears the selection. Args: [t] *)
  val unselect_all : 'a t -> unit Bonsai.Effect.t Bonsai.t
end
