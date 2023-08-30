open! Core
open! Bonsai_web
open Bonsai_web_ui_partial_render_table_protocol
module Order = Order
module Sortable_header = Sortable_header

module For_testing : sig
  module Table_body = Table_body.For_testing

  type t = { body : Table_body.t }
end

module Focus_by_row = Focus.By_row

module Basic : sig
  module Focus : sig
    module By_row = Focus.By_row

    type ('a, 'p, 'k) t =
      | None : (unit, unit, 'k) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Value.t }
          -> ('k Focus_by_row.optional, 'k option, 'k) t
  end

  module Result : sig
    type 'focus t =
      { view : Vdom.Node.t
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; num_filtered_rows : int
      ; sortable_header : int Sortable_header.t
      }
    [@@deriving fields ~getters]
  end

  module Columns : sig
    (** There are a few ways to specify the columns on a partial render, table,
        and they each have their own tradeoffs and capibilities.  You can not
        mix-and-match column kinds.  Read the doc comments for each of the
        submodules to learn more.  *)

    type ('key, 'data) t
    type ('key, 'data) columns := ('key, 'data) t

    module Dynamic_cells : sig
      (** Dynamic_cells is a column-specification format with the following
          tradeoffs:

          - Pro: Each cell is it's own bonsai computation, so you can stick complex
            components in side of them, like forms, or graphs.
          - Con: The set of columns must be statically known ahead of time, and can
            not be determined dynamically. *)

      type ('key, 'data) t

      val column
        :  ?sort:('key * 'data -> 'key * 'data -> int) Value.t
             (** If this column is sortable, you can provide the sorting function here *)
        -> ?sort_reversed:('key * 'data -> 'key * 'data -> int) Value.t
             (** If the column has a specialized "reverse order", you can provide it here. *)
        -> ?initial_width:Css_gen.Length.t
        -> ?visible:bool Value.t
             (** [visible] can be set to [false] to hide the whole column. *)
        -> header:(Sort_state.t -> Vdom.Node.t) Value.t
             (** [header] determines the contents of the column header *)
        -> cell:(key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t)
             (** [cell] is the function determines the contents of every cell in this column. *)
        -> unit
        -> ('key, 'data) t

      (** [group ~label children] builds a header-group that has [children] underneath it.
          The content of header-group is set to [label] *)
      val group : label:Vdom.Node.t Value.t -> ('key, 'data) t list -> ('key, 'data) t

      (** [expand ~label child] builds a header-group that has a single child underneath it. *)
      val expand : label:Vdom.Node.t Value.t -> ('key, 'data) t -> ('key, 'data) t

      (** [lift] pulls a list of columns out into a column specification for use in the primary APIs  *)
      val lift : ('key, 'data) t list -> ('key, 'data) columns

      (** [Header_helpers] provides helper functions for constructing table headers with
          sort indicators *)
      module Header_helpers = Column.Dynamic_cells_with_sorter.Header_helpers
    end

    module Dynamic_columns : sig
      (** Dynamic_columns is a column-specification format with the
          following tradeoffs:

          - Pro: The set of columns, and how to render them can be determined
            dynamically ([lift] takes a column list inside a Value.t)
          - Con: Cells are computed with plain functions, and can not maintain
            state. *)
      type ('key, 'data) t

      val column
        :  ?sort:('key * 'data -> 'key * 'data -> int)
             (** If this column is sortable, you can provide the sorting function here *)
        -> ?sort_reversed:('key * 'data -> 'key * 'data -> int)
             (** If the column has a specialized "reverse order", you can provide it here. *)
        -> ?initial_width:Css_gen.Length.t
        -> ?visible:bool (** [visible] can be set to [false] to hide the whole column. *)
        -> header:(Sort_state.t -> Vdom.Node.t)
             (** [header] determines the contents of the column header *)
        -> cell:(key:'key -> data:'data -> Vdom.Node.t)
             (** [cell] is the function determines the contents of every cell in this column. *)
        -> unit
        -> ('key, 'data) t

      (** [group ~label children] builds a header-group that has [children] underneath it.
          The content of header-group is set to [label] *)
      val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t

      (** [lift] pulls a list of columns out into a column specification for use in the primary APIs  *)
      val lift : ('key, 'data) t list Value.t -> ('key, 'data) columns

      (** [Header_helpers] provides helper functions for constructing table headers with
          sort indicators *)
      module Header_helpers = Column.Dynamic_columns_with_sorter.Header_helpers
    end
  end

  type 'a compare := 'a -> 'a -> int

  (** This is the main UI component for the table content. *)
  val component
    :  ?filter:(key:'key -> data:'data -> bool) Value.t
         (** An optional function may be provided, which filters the rows in the table. *)
    -> ?override_sort:
         ('key compare -> ('key * 'data) compare -> ('key * 'data) compare) Value.t
         (** override_sort is an optional function that transforms the tables current sort,
        taking into account the default-sort and any user-provided sorts that they've
        added by clicking on column headers.

        [override_sort] is also given the comparison function for the key of the table,
        which the overrider can use as a fall-back for when the the ('key * 'data)
        comparison function returns 0. *)
    -> ?default_sort:('key * 'data) compare Value.t
         (** An optional function may be provided to sort the table. *)
    -> ?preload_rows:int
    -> ('key, 'cmp) Bonsai.comparator
    -> focus:('focus, 'presence, 'key) Focus.t
    -> row_height:[ `Px of int ] Value.t
         (** [row_height] is the height of every row in the table. If the row height
        is specified to be 0px or less, we instead use 1px. *)
    -> columns:('key, 'data) Columns.t
    -> ('key, 'data, 'cmp) Map.t Value.t (** The input data for the table *)
    -> 'focus Result.t Computation.t
end

module Expert : sig
  (** In the [Basic] module, you pass the component all of the table data at once. In the
      [Expert] module, by contrast, you give it a "collation" -- that is, a filtered,
      sorted, range-restricted window -- of the full table. This can be useful when the
      table data is too large to pass to the client directly, or when you'd like to update
      your table via RPC. *)

  open Incr_map_collate

  module Focus : sig
    module By_row = Focus.By_row

    type ('a, 'p, 'k) t = ('a, 'p, 'k) Focus.Kind.t =
      | None : (unit, unit, 'k) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Value.t
              (** Row-selection is not required to be inside the viewport, so the selected row
              can be offscreen such that it isn't given to the table component. [compute_presence]
              forces the user to consider if a row is considered 'focused' or not. *)
          ; compute_presence : 'k option Value.t -> 'p Computation.t
          }
          -> (('k, 'p) Focus_by_row.t, 'p, 'k) t
  end

  module Result : sig
    type 'focus t =
      { view : Vdom.Node.t
      ; range : int * int
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      }
    [@@deriving fields ~getters]
  end

  module Columns : sig
    type ('key, 'data) t
    type ('key, 'data) columns := ('key, 'data) t

    module Dynamic_cells : sig
      type ('key, 'data) t

      val column
        :  ?initial_width:Css_gen.Length.t
        -> ?visible:bool Value.t
        -> header:Vdom.Node.t Value.t
        -> cell:(key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t)
        -> unit
        -> ('key, 'data) t

      val group : label:Vdom.Node.t Value.t -> ('key, 'data) t list -> ('key, 'data) t
      val lift : ('key, 'data) t list -> ('key, 'data) columns

      (** [Header_helpers] provides helper functions for constructing table headers with
          sort indicators *)
      module Header_helpers = Column.Dynamic_cells.Header_helpers
    end

    module Dynamic_columns : sig
      type ('key, 'data) t

      val column
        :  ?initial_width:Css_gen.Length.t
        -> ?visible:bool
        -> header:Vdom.Node.t
        -> cell:(key:'key -> data:'data -> Vdom.Node.t)
        -> unit
        -> ('key, 'data) t

      val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
      val lift : ('key, 'data) t list Value.t -> ('key, 'data) columns

      (** [Header_helpers] provides helper functions for constructing table headers with
          sort indicators *)
      module Header_helpers = Column.Dynamic_columns.Header_helpers
    end
  end

  val collate
    :  ?operation_order:[ `Filter_first | `Sort_first ]
    -> filter_equal:('filter -> 'filter -> bool)
         (** [filter_equal] is used to decide when the filters have actually changed, requiring
        a recomputation of the collation. *)
    -> order_equal:('order -> 'order -> bool)
         (** [order_equal] is used to decide when the sorting params have actually changed,
        requiring a recomputation of the collation. *)
    -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
         (** [filter_to_predicate] takes the current set of filters ['filter] and optionally
        returns a function that can apply those filters to each row. When
        [filter_to_predicate] returns [None], no filtering is done. *)
    -> order_to_compare:('order -> ('k, 'v, 'cmp) Compare.t)
         (** [order_to_compare] takes the current set of sort params ['order] and uses the
        [Compare] specification to decide how to apply them. Return [Unchanged] to perform
        no sorting. *)
    -> ('k, 'v, 'cmp) Map.t Value.t
       (** A [Map.t] containing the source for all the table data, pre-collation. *)
    -> ('k, 'filter, 'order) Collate.t Value.t
       (** A [Collate.t] is a specification for how to perform collation: it's where the
        ['filter], ['order], and rank range are defined. *)
    -> ('k, 'v) Collated.t Computation.t

  val component
    :  ?preload_rows:int
         (** [preload_rows] is the number of rows that are maintained before and after the
        viewport range. This number can have a significant effect on performance: too
        small and scrolling might be choppy; too large and you start to lose some of the
        benefits of partial rendering. *)
    -> ('key, 'cmp) Bonsai.comparator
    -> focus:('focus, 'presence, 'key) Focus.t
    -> row_height:[ `Px of int ] Value.t
         (** [row_height] is the height of every row in the table. If the row height
        is specified to be 0px or less, we instead use 1px. *)
    -> columns:('key, 'row) Columns.t
    -> ('key, 'row) Collated.t Value.t
       (** The collated value is the proper input to the component.
        You can use [Expert.collate] to get a Collated.t value, or do
        the collation manually on the server by using the Incr_map_collate
        library manually. *)
    -> 'focus Result.t Computation.t
end
