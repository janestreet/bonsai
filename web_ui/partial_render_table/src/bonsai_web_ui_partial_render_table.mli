open! Core
open! Bonsai_web

module For_testing : sig
  module Table_body = Table_body.For_testing

  type t = { body : Table_body.t }
end

module Focus : sig
  module By_row : sig
    type 'k t =
      { focused : 'k option
      ; unfocus : unit Ui_effect.t
      ; focus_up : unit Ui_effect.t
      ; focus_down : unit Ui_effect.t
      ; page_up : unit Ui_effect.t
      ; page_down : unit Ui_effect.t
      ; focus : 'k -> unit Ui_effect.t
      }
  end

  type ('a, 'k) t =
    | None : (unit, 'k) t
    | By_row : ('k By_row.t, 'k) t
end

module Basic : sig
  module Focus = Focus

  module Result : sig
    type 'focus t =
      { view : Vdom.Node.t
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      }
    [@@deriving fields]
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
        -> ?initial_width:[ `Px of int ]
        -> ?visible:bool Value.t
        (** [visible] can be set to [false] to hide the whole column. *)
        -> label:Vdom.Node.t Value.t
        (** [label] determines the contents of the column header *)
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
        -> ?initial_width:[ `Px of int ]
        -> ?visible:bool (** [visible] can be set to [false] to hide the whole column. *)
        -> label:Vdom.Node.t (** [label] determines the contents of the column header *)
        -> cell:(key:'key -> data:'data -> Vdom.Node.t)
        (** [cell] is the function determines the contents of every cell in this column. *)
        -> unit
        -> ('key, 'data) t

      (** [group ~label children] builds a header-group that has [children] underneath it.
          The content of header-group is set to [label] *)
      val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t

      (** [lift] pulls a list of columns out into a column specification for use in the primary APIs  *)
      val lift : ('key, 'data) t list Value.t -> ('key, 'data) columns
    end
  end

  (** This is the main UI component for the table content. *)
  val component
    :  ?filter:(key:'key -> data:'data -> bool) Value.t
    (** An optional function may be provided, which filters the rows in the table. *)
    -> ?default_sort:('key * 'data -> 'key * 'data -> int) Value.t
    (** An optional function may be provided to sort the table. *)
    -> ?preload_rows:int
    -> ('key, 'cmp) Bonsai.comparator
    -> focus:('focus, 'key) Focus.t
    -> row_height:[ `Px of int ]
    -> columns:('key, 'data) Columns.t
    -> ('key, 'data, 'cmp) Map.t Value.t (** The input data for the table *)
    -> 'focus Result.t Computation.t
end

module Expert : sig
  open Incr_map_collate
  module Memo := Incr_memoize
  module Focus = Focus

  module Result : sig
    type 'focus t =
      { view : Vdom.Node.t
      ; range : int * int
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      }
    [@@deriving fields]
  end

  module Columns : sig
    type ('key, 'data) t
    type ('key, 'data) columns := ('key, 'data) t

    module Dynamic_cells : sig
      type ('key, 'data) t

      val column
        :  ?initial_width:[ `Px of int ]
        -> ?visible:bool Value.t
        -> label:Vdom.Node.t Value.t
        -> cell:(key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t)
        -> unit
        -> ('key, 'data) t

      val group : label:Vdom.Node.t Value.t -> ('key, 'data) t list -> ('key, 'data) t
      val lift : ('key, 'data) t list -> ('key, 'data) columns
    end

    module Dynamic_columns : sig
      type ('key, 'data) t

      val column
        :  ?initial_width:[ `Px of int ]
        -> ?visible:bool
        -> label:Vdom.Node.t
        -> cell:(key:'key -> data:'data -> Vdom.Node.t)
        -> unit
        -> ('key, 'data) t

      val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
      val lift : ('key, 'data) t list Value.t -> ('key, 'data) columns
    end
  end

  val collate
    :  ?operation_order:[ `Filter_first | `Sort_first ]
    -> ?filter_memoize_params:'filter Memo.Store_params.t
    -> ?order_memoize_params:'order Memo.Store_params.t
    -> filter_equal:('filter -> 'filter -> bool)
    -> order_equal:('order -> 'order -> bool)
    -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
    -> order_to_compare:('order -> ('k, 'v, 'cmp) Compare.t)
    -> ('k, 'v, 'cmp) Map.t Value.t
    -> ('k, 'filter, 'order) Collate.t Value.t
    -> ('k, 'v) Collated.t Computation.t

  val component
    :  ?preload_rows:int
    (** [preload_rows] is the number of rows that are maintained before and after the
        viewport range.  *)
    -> ('key, 'cmp) Bonsai.comparator
    -> focus:('focus, 'key) Focus.t
    -> row_height:[ `Px of int ]
    (** [row_height] is the fixed-height of every row in the table. *)
    -> columns:('key, 'row) Columns.t
    -> ('key, 'row) Collated.t Value.t
    (** The collated value is the proper input to the component.
        You can use [Expert.collate] to get a Collated.t value, or do
        the collation manually on the server by using the Incr_map_collate
        library manually. *)
    -> 'focus Result.t Computation.t
end
