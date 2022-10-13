open! Core

module type Col_id = sig
  type t [@@deriving equal, sexp, bin_io]
end

module Dir : sig
  type t =
    [ `Asc
    | `Desc
    ]
  [@@deriving sexp, bin_io, equal, compare]
end

module Order : sig
  type 'col_id t = ('col_id * Dir.t) list [@@deriving sexp, bin_io, equal, compare]

  module Action : sig
    (** Simple variant module used as a message for changing ordering. *)
    type 'col_id t =
      | Set_sort of 'col_id
      | Add_sort of 'col_id
    [@@deriving sexp_of]
  end

  (** Performs ordering change. {!Action.Set_sort} sets order on one column;
      {!Action.Add_sort} maintains multiple column ordering, when the newest added
      sorting is the most priority. The order on one column is changed in a cycle fashion:
      [Ascending -> Descending -> None -> Ascending].
  *)
  val apply_action
    :  'col_id t
    -> (module Col_id with type t = 'col_id)
    -> 'col_id Action.t
    -> 'col_id t

  val to_compare
    :  'col_id t
    -> sorters:('col_id, 'key * 'data -> 'key * 'data -> int, 'col_cmp) Map.t
    -> default_sort:('key * 'data -> 'key * 'data -> int) option
    -> ('key, 'data, 'row_cmp) Incr_map_collate.Compare.t

  val default : 'col_id t
end

module Stable : sig
  module Order : sig
    module V1 : sig
      type nonrec 'col_id t = 'col_id Order.t [@@deriving sexp, bin_io, equal, compare]
    end
  end
end
