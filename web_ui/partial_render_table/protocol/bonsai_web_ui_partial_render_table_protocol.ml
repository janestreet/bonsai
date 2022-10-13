open Core

module Stable = struct
  open Core.Core_stable

  module Dir = struct
    module V1 = struct
      type t =
        [ `Asc
        | `Desc
        ]
      [@@deriving sexp, bin_io, equal, compare]
    end
  end

  module Order = struct
    module V1 = struct
      type 'col_id t = ('col_id * Dir.V1.t) list [@@deriving sexp, bin_io, equal, compare]
    end
  end
end

module type Col_id = sig
  type t [@@deriving equal, sexp, bin_io]
end

module Dir = struct
  include Stable.Dir.V1
end

module Order = struct
  include Stable.Order.V1

  module Action = struct
    type 'col_id t =
      | Set_sort of 'col_id
      | Add_sort of 'col_id
    [@@deriving sexp_of]
  end

  let apply_action
        (type col_id)
        t
        (module Col_id : Col_id with type t = col_id)
        (action : col_id Action.t)
    =
    let equal = Col_id.equal in
    let cycle_sort_direction id =
      match List.Assoc.find ~equal t id with
      | None -> [ id, `Asc ]
      | Some `Asc -> [ id, `Desc ]
      | Some `Desc -> []
    in
    match action with
    | Set_sort id -> cycle_sort_direction id
    | Add_sort id -> cycle_sort_direction id @ List.Assoc.remove ~equal t id
  ;;

  let to_compare t ~sorters ~default_sort : _ Incr_map_collate.Compare.t =
    match t, default_sort with
    | [], None -> Unchanged
    | [], Some compare -> Custom_by_key_and_value { compare }
    | t, default_sort ->
      let l =
        List.filter_map t ~f:(fun (id, direction) ->
          let open Option.Let_syntax in
          let%map compare = Map.find sorters id in
          match direction with
          | `Asc -> compare
          | `Desc -> Comparable.reverse compare)
      in
      let compare =
        List.append l (Option.to_list default_sort) |> Comparable.lexicographic
      in
      Custom_by_key_and_value { compare }
  ;;

  let default = []
end
