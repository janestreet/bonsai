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

module Sort_kind = struct
  type ('key, 'data) sort = 'key * 'data -> 'key * 'data -> int

  type ('key, 'data) t =
    { forward : ('key, 'data) sort
    ; reverse : ('key, 'data) sort
    }

  let reversible ~forward = { forward; reverse = Comparable.reverse forward }
  let reversible' ~reverse = { forward = Comparable.reverse reverse; reverse }
end

module Dir = struct
  include Stable.Dir.V1
end

module Sort_state = struct
  type t =
    | Not_sortable
    | Not_sorted
    | Single_sort of Dir.t
    | Multi_sort of
        { dir : Dir.t
        ; index : int
        }
end

module Order = struct
  include Stable.Order.V1

  module Action = struct
    type 'col_id t =
      | Set_sort of 'col_id
      | Set_sort_with_dir of 'col_id * Dir.t option
      | Add_sort of 'col_id
    [@@deriving sexp_of]
  end

  let apply_action t ~equal (action : 'col_id Action.t) =
    let cycle_sort_direction id =
      match List.Assoc.find ~equal t id with
      | None -> [ id, `Asc ]
      | Some `Asc -> [ id, `Desc ]
      | Some `Desc -> []
    in
    match action with
    | Set_sort id -> cycle_sort_direction id
    | Set_sort_with_dir (id, dir) ->
      (match dir with
       | None -> []
       | Some dir -> [ id, dir ])
    | Add_sort id ->
      (* For multi-column sorting:
         1. Sort columns are added to the end of the list, because users expect to click
         the most significant sort first.
         2. Flipping the direction of an already sorted column will void all of the
         lower-priority sorts. *)
      if List.Assoc.mem t ~equal id
      then (
        let keep = List.take_while t ~f:(fun (id', _dir) -> not (equal id id')) in
        keep @ cycle_sort_direction id)
      else t @ cycle_sort_direction id
  ;;

  let to_compare ?override_sort t ~sorters ~default_sort : _ Incr_map_collate.Compare.t =
    let override = Option.value override_sort ~default:Fn.id in
    match t, default_sort with
    | [], None ->
      (match override_sort with
       | None -> Unchanged
       | Some override -> Custom_by_key_and_value { compare = override (fun _ _ -> 0) })
    | [], Some compare ->
      let compare = override compare in
      Custom_by_key_and_value { compare }
    | t, default_sort ->
      let l =
        List.filter_map t ~f:(fun (id, direction) ->
          let open Option.Let_syntax in
          let%map compare = Map.find sorters id in
          match direction with
          | `Asc -> compare.Sort_kind.forward
          | `Desc -> compare.Sort_kind.reverse)
      in
      let compare =
        List.append l (Option.to_list default_sort)
        |> (fun cmps a b -> Comparable.lexicographic cmps a b)
        |> override
      in
      Custom_by_key_and_value { compare }
  ;;

  let default = []
end
