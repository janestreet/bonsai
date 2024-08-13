open! Core
open! Import

module T = struct
  include Univ_map.Make (Univ_map.Type_id_key) (Incr)

  let add_overwriting t ~key ~data = update t key ~f:(fun _ -> data)
end

include T

module Recursive = struct
  type entry =
    { environment : T.t
    ; resolved_contain : May_contain.Resolved.t
    }

  include
    Univ_map.Make
      (Fix_id)
      (struct
        type nonrec 'a t = entry

        let sexp_of_t _ = sexp_of_opaque
      end)

  let add_overwriting t ~key ~data = update t key ~f:(fun _ -> data)

  let resolve_may_contain t may_contain =
    let open May_contain in
    let ({ contains; recursive_dependencies } : Unresolved.t) = may_contain in
    match contains with
    (* If all of the fields are already resolved, we don't need iterate over any
       dependencies. *)
    | { path = Yes_or_maybe | No
      ; lifecycle = Yes_or_maybe | No
      ; input = Yes_or_maybe | No
      } -> May_contain.Unresolved.resolved_equivalent may_contain
    | _ ->
      let resolved_contains =
        Dependencies.fold
          recursive_dependencies
          ~init:(May_contain.Unresolved.resolved_equivalent may_contain)
          ~f:(fun acc (T id) ->
            let { resolved_contain; environment = _ } = find_exn t id in
            May_contain.Resolved.merge acc resolved_contain)
      in
      May_contain.Resolved.create
        ~path:resolved_contains.path
        ~lifecycle:resolved_contains.lifecycle
        ~input:resolved_contains.input
  ;;
end
