open! Core

module Dependencies = struct
  module Unit_data = struct
    type 'a t = unit [@@deriving sexp_of]
  end

  include Univ_map.Make (Fix_id) (Unit_data)
  include Univ_map.Merge (Fix_id) (Unit_data) (Unit_data) (Unit_data)

  module Item = struct
    type t = T : 'a Fix_id.t -> t
  end

  let merge a b =
    let f ~key:_ = function
      | `Both ((), ()) | `Left () | `Right () -> Some ()
    in
    merge a b ~f:{ f }
  ;;

  let fold t ~init ~f =
    Map.fold (Type_equal.conv type_equal t) ~init ~f:(fun ~key:_ ~data:(T (k, ())) acc ->
      f acc (Item.T k))
  ;;
end

type resolved = private Resolved [@@deriving sexp_of]
type unresolved = private Unresolved [@@deriving sexp_of]

module Single = struct
  type 'resolution t =
    | Yes_or_maybe : 'a t
    | No : 'a t
    | Depends_on_recursion : unresolved t
  [@@deriving sexp_of]

  let merge_resolved (a : resolved t) (b : resolved t) : resolved t =
    match a, b with
    | Yes_or_maybe, Yes_or_maybe -> Yes_or_maybe
    | Yes_or_maybe, No | No, Yes_or_maybe -> Yes_or_maybe
    | No, No -> No
  ;;

  let merge_unresolved (a : unresolved t) (b : unresolved t) : unresolved t =
    match a, b with
    | Yes_or_maybe, Yes_or_maybe -> Yes_or_maybe
    | Yes_or_maybe, Depends_on_recursion | Depends_on_recursion, Yes_or_maybe ->
      Yes_or_maybe
    | Yes_or_maybe, No | No, Yes_or_maybe -> Yes_or_maybe
    | Depends_on_recursion, Depends_on_recursion -> Depends_on_recursion
    | Depends_on_recursion, No | No, Depends_on_recursion -> Depends_on_recursion
    | No, No -> No
  ;;

  let unresolve (a : resolved t) : unresolved t =
    match a with
    | Yes_or_maybe -> Yes_or_maybe
    | No -> No
  ;;

  (* [resolved_equivalent] is only used during resolution of recursive dependencies. We
     map [Depends_on_recursion] to [No] because [No] is the identity for [resolved t] and
     as part of resolution, we'll merge in all of the recursive dependencies. *)
  let resolved_equivalent (a : unresolved t) : resolved t =
    match a with
    | Yes_or_maybe -> Yes_or_maybe
    | No -> No
    | Depends_on_recursion -> No
  ;;
end

type 'resolution t =
  { path : 'resolution Single.t
  ; lifecycle : 'resolution Single.t
  ; input : 'resolution Single.t
  }
[@@deriving sexp_of]

module Resolved = struct
  type nonrec t = resolved t

  let create ~path ~lifecycle ~input = { path; lifecycle; input }

  let merge (a : t) (b : t) =
    { path = Single.merge_resolved a.path b.path
    ; lifecycle = Single.merge_resolved a.lifecycle b.lifecycle
    ; input = Single.merge_resolved a.input b.input
    }
  ;;

  let both_use_path (a : t) (b : t) : bool =
    match a.path, b.path with
    | Yes_or_maybe, Yes_or_maybe -> true
    | Yes_or_maybe, No | No, Yes_or_maybe | No, No -> false
  ;;

  let path contains = contains.path
  let lifecycle contains = contains.lifecycle
  let input contains = contains.input
end

module Unresolved = struct
  type nonrec t =
    { contains : unresolved t
    ; recursive_dependencies : Dependencies.t
    }

  let merge
    { contains = ca; recursive_dependencies = ra }
    { contains = cb; recursive_dependencies = rb }
    =
    { contains =
        { path = Single.merge_unresolved ca.path cb.path
        ; lifecycle = Single.merge_unresolved ca.lifecycle cb.lifecycle
        ; input = Single.merge_unresolved ca.input cb.input
        }
    ; recursive_dependencies = Dependencies.merge ra rb
    }
  ;;

  let of_resolved (resolved : Resolved.t) =
    { contains =
        { path = Single.unresolve resolved.path
        ; lifecycle = Single.unresolve resolved.lifecycle
        ; input = Single.unresolve resolved.input
        }
    ; recursive_dependencies = Dependencies.empty
    }
  ;;

  let resolved_equivalent { contains; recursive_dependencies = _ } =
    { path = Single.resolved_equivalent contains.path
    ; lifecycle = Single.resolved_equivalent contains.lifecycle
    ; input = Single.resolved_equivalent contains.input
    }
  ;;

  let lazy_ =
    { contains = { path = Yes_or_maybe; lifecycle = Yes_or_maybe; input = Yes_or_maybe }
    ; recursive_dependencies = Dependencies.empty
    }
  ;;

  let recursive fix_id =
    { contains =
        { path = Depends_on_recursion
        ; lifecycle = Depends_on_recursion
        ; input = Depends_on_recursion
        }
    ; recursive_dependencies = Dependencies.singleton fix_id ()
    }
  ;;

  let non_recursive
    ~(path : resolved Single.t)
    ~(lifecycle : resolved Single.t)
    ~(input : resolved Single.t)
    =
    { contains =
        { path = Single.unresolve path
        ; lifecycle = Single.unresolve lifecycle
        ; input = Single.unresolve input
        }
    ; recursive_dependencies = Dependencies.empty
    }
  ;;

  let remove_dependency (t : t) ~on:fix_id =
    { t with
      recursive_dependencies = Dependencies.remove t.recursive_dependencies fix_id
    }
  ;;
end
