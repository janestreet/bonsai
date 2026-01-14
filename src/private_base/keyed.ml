open! Core

module T = struct
  type t =
    | T :
        { key : 'k
        ; id : 'k Type_equal.Id.t
        ; compare : 'k -> 'k -> int
        }
        -> t

  let compare
    (T { key = key1; id = id1; compare = compare1 })
    (T { key = key2; id = id2; compare = _ })
    =
    match Type_equal.Id.same_witness id1 id2 with
    | Some T -> compare1 key1 key2
    | None ->
      (* Use the Uid comparison function so that the comparator is stable. This function
         will never return 0 because we've already established that these type-ids are not
         equal *)
      Type_equal.Id.Uid.compare (Type_equal.Id.uid id1) (Type_equal.Id.uid id2)
  ;;

  let sexp_of_t (T { key; id; compare = _ }) = Type_equal.Id.to_sexp id key
  let create ~key ~id ~compare = T { key; id; compare }
end

include T
include Comparable.Make_plain (T)
