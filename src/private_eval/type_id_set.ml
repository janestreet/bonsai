open! Core
open! Import

module T =
  Univ_map.Make
    (Univ_map.Type_id_key)
    (struct
      type 'a t = unit

      let sexp_of_t _ = sexp_of_unit
    end)

type t = T.t
type 'b mapper = { f : 'a. 'a Type_equal.Id.t -> 'b }
type 'acc folder = { f : 'a. 'acc -> 'a Type_equal.Id.t -> 'acc }

let empty = T.empty
let singleton a = T.singleton a ()
let is_empty = T.is_empty
let length t = Map.length (Type_equal.conv T.type_equal t)

let iter t ({ f } : unit mapper) =
  Map.iter (Type_equal.conv T.type_equal t) ~f:(fun (T (key, ())) -> f key)
;;

let add t id = T.set t ~key:id ~data:()
let remove t id = T.remove t id
let union a b = List.fold (T.to_alist b) ~init:a ~f:(fun acc (T (id, ())) -> add acc id)

let fold t ~init ({ f } : _ folder) =
  Map.fold
    (Type_equal.conv T.type_equal t)
    ~init
    ~f:(fun ~key:_ ~data:(T (key, ())) acc -> f acc key)
;;

let map_to_list t ({ f } : _ mapper) =
  List.map (T.to_alist t) ~f:(fun (T (key, ())) -> f key)
;;
