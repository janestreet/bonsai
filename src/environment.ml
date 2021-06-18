open! Core
open! Import
include Univ_map.Make (Univ_map.Type_id_key) (Incr)

let add_overwriting t ~key ~data = update t key ~f:(fun _ -> data)
