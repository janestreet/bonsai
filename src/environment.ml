open! Core_kernel
open! Import
include Univ_map.Make (Univ_map.Type_id_key) (Incr)

let add_exn t ~key ~data = add_exn t key data
