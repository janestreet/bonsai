open! Core_kernel

include
  Univ_map.S1
  with module Key = Univ_map.Type_id_key
   and type ('w, 'a) data := ('a, 'w) Incremental.t
