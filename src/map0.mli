open! Core
open! Import

val mapi : [ `Use_assoc ]
val map : [ `Use_assoc ]
val of_set : ('k, 'cmp) Set.t Value.t -> ('k, unit, 'cmp) Map.t Computation.t

val filter_mapi
  :  ('k, 'v1, 'cmp) Map.t Value.t
  -> f:(key:'k -> data:'v1 -> 'v2 option)
  -> ('k, 'v2, 'cmp) Map.t Computation.t

val filter_map
  :  ('k, 'v1, 'cmp) Map.t Value.t
  -> f:('v1 -> 'v2 option)
  -> ('k, 'v2, 'cmp) Map.t Computation.t

val partition_mapi
  :  ('k, 'v1, 'cmp) Map.t Value.t
  -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t)
  -> (('k, 'v2, 'cmp) Map.t * ('k, 'v3, 'cmp) Map.t) Computation.t

val unordered_fold
  :  ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc)
  -> ('k, 'v, 'cmp) Map.t Value.t
  -> init:'acc
  -> add:(key:'k -> data:'v -> 'acc -> 'acc)
  -> remove:(key:'k -> data:'v -> 'acc -> 'acc)
  -> 'acc Computation.t

val unordered_fold_with_extra
  :  ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'extra -> 'acc)
  -> ('k, 'v, 'e) Map.t Value.t
  -> 'extra Value.t
  -> init:'acc
  -> add:(key:'k -> data:'v -> 'acc -> 'extra -> 'acc)
  -> remove:(key:'k -> data:'v -> 'acc -> 'extra -> 'acc)
  -> extra_changed:
       (old_extra:'extra -> new_extra:'extra -> input:('k, 'v, 'e) Map.t -> 'acc -> 'acc)
  -> 'acc Computation.t

val cutoff
  :  ('k, 'v, 'cmp) Map.t Value.t
  -> equal:('v -> 'v -> bool)
  -> ('k, 'v, 'cmp) Map.t Computation.t

val mapi_count
  :  ('k1, 'v, 'cmp1) Map.t Value.t
  -> comparator:('k2, 'cmp2) Module_types.comparator
  -> f:(key:'k1 -> data:'v -> 'k2)
  -> ('k2, int, 'cmp2) Map.t Computation.t

val map_count
  :  ('k1, 'v, 'cmp1) Map.t Value.t
  -> comparator:('k2, 'cmp2) Module_types.comparator
  -> f:('v -> 'k2)
  -> ('k2, int, 'cmp2) Map.t Computation.t

val mapi_min
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('r, _) Module_types.comparator
  -> f:(key:'k -> data:'v -> 'r)
  -> 'r option Computation.t

val mapi_max
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('r, _) Module_types.comparator
  -> f:(key:'k -> data:'v -> 'r)
  -> 'r option Computation.t

val map_min
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('r, _) Module_types.comparator
  -> f:('v -> 'r)
  -> 'r option Computation.t

val map_max
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('r, _) Module_types.comparator
  -> f:('v -> 'r)
  -> 'r option Computation.t

val min_value
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('v, _) Module_types.comparator
  -> 'v option Computation.t

val max_value
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('v, _) Module_types.comparator
  -> 'v option Computation.t

val mapi_bounds
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('r, _) Module_types.comparator
  -> f:(key:'k -> data:'v -> 'r)
  -> ('r * 'r) option Computation.t

val map_bounds
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('r, _) Module_types.comparator
  -> f:('v -> 'r)
  -> ('r * 'r) option Computation.t

val value_bounds
  :  ('k, 'v, _) Map.t Value.t
  -> comparator:('v, _) Module_types.comparator
  -> ('v * 'v) option Computation.t

val merge
  :  ('k, 'v1, 'cmp) Map.t Value.t
  -> ('k, 'v2, 'cmp) Map.t Value.t
  -> f:(key:'k -> ('v1, 'v2) Map.Merge_element.t -> 'v3 option)
  -> ('k, 'v3, 'cmp) Map.t Computation.t

val merge_both_some
  :  ('k, 'v1, 'cmp) Map.t Value.t
  -> ('k, 'v2, 'cmp) Map.t Value.t
  -> f:(key:'k -> 'v1 -> 'v2 -> 'v3)
  -> ('k, 'v3, 'cmp) Map.t Computation.t

val unzip
  :  ('k, 'a * 'b, 'cmp) Map.t Value.t
  -> (('k, 'a, 'cmp) Map.t * ('k, 'b, 'cmp) Map.t) Computation.t

val unzip_mapi
  :  ('k, 'v, 'cmp) Map.t Value.t
  -> f:(key:'k -> data:'v -> 'v1 * 'v2)
  -> (('k, 'v1, 'cmp) Map.t * ('k, 'v2, 'cmp) Map.t) Computation.t

val keys : ('k, 'v, 'c) Map.t Value.t -> ('k, 'c) Set.t Computation.t
val rank : ('k, 'v, 'cmp) Map.t Value.t -> 'k Value.t -> int option Computation.t

val subrange
  :  ('k, 'v, 'cmp) Map.t Value.t
  -> ('k Maybe_bound.As_lower_bound.t * 'k Maybe_bound.As_upper_bound.t) option Value.t
  -> ('k, 'v, 'cmp) Map.t Computation.t

val subrange_by_rank
  :  ('k, 'v, 'cmp) Map.t Value.t
  -> (int Maybe_bound.As_lower_bound.t * int Maybe_bound.As_upper_bound.t) Value.t
  -> ('k, 'v, 'cmp) Map.t Computation.t

val rekey
  :  ('k1, 'v, 'cmp1) Map.t Value.t
  -> comparator:('k2, 'cmp2) Module_types.comparator
  -> f:(key:'k1 -> data:'v -> 'k2)
  -> ('k2, 'v, 'cmp2) Map.t Computation.t

val index_byi
  :  ('inner_key, 'v, 'inner_cmp) Map.t Value.t
  -> comparator:('outer_key, 'outer_cmp) Module_types.comparator
  -> index:(key:'inner_key -> data:'v -> 'outer_key option)
  -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Computation.t

val index_by
  :  ('inner_key, 'v, 'inner_cmp) Map.t Value.t
  -> comparator:('outer_key, 'outer_cmp) Module_types.comparator
  -> index:('v -> 'outer_key option)
  -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Computation.t

val unordered_fold_nested_maps
  :  ?update:
    (outer_key:'outer_key
     -> inner_key:'inner_key
     -> old_data:'v
     -> new_data:'v
     -> 'acc
     -> 'acc)
  -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Value.t
  -> init:'acc
  -> add:(outer_key:'outer_key -> inner_key:'inner_key -> data:'v -> 'acc -> 'acc)
  -> remove:(outer_key:'outer_key -> inner_key:'inner_key -> data:'v -> 'acc -> 'acc)
  -> 'acc Computation.t

val transpose
  :  ('k2, 'k2_cmp) Module_types.comparator
  -> ('k1, ('k2, 'v, 'k2_cmp) Map.t, 'k1_cmp) Map.t Value.t
  -> ('k2, ('k1, 'v, 'k1_cmp) Map.t, 'k2_cmp) Map.t Computation.t

val collapse
  :  ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Value.t
  -> comparator:('inner_key, 'inner_cmp) Module_types.comparator
  -> ( 'outer_key * 'inner_key
     , 'v
     , ('outer_cmp, 'inner_cmp) Tuple2.comparator_witness )
       Map.t
       Computation.t

val collapse_by
  :  ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Value.t
  -> merge_keys:('outer_key -> 'inner_key -> 'combined_key)
  -> comparator:('combined_key, 'combined_cmp) Module_types.comparator
  -> ('combined_key, 'v, 'combined_cmp) Map.t Computation.t

val expand
  :  ('outer_key * 'inner_key, 'v, 'tuple_cmp) Map.t Value.t
  -> outer_comparator:('outer_key, 'outer_cmp) Module_types.comparator
  -> inner_comparator:('inner_key, 'inner_cmp) Module_types.comparator
  -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Computation.t

val counti
  :  ('k, 'v, _) Map.t Value.t
  -> f:(key:'k -> data:'v -> bool)
  -> int Computation.t

val count : (_, 'v, _) Map.t Value.t -> f:('v -> bool) -> int Computation.t

val for_alli
  :  ('k, 'v, _) Map.t Value.t
  -> f:(key:'k -> data:'v -> bool)
  -> bool Computation.t

val for_all : (_, 'v, _) Map.t Value.t -> f:('v -> bool) -> bool Computation.t

val existsi
  :  ('k, 'v, _) Map.t Value.t
  -> f:(key:'k -> data:'v -> bool)
  -> bool Computation.t

val exists : (_, 'v, _) Map.t Value.t -> f:('v -> bool) -> bool Computation.t

val sum
  :  (_, 'v, _) Map.t Value.t
  -> (module Abstract_algebra.Commutative_group.Without_sexp with type t = 'u)
  -> f:('v -> 'u)
  -> 'u Computation.t
