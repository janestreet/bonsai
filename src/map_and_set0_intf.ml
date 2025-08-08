open! Core
open! Import

module type Input = sig
  module Value : sig
    type 'a t

    val both : ?here:Stdlib.Lexing.position -> 'a t -> 'b t -> ('a * 'b) t
  end

  module Computation : sig
    type 'a t
  end

  module Incr : sig
    val compute
      :  ?here:Stdlib.Lexing.position
      -> 'a Value.t
      -> f:('a Incr.t -> 'b Incr.t)
      -> 'b Computation.t
  end
end

module type Output = sig
  module Value : sig
    type 'a t
  end

  module Computation : sig
    type 'a t
  end

  module Core_map := Core.Map

  module Map : sig
    val mapi
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v1, 'cmp) Map.t Value.t
      -> f:(key:'k -> data:'v1 -> 'v2)
      -> ('k, 'v2, 'cmp) Map.t Computation.t

    val map
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v1, 'cmp) Map.t Value.t
      -> f:('v1 -> 'v2)
      -> ('k, 'v2, 'cmp) Map.t Computation.t

    val of_set
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'cmp) Set.t Value.t
      -> ('k, unit, 'cmp) Map.t Computation.t

    val filter_mapi
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v1, 'cmp) Map.t Value.t
      -> f:(key:'k -> data:'v1 -> 'v2 option)
      -> ('k, 'v2, 'cmp) Map.t Computation.t

    val filter_map
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v1, 'cmp) Map.t Value.t
      -> f:('v1 -> 'v2 option)
      -> ('k, 'v2, 'cmp) Map.t Computation.t

    val partition_mapi
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v1, 'cmp) Map.t Value.t
      -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t)
      -> (('k, 'v2, 'cmp) Map.t * ('k, 'v3, 'cmp) Map.t) Computation.t

    val unordered_fold
      :  ?here:Stdlib.Lexing.position
      -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc)
      -> ?finalize:('acc -> 'acc)
      -> ('k, 'v, 'cmp) Map.t Value.t
      -> init:'acc
      -> add:(key:'k -> data:'v -> 'acc -> 'acc)
      -> remove:(key:'k -> data:'v -> 'acc -> 'acc)
      -> 'acc Computation.t

    val unordered_fold_with_extra
      :  ?here:Stdlib.Lexing.position
      -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'extra -> 'acc)
      -> ('k, 'v, 'e) Map.t Value.t
      -> 'extra Value.t
      -> init:'acc
      -> add:(key:'k -> data:'v -> 'acc -> 'extra -> 'acc)
      -> remove:(key:'k -> data:'v -> 'acc -> 'extra -> 'acc)
      -> extra_changed:
           (old_extra:'extra
            -> new_extra:'extra
            -> input:('k, 'v, 'e) Map.t
            -> 'acc
            -> 'acc)
      -> 'acc Computation.t

    val cutoff
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, 'cmp) Map.t Value.t
      -> equal:('v -> 'v -> bool)
      -> ('k, 'v, 'cmp) Map.t Computation.t

    val mapi_count
      :  ?here:Stdlib.Lexing.position
      -> ('k1, 'v, 'cmp1) Map.t Value.t
      -> comparator:('k2, 'cmp2) Comparator.Module.t
      -> f:(key:'k1 -> data:'v -> 'k2)
      -> ('k2, int, 'cmp2) Map.t Computation.t

    val map_count
      :  ?here:Stdlib.Lexing.position
      -> ('k1, 'v, 'cmp1) Map.t Value.t
      -> comparator:('k2, 'cmp2) Comparator.Module.t
      -> f:('v -> 'k2)
      -> ('k2, int, 'cmp2) Map.t Computation.t

    val mapi_min
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('r, _) Comparator.Module.t
      -> f:(key:'k -> data:'v -> 'r)
      -> 'r option Computation.t

    val mapi_max
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('r, _) Comparator.Module.t
      -> f:(key:'k -> data:'v -> 'r)
      -> 'r option Computation.t

    val map_min
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('r, _) Comparator.Module.t
      -> f:('v -> 'r)
      -> 'r option Computation.t

    val map_max
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('r, _) Comparator.Module.t
      -> f:('v -> 'r)
      -> 'r option Computation.t

    val min_value
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('v, _) Comparator.Module.t
      -> 'v option Computation.t

    val max_value
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('v, _) Comparator.Module.t
      -> 'v option Computation.t

    val mapi_bounds
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('r, _) Comparator.Module.t
      -> f:(key:'k -> data:'v -> 'r)
      -> ('r * 'r) option Computation.t

    val map_bounds
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('r, _) Comparator.Module.t
      -> f:('v -> 'r)
      -> ('r * 'r) option Computation.t

    val value_bounds
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> comparator:('v, _) Comparator.Module.t
      -> ('v * 'v) option Computation.t

    val merge
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v1, 'cmp) Map.t Value.t
      -> ('k, 'v2, 'cmp) Map.t Value.t
      -> f:(key:'k -> ('v1, 'v2) Map.Merge_element.t -> 'v3 option)
      -> ('k, 'v3, 'cmp) Map.t Computation.t

    val merge_both_some
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v1, 'cmp) Map.t Value.t
      -> ('k, 'v2, 'cmp) Map.t Value.t
      -> f:(key:'k -> 'v1 -> 'v2 -> 'v3)
      -> ('k, 'v3, 'cmp) Map.t Computation.t

    val unzip
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'a * 'b, 'cmp) Map.t Value.t
      -> (('k, 'a, 'cmp) Map.t * ('k, 'b, 'cmp) Map.t) Computation.t

    val unzip_mapi
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, 'cmp) Map.t Value.t
      -> f:(key:'k -> data:'v -> 'v1 * 'v2)
      -> (('k, 'v1, 'cmp) Map.t * ('k, 'v2, 'cmp) Map.t) Computation.t

    val keys
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, 'c) Map.t Value.t
      -> ('k, 'c) Set.t Computation.t

    val rank
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, 'cmp) Map.t Value.t
      -> 'k Value.t
      -> int option Computation.t

    val subrange
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, 'cmp) Map.t Value.t
      -> ('k Maybe_bound.As_lower_bound.t * 'k Maybe_bound.As_upper_bound.t) option
           Value.t
      -> ('k, 'v, 'cmp) Map.t Computation.t

    val subrange_by_rank
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, 'cmp) Map.t Value.t
      -> (int Maybe_bound.As_lower_bound.t * int Maybe_bound.As_upper_bound.t) Value.t
      -> ('k, 'v, 'cmp) Map.t Computation.t

    val rekey
      :  ?here:Stdlib.Lexing.position
      -> ('k1, 'v, 'cmp1) Map.t Value.t
      -> comparator:('k2, 'cmp2) Comparator.Module.t
      -> f:(key:'k1 -> data:'v -> 'k2)
      -> ('k2, 'v, 'cmp2) Map.t Computation.t

    val index_byi
      :  ?here:Stdlib.Lexing.position
      -> ('inner_key, 'v, 'inner_cmp) Map.t Value.t
      -> comparator:('outer_key, 'outer_cmp) Comparator.Module.t
      -> index:(key:'inner_key -> data:'v -> 'outer_key option)
      -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Computation.t

    val index_by
      :  ?here:Stdlib.Lexing.position
      -> ('inner_key, 'v, 'inner_cmp) Map.t Value.t
      -> comparator:('outer_key, 'outer_cmp) Comparator.Module.t
      -> index:('v -> 'outer_key option)
      -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Computation.t

    val unordered_fold_nested_maps
      :  ?here:Stdlib.Lexing.position
      -> ?update:
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
      :  ?here:Stdlib.Lexing.position
      -> ('k2, 'k2_cmp) Comparator.Module.t
      -> ('k1, ('k2, 'v, 'k2_cmp) Map.t, 'k1_cmp) Map.t Value.t
      -> ('k2, ('k1, 'v, 'k1_cmp) Map.t, 'k2_cmp) Map.t Computation.t

    val collapse
      :  ?here:Stdlib.Lexing.position
      -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Value.t
      -> comparator:('inner_key, 'inner_cmp) Comparator.Module.t
      -> ( 'outer_key * 'inner_key
           , 'v
           , ('outer_cmp, 'inner_cmp) Tuple2.comparator_witness )
           Map.t
           Computation.t

    val collapse_by
      :  ?here:Stdlib.Lexing.position
      -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Value.t
      -> merge_keys:('outer_key -> 'inner_key -> 'combined_key)
      -> comparator:('combined_key, 'combined_cmp) Comparator.Module.t
      -> ('combined_key, 'v, 'combined_cmp) Map.t Computation.t

    val expand
      :  ?here:Stdlib.Lexing.position
      -> ('outer_key * 'inner_key, 'v, 'tuple_cmp) Map.t Value.t
      -> outer_comparator:('outer_key, 'outer_cmp) Comparator.Module.t
      -> inner_comparator:('inner_key, 'inner_cmp) Comparator.Module.t
      -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Computation.t

    val counti
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> f:(key:'k -> data:'v -> bool)
      -> int Computation.t

    val count
      :  ?here:Stdlib.Lexing.position
      -> (_, 'v, _) Map.t Value.t
      -> f:('v -> bool)
      -> int Computation.t

    val for_alli
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> f:(key:'k -> data:'v -> bool)
      -> bool Computation.t

    val for_all
      :  ?here:Stdlib.Lexing.position
      -> (_, 'v, _) Map.t Value.t
      -> f:('v -> bool)
      -> bool Computation.t

    val existsi
      :  ?here:Stdlib.Lexing.position
      -> ('k, 'v, _) Map.t Value.t
      -> f:(key:'k -> data:'v -> bool)
      -> bool Computation.t

    val exists
      :  ?here:Stdlib.Lexing.position
      -> (_, 'v, _) Map.t Value.t
      -> f:('v -> bool)
      -> bool Computation.t

    val sum
      :  ?here:Stdlib.Lexing.position
      -> (_, 'v, _) Map.t Value.t
      -> (module Abstract_algebra.Commutative_group.Without_sexp with type t = 'u)
      -> f:('v -> 'u)
      -> 'u Computation.t
  end

  module Set : sig
    module Map := Core_map

    val union
      :  ?here:Stdlib.Lexing.position
      -> ('a, 'cmp) Set.t Value.t
      -> ('a, 'cmp) Set.t Value.t
      -> ('a, 'cmp) Set.t Computation.t

    val inter
      :  ?here:Stdlib.Lexing.position
      -> ('a, 'cmp) Set.t Value.t
      -> ('a, 'cmp) Set.t Value.t
      -> ('a, 'cmp) Set.t Computation.t

    val diff
      :  ?here:Stdlib.Lexing.position
      -> ('a, 'cmp) Set.t Value.t
      -> ('a, 'cmp) Set.t Value.t
      -> ('a, 'cmp) Set.t Computation.t

    val filter
      :  ?here:Stdlib.Lexing.position
      -> ('a, 'cmp) Set.t Value.t
      -> f:('a -> bool)
      -> ('a, 'cmp) Set.t Computation.t

    val unordered_fold
      :  ?here:Stdlib.Lexing.position
      -> ('a, 'cmp) Set.t Value.t
      -> init:'acc
      -> add:('acc -> 'a -> 'acc)
      -> remove:('acc -> 'a -> 'acc)
      -> 'acc Computation.t

    val cartesian_product
      :  ?here:Stdlib.Lexing.position
      -> ('a1, 'cmp1) Set.t Value.t
      -> ('a2, 'cmp2) Set.t Value.t
      -> ('a1 * 'a2, ('cmp1, 'cmp2) Tuple2.comparator_witness) Set.t Computation.t

    val union_map_data
      :  ?here:Stdlib.Lexing.position
      -> comparator:('a, 'cmp) Comparator.t
      -> (_, ('a, 'cmp) Set.t, _) Map.t Value.t
      -> ('a, 'cmp) Set.t Computation.t
  end
end

module Map0 = struct
  module type S = sig
    module Make (Input : Input) : sig
      include
        Output
        with module Value := Input.Value
         and module Computation := Input.Computation
    end
  end
end
