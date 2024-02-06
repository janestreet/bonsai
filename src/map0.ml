open! Core
open! Import
include Map0_intf

module Make (Input : Input) : sig
  include
    Output with module Value := Input.Value and module Computation := Input.Computation
end = struct
  open Input

  let map m ~f = Incr.compute m ~f:(Incr_map.map ~f)
  let mapi m ~f = Incr.compute m ~f:(Incr_map.mapi ~f)
  let of_set = Incr.compute ~f:Incr_map.of_set
  let filter_mapi m ~f = Incr.compute m ~f:(Incr_map.filter_mapi ~f)
  let filter_map m ~f = Incr.compute m ~f:(Incr_map.filter_map ~f)
  let partition_mapi m ~f = Incr.compute m ~f:(Incr_map.partition_mapi ~f)

  let unordered_fold ?update m ~init ~add ~remove =
    Incr.compute m ~f:(Incr_map.unordered_fold ?update ~init ~add ~remove)
  ;;

  let unordered_fold_with_extra ?update m e ~init ~add ~remove ~extra_changed =
    Incr.compute (Value.both m e) ~f:(fun m_and_e ->
      let%pattern_bind.Ui_incr m, e = m_and_e in
      Incr_map.unordered_fold_with_extra ?update m e ~init ~add ~remove ~extra_changed)
  ;;

  let cutoff m ~equal =
    Incr.compute m ~f:(Incr_map.cutoff ~cutoff:(Ui_incr.Cutoff.of_equal equal))
  ;;

  let mapi_count
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~f
    =
    Incr.compute m ~f:(Incr_map.mapi_count ~comparator:(module M) ~f)
  ;;

  let map_count
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~f
    =
    Incr.compute m ~f:(Incr_map.map_count ~comparator:(module M) ~f)
  ;;

  let mapi_min
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~f
    =
    Incr.compute m ~f:(Incr_map.mapi_min ~comparator:(module M) ~f)
  ;;

  let mapi_max
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~f
    =
    Incr.compute m ~f:(Incr_map.mapi_max ~comparator:(module M) ~f)
  ;;

  let map_min
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~f
    =
    Incr.compute m ~f:(Incr_map.map_min ~comparator:(module M) ~f)
  ;;

  let map_max
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~f
    =
    Incr.compute m ~f:(Incr_map.map_max ~comparator:(module M) ~f)
  ;;

  let min_value (type k cmp) m ~comparator:((module M) : (k, cmp) Module_types.comparator)
    =
    Incr.compute m ~f:(Incr_map.min_value ~comparator:(module M))
  ;;

  let max_value (type k cmp) m ~comparator:((module M) : (k, cmp) Module_types.comparator)
    =
    Incr.compute m ~f:(Incr_map.max_value ~comparator:(module M))
  ;;

  let mapi_bounds
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~f
    =
    Incr.compute m ~f:(Incr_map.mapi_bounds ~comparator:(module M) ~f)
  ;;

  let map_bounds
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~f
    =
    Incr.compute m ~f:(Incr_map.map_bounds ~comparator:(module M) ~f)
  ;;

  let value_bounds
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    =
    Incr.compute m ~f:(Incr_map.value_bounds ~comparator:(module M))
  ;;

  let merge a b ~f =
    Incr.compute (Value.both a b) ~f:(fun a_and_b ->
      let%pattern_bind.Ui_incr a, b = a_and_b in
      Incr_map.merge a b ~f)
  ;;

  let merge_both_some a b ~f =
    Incr.compute (Value.both a b) ~f:(fun a_and_b ->
      let%pattern_bind.Ui_incr a, b = a_and_b in
      Incr_map.merge_both_some a b ~f)
  ;;

  let unzip m =
    Incr.compute m ~f:(fun m ->
      let l, r = Incr_map.unzip m in
      Ui_incr.both l r)
  ;;

  let unzip_mapi m ~f =
    Incr.compute m ~f:(fun m ->
      let l, r = Incr_map.unzip_mapi m ~f in
      Ui_incr.both l r)
  ;;

  let keys = Incr.compute ~f:Incr_map.keys

  let rank m k =
    Incr.compute (Value.both m k) ~f:(fun m_and_k ->
      let%pattern_bind.Ui_incr m, k = m_and_k in
      Incr_map.rank m k)
  ;;

  let subrange m bounds =
    Incr.compute (Value.both m bounds) ~f:(fun m_and_bounds ->
      let%pattern_bind.Ui_incr m, bounds = m_and_bounds in
      Incr_map.subrange m bounds)
  ;;

  let subrange_by_rank m bounds =
    Incr.compute (Value.both m bounds) ~f:(fun m_and_bounds ->
      let%pattern_bind.Ui_incr m, bounds = m_and_bounds in
      Incr_map.subrange_by_rank m bounds)
  ;;

  let rekey (type k cmp) m ~comparator:((module M) : (k, cmp) Module_types.comparator) ~f =
    Incr.compute m ~f:(Incr_map.rekey ~comparator:(module M) ~f)
  ;;

  let index_byi
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~index
    =
    Incr.compute m ~f:(Incr_map.index_byi ~comparator:(module M) ~index)
  ;;

  let index_by
    (type k cmp)
    m
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    ~index
    =
    Incr.compute m ~f:(Incr_map.index_by ~comparator:(module M) ~index)
  ;;

  let unordered_fold_nested_maps ?update m ~init ~add ~remove =
    Incr.compute m ~f:(Incr_map.unordered_fold_nested_maps ?update ~init ~add ~remove)
  ;;

  let transpose (type k cmp) ((module M) : (k, cmp) Module_types.comparator) m =
    Incr.compute m ~f:(Incr_map.transpose (module M))
  ;;

  let collapse (type k cmp) m ~comparator:((module M) : (k, cmp) Module_types.comparator) =
    Incr.compute m ~f:(Incr_map.collapse ~comparator:(module M))
  ;;

  let collapse_by
    (type k cmp)
    m
    ~merge_keys
    ~comparator:((module M) : (k, cmp) Module_types.comparator)
    =
    Incr.compute m ~f:(Incr_map.collapse_by ~comparator:(module M) ~merge_keys)
  ;;

  let expand
    (type k k2 cmp cmp2)
    m
    ~outer_comparator:((module M_outer) : (k, cmp) Module_types.comparator)
    ~inner_comparator:((module M_inner) : (k2, cmp2) Module_types.comparator)
    =
    Incr.compute
      m
      ~f:
        (Incr_map.expand
           ~inner_comparator:(module M_inner)
           ~outer_comparator:(module M_outer))
  ;;

  let counti m ~f = Incr.compute m ~f:(Incr_map.counti ~f)
  let count m ~f = Incr.compute m ~f:(Incr_map.count ~f)
  let for_alli m ~f = Incr.compute m ~f:(Incr_map.for_alli ~f)
  let for_all m ~f = Incr.compute m ~f:(Incr_map.for_all ~f)
  let existsi m ~f = Incr.compute m ~f:(Incr_map.existsi ~f)
  let exists m ~f = Incr.compute m ~f:(Incr_map.exists ~f)
  let sum m algebra ~f = Incr.compute m ~f:(fun m -> Incr_map.sum m algebra ~f)
end
