open! Core
open! Import
include Map_and_set0_intf

module Make (Input : Input) : sig
  include
    Output with module Value := Input.Value and module Computation := Input.Computation
end = struct
  open Input

  module Map = struct
    let map ~(here : [%call_pos]) m ~f = Incr.compute ~here m ~f:(Incr_map.map ~f)
    let mapi ~(here : [%call_pos]) m ~f = Incr.compute ~here m ~f:(Incr_map.mapi ~f)
    let of_set ~(here : [%call_pos]) set = Incr.compute ~here ~f:Incr_map.of_set set

    let filter_mapi ~(here : [%call_pos]) m ~f =
      Incr.compute ~here m ~f:(Incr_map.filter_mapi ~f)
    ;;

    let filter_map ~(here : [%call_pos]) m ~f =
      Incr.compute ~here m ~f:(Incr_map.filter_map ~f)
    ;;

    let partition_mapi ~(here : [%call_pos]) m ~f =
      Incr.compute ~here m ~f:(Incr_map.partition_mapi ~f)
    ;;

    let unordered_fold ~(here : [%call_pos]) ?update ?finalize m ~init ~add ~remove =
      Incr.compute
        ~here
        m
        ~f:(Incr_map.unordered_fold ?update ?finalize ~init ~add ~remove)
    ;;

    let unordered_fold_with_extra
      ~(here : [%call_pos])
      ?update
      m
      e
      ~init
      ~add
      ~remove
      ~extra_changed
      =
      Incr.compute ~here (Value.both ~here m e) ~f:(fun m_and_e ->
        let%pattern_bind.Ui_incr m, e = m_and_e in
        Incr_map.unordered_fold_with_extra ?update m e ~init ~add ~remove ~extra_changed)
    ;;

    let cutoff ~(here : [%call_pos]) m ~equal =
      Incr.compute ~here m ~f:(Incr_map.cutoff ~cutoff:(Ui_incr.Cutoff.of_equal equal))
    ;;

    let mapi_count
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.mapi_count ~comparator:(module M) ~f)
    ;;

    let map_count
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.map_count ~comparator:(module M) ~f)
    ;;

    let mapi_min
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.mapi_min ~comparator:(module M) ~f)
    ;;

    let mapi_max
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.mapi_max ~comparator:(module M) ~f)
    ;;

    let map_min
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.map_min ~comparator:(module M) ~f)
    ;;

    let map_max
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.map_max ~comparator:(module M) ~f)
    ;;

    let min_value
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      =
      Incr.compute ~here m ~f:(Incr_map.min_value ~comparator:(module M))
    ;;

    let max_value
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      =
      Incr.compute ~here m ~f:(Incr_map.max_value ~comparator:(module M))
    ;;

    let mapi_bounds
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.mapi_bounds ~comparator:(module M) ~f)
    ;;

    let map_bounds
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.map_bounds ~comparator:(module M) ~f)
    ;;

    let value_bounds
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      =
      Incr.compute ~here m ~f:(Incr_map.value_bounds ~comparator:(module M))
    ;;

    let merge ~(here : [%call_pos]) a b ~f =
      Incr.compute ~here (Value.both ~here a b) ~f:(fun a_and_b ->
        let%pattern_bind.Ui_incr a, b = a_and_b in
        Incr_map.merge a b ~f)
    ;;

    let merge_both_some ~(here : [%call_pos]) a b ~f =
      Incr.compute ~here (Value.both ~here a b) ~f:(fun a_and_b ->
        let%pattern_bind.Ui_incr a, b = a_and_b in
        Incr_map.merge_both_some a b ~f)
    ;;

    let unzip ~(here : [%call_pos]) m =
      Incr.compute ~here m ~f:(fun m ->
        let l, r = Incr_map.unzip m in
        Ui_incr.both l r)
    ;;

    let unzip_mapi ~(here : [%call_pos]) m ~f =
      Incr.compute ~here m ~f:(fun m ->
        let l, r = Incr_map.unzip_mapi m ~f in
        Ui_incr.both l r)
    ;;

    let keys ~(here : [%call_pos]) m = Incr.compute ~here ~f:Incr_map.keys m

    let rank ~(here : [%call_pos]) m k =
      Incr.compute ~here (Value.both ~here m k) ~f:(fun m_and_k ->
        let%pattern_bind.Ui_incr m, k = m_and_k in
        Incr_map.rank m k)
    ;;

    let subrange ~(here : [%call_pos]) m bounds =
      Incr.compute ~here (Value.both ~here m bounds) ~f:(fun m_and_bounds ->
        let%pattern_bind.Ui_incr m, bounds = m_and_bounds in
        Incr_map.subrange m bounds)
    ;;

    let subrange_by_rank ~(here : [%call_pos]) m bounds =
      Incr.compute ~here (Value.both ~here m bounds) ~f:(fun m_and_bounds ->
        let%pattern_bind.Ui_incr m, bounds = m_and_bounds in
        Incr_map.subrange_by_rank m bounds)
    ;;

    let rekey
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~f
      =
      Incr.compute ~here m ~f:(Incr_map.rekey ~comparator:(module M) ~f)
    ;;

    let index_byi
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~index
      =
      Incr.compute ~here m ~f:(Incr_map.index_byi ~comparator:(module M) ~index)
    ;;

    let index_by
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      ~index
      =
      Incr.compute ~here m ~f:(Incr_map.index_by ~comparator:(module M) ~index)
    ;;

    let unordered_fold_nested_maps ~(here : [%call_pos]) ?update m ~init ~add ~remove =
      Incr.compute
        ~here
        m
        ~f:(Incr_map.unordered_fold_nested_maps ?update ~init ~add ~remove)
    ;;

    let transpose
      (type k cmp)
      ~(here : [%call_pos])
      ((module M) : (k, cmp) Comparator.Module.t)
      m
      =
      Incr.compute ~here m ~f:(Incr_map.transpose (module M))
    ;;

    let collapse
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      =
      Incr.compute ~here m ~f:(Incr_map.collapse ~comparator:(module M))
    ;;

    let collapse_by
      (type k cmp)
      ~(here : [%call_pos])
      m
      ~merge_keys
      ~comparator:((module M) : (k, cmp) Comparator.Module.t)
      =
      Incr.compute ~here m ~f:(Incr_map.collapse_by ~comparator:(module M) ~merge_keys)
    ;;

    let expand
      (type k k2 cmp cmp2)
      ~(here : [%call_pos])
      m
      ~outer_comparator:((module M_outer) : (k, cmp) Comparator.Module.t)
      ~inner_comparator:((module M_inner) : (k2, cmp2) Comparator.Module.t)
      =
      Incr.compute
        ~here
        m
        ~f:
          (Incr_map.expand
             ~inner_comparator:(module M_inner)
             ~outer_comparator:(module M_outer))
    ;;

    let counti ~(here : [%call_pos]) m ~f = Incr.compute ~here m ~f:(Incr_map.counti ~f)
    let count ~(here : [%call_pos]) m ~f = Incr.compute ~here m ~f:(Incr_map.count ~f)

    let for_alli ~(here : [%call_pos]) m ~f =
      Incr.compute ~here m ~f:(Incr_map.for_alli ~f)
    ;;

    let for_all ~(here : [%call_pos]) m ~f = Incr.compute ~here m ~f:(Incr_map.for_all ~f)
    let existsi ~(here : [%call_pos]) m ~f = Incr.compute ~here m ~f:(Incr_map.existsi ~f)
    let exists ~(here : [%call_pos]) m ~f = Incr.compute ~here m ~f:(Incr_map.exists ~f)

    let sum ~(here : [%call_pos]) m algebra ~f =
      Incr.compute ~here m ~f:(fun m -> Incr_map.sum m algebra ~f)
    ;;
  end

  module Set = struct
    (* Proxy a binary function to Incr and back. *)
    let incr_binary_fn ~here a b ~f =
      Incr.compute ~here (Value.both ~here a b) ~f:(fun a_and_b ->
        let%pattern_bind.Ui_incr a, b = a_and_b in
        f a b)
    ;;

    let union ~(here : [%call_pos]) a b = incr_binary_fn ~here ~f:Incr_set.union a b
    let inter ~(here : [%call_pos]) a b = incr_binary_fn ~here ~f:Incr_set.inter a b
    let diff ~(here : [%call_pos]) a b = incr_binary_fn ~here ~f:Incr_set.diff a b

    let filter ~(here : [%call_pos]) set ~f =
      Incr.compute ~here set ~f:(Incr_set.filter ~f)
    ;;

    let unordered_fold ~(here : [%call_pos]) set ~init ~add ~remove =
      Incr.compute ~here set ~f:(Incr_set.unordered_fold ~init ~add ~remove)
    ;;

    let cartesian_product ~(here : [%call_pos]) a b =
      incr_binary_fn ~here ~f:Incr_set.cartesian_product a b
    ;;

    let union_map_data ~(here : [%call_pos]) ~comparator map =
      Incr.compute ~here map ~f:(fun map -> Incr_set.union_map_data ~comparator map)
    ;;
  end
end
