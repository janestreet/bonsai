open! Core
open! Import

let () = Incr.State.(set_max_height_allowed t 1024)

let rec gather : type result. result Computation.gather_fun =
  let open Computation in
  fun ~recursive_scopes ~time_source -> function
    | Return { value; here = _ } -> Eval_return.f ~value
    | Leaf1 { model; input_id; dynamic_action; apply_action; input; reset; here = _ } ->
      Eval_leaf1.f
        ~model
        ~input_id
        ~dynamic_action
        ~apply_action
        ~input
        ~reset
        ~time_source
    | Leaf0 { model; static_action; apply_action; reset; here = _ } ->
      Eval_leaf0.f ~model ~static_action ~time_source ~apply_action ~reset
    | Leaf_incr { input; compute; here = _ } ->
      Eval_leaf_incr.f ~input ~compute ~time_source
    | Sub { into = Sub { into = Sub _; _ }; _ } as t ->
      Eval_sub.chain t ~gather:{ f = gather } ~recursive_scopes ~time_source
    | Sub { from; via; into; here } ->
      let%bind.Trampoline (T info_from) = gather ~recursive_scopes ~time_source from in
      let%bind.Trampoline (T info_into) = gather ~recursive_scopes ~time_source into in
      Trampoline.return (Eval_sub.gather ~here ~info_from ~info_into ~via)
    | Store { id; value; inner; here = _ } ->
      Eval_store.f ~gather ~recursive_scopes ~time_source ~id ~value ~inner
    | Fetch { id; default; for_some; here = _ } -> Eval_fetch.f ~id ~default ~for_some
    | Assoc { map; key_comparator; key_id; cmp_id; data_id; by; here = _ } ->
      Eval_assoc.f
        ~gather
        ~recursive_scopes
        ~time_source
        ~map
        ~key_comparator
        ~key_id
        ~cmp_id
        ~data_id
        ~by
    | Assoc_on
        { map
        ; io_comparator
        ; model_comparator
        ; io_key_id
        ; io_cmp_id
        ; model_key_id
        ; model_cmp_id
        ; data_id
        ; by
        ; get_model_key
        ; here = _
        } ->
      Eval_assoc_on.f
        ~gather
        ~recursive_scopes
        ~time_source
        ~map
        ~io_comparator
        ~model_comparator
        ~io_key_id
        ~io_cmp_id
        ~model_key_id
        ~model_cmp_id
        ~data_id
        ~by
        ~get_model_key
    | Assoc_simpl { map; by; may_contain; here = _ } ->
      Eval_assoc_simple.f ~map ~by ~may_contain
    | Switch { match_; arms; here = _ } ->
      Eval_switch.f ~gather ~recursive_scopes ~time_source ~match_ ~arms
    | Lazy { t = lazy_computation; here = _ } ->
      Eval_lazy.f ~gather ~recursive_scopes ~time_source ~lazy_computation
    | Fix_define { fix_id; initial_input; input_id; result; here = _ } ->
      Eval_fix.define
        ~gather
        ~recursive_scopes
        ~time_source
        ~fix_id
        ~initial_input
        ~input_id
        ~result
    | Fix_recurse { input; input_id; fix_id; here = _ } ->
      Eval_fix.recurse ~recursive_scopes ~input ~input_id ~fix_id
    | Wrap
        { wrapper_model
        ; action_id
        ; result_id
        ; inject_id
        ; model_id
        ; inner
        ; dynamic_apply_action
        ; reset
        ; here = _
        } ->
      Eval_wrap.f
        ~gather
        ~recursive_scopes
        ~time_source
        ~wrapper_model
        ~action_id
        ~result_id
        ~inject_id
        ~model_id
        ~inner
        ~dynamic_apply_action
        ~reset
    | With_model_resetter { inner; reset_id; here = _ } ->
      Eval_with_model_resetter.f ~gather ~recursive_scopes ~time_source ~inner ~reset_id
    | Path { here = _ } -> Eval_path.f
    | Lifecycle { lifecycle; here = _ } -> Eval_lifecycle.f ~lifecycle
    | Monitor_free_variables { inner; free_vars; here = _ }
      when Type_id_set.is_empty free_vars -> gather ~recursive_scopes ~time_source inner
    | Monitor_free_variables { here; inner; free_vars } ->
      Eval_monitor_free_variables.f
        ~gather
        ~recursive_scopes
        ~time_source
        ~inner
        ~free_vars
        ~here
;;

let gather ~recursive_scopes ~time_source c =
  Trampoline.run (gather ~recursive_scopes ~time_source c)
;;
