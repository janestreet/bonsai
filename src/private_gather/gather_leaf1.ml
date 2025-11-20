open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

let f ~model ~input_id ~dynamic_action ~input ~time_source ~reset ~apply_action ~here =
  let wrap_leaf inject = Action.dynamic_leaf >>> inject in
  let run ~environment ~fix_envs:_ ~path:_ ~model ~inject =
    annotate ~here Model model;
    let input = Value.eval environment input in
    (* It's important to create [inject_dynamic] outside of the [let%mapn] so that it
       remains [phys_equal] when the [model] changes. *)
    let inject_dynamic = Lazy_inject.make (wrap_leaf inject) in
    let result =
      let%mapn model in
      model, inject_dynamic
    in
    Trampoline.return
      (Snapshot.create ~here ~result ~input:(Input.dynamic input) ~lifecycle:None, ())
  in
  let apply_action ~inject ~schedule_event input model = function
    | Action.Leaf_static _ ->
      Ui_metrics.Counters.observe Bonsai_leaf1_apply_action_got_static_action;
      eprint_s
        [%message "BUG: state_machine1's apply_action was called with a static action"];
      model
    | Leaf_dynamic action ->
      let inject = Lazy_inject.make (wrap_leaf inject) in
      apply_action ~inject ~schedule_event ~time_source input model action
  in
  let reset ~inject ~schedule_event model =
    reset ~inject:(wrap_leaf inject) ~schedule_event ~time_source model
  in
  Trampoline.return
    (Computation.T
       { model
       ; input = input_id
       ; action = Action.Type_id.leaf dynamic_action
       ; apply_action
       ; reset
       ; run
       ; may_contain =
           May_contain.Unresolved.non_recursive ~path:No ~lifecycle:No ~input:Yes_or_maybe
       })
;;
