open! Core
open! Import
open Incr.Let_syntax

let f ~model ~static_action ~time_source ~apply_action ~reset =
  let wrap_leaf inject = Action.static_leaf >>> inject in
  let run ~environment:_ ~fix_envs:_ ~path:_ ~model ~inject =
    annotate Model model;
    (* It's important to create [inject_static] outside of the [let%mapn] so that it
           remains [phys_equal] when the [model] changes. *)
    let inject_static = Lazy_inject.make (wrap_leaf inject) in
    let result =
      let%map model in
      model, inject_static
    in
    Trampoline.return (Snapshot.create ~result ~input:Input.static ~lifecycle:None, ())
  in
  let apply_action ~inject ~schedule_event _input model = function
    | Action.Leaf_dynamic _ ->
      eprint_s
        [%message "BUG: state_machine0's apply_action was called with a dynamic action"];
      model
    | Leaf_static action ->
      let inject = Lazy_inject.make (wrap_leaf inject) in
      apply_action ~inject ~schedule_event ~time_source model action
  in
  let reset ~inject ~schedule_event model =
    reset ~inject:(wrap_leaf inject) ~schedule_event ~time_source model
  in
  Trampoline.return
    (Computation.T
       { model
       ; input = Meta.Input.unit
       ; action = Action.Type_id.leaf static_action
       ; apply_action
       ; reset
       ; run
       ; may_contain =
           May_contain.Unresolved.non_recursive ~path:No ~lifecycle:No ~input:No
       })
;;
