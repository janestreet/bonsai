open! Core
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect
open Bonsai.Let_syntax

let exactly_once effect (local_ graph) =
  let has_run, set_has_run =
    Bonsai.state ~equal:[%equal: Bool.t] false ~sexp_of_model:[%sexp_of: Bool.t] graph
  in
  let%sub () =
    if%sub has_run
    then Bonsai.return ()
    else (
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map set_has_run
           and event = effect in
           Effect.Many [ set_has_run true; event ])
        graph;
      Bonsai.return ())
  in
  ()
;;

let exactly_once_with_value ?sexp_of_model ?equal effect (local_ graph) =
  let value, set_value = Bonsai.state_opt ?sexp_of_model ?equal graph in
  let%sub () =
    match%sub value with
    | None ->
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map set_value and effect in
           let%bind.Effect r = effect in
           set_value (Some r))
        graph;
      Bonsai.return ()
    | Some _ -> Bonsai.return ()
  in
  value
;;

let bonk (local_ graph) =
  let (_ : unit Bonsai.t), bonk =
    Bonsai.state_machine
      ~default_model:()
      ~apply_action:(fun context () effect ->
        Bonsai.Apply_action_context.schedule_event context effect)
      graph
  in
  bonk
;;

let chain_incr_effects input (local_ graph) =
  let (_ : unit Bonsai.t), inject =
    Bonsai.state_machine_with_input
      input
      ~default_model:()
      ~apply_action:(fun ctx input _model effect_fns ->
        match input, effect_fns with
        | Bonsai.Computation_status.Inactive, _ | _, [] -> ()
        | Active input, effect_fn :: dependents ->
          let effect =
            let%bind.Ui_effect () = effect_fn input in
            Bonsai.Apply_action_context.inject ctx dependents
          in
          Bonsai.Apply_action_context.schedule_event ctx effect)
      graph
  in
  inject
;;
