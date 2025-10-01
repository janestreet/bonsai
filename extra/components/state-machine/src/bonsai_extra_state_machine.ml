open! Core
module Bonsai = Bonsai.Cont
open Bonsai.Let_syntax

let state_machine1_dynamic_model
  ?sexp_of_action
  ?sexp_of_model
  ?equal
  ~model
  ~apply_action
  input
  (local_ graph)
  =
  let model_creator =
    match model with
    | `Given m ->
      Bonsai.map m ~f:(fun m -> function
        | None -> m
        | Some a -> a)
    | `Computed f -> f
  in
  let apply_action context input model action =
    match input with
    | Bonsai.Computation_status.Active (input, model_creator) ->
      let model = model_creator model in
      Some (apply_action context input model action)
    | Inactive ->
      let sexp_of_action = Option.value sexp_of_action ~default:sexp_of_opaque in
      eprint_s
        [%message
          [%here]
            "An action sent to a [state_machine1_dynamic_model] has been dropped because \
             its input was not present. This happens when the \
             [state_machine1_dynamic_model] is inactive when it receives a message."
            ~action:(sexp_of_action action : Sexp.t)];
      model
  in
  let model, inject =
    Bonsai.state_machine_with_input
      ?sexp_of_model:(Option.map ~f:Option.sexp_of_t sexp_of_model)
      ?sexp_of_action
      ?equal:(Option.map ~f:Option.equal equal)
      ~default_model:None
      ~apply_action
      (Bonsai.both input model_creator)
      graph
  in
  let model =
    let%arr model and model_creator in
    model_creator model
  in
  model, inject
;;

let state_machine0_dynamic_model
  ?sexp_of_action
  ?sexp_of_model
  ?equal
  ~model
  ~apply_action
  (local_ graph)
  =
  let apply_action context () model action = apply_action context model action in
  state_machine1_dynamic_model
    ?sexp_of_action
    ?sexp_of_model
    ?equal
    ~model
    ~apply_action
    (Bonsai.return ())
    graph
;;

let state_dynamic_model ?sexp_of_model ?equal ~model (local_ graph) =
  let apply_action (_ : _ Bonsai.Apply_action_context.t) _old_model new_model =
    new_model
  in
  state_machine0_dynamic_model
    ?sexp_of_action:sexp_of_model
    ?sexp_of_model
    ?equal
    ~model
    ~apply_action
    graph
;;
