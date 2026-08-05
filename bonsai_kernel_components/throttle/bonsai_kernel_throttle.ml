open! Core
open Bonsai
open Bonsai.Let_syntax

module Action = struct
  type ('input, 'output) t =
    | Update_input of 'input
    | Resolve_effect of 'output
    | Finish_sleep
end

module Model = struct
  type ('input, 'output) t =
    | Idle of { output : 'output option }
    | Running of
        { output : 'output option
        ; next_input : 'input option
        }
    | Waiting of
        { output : 'output
        ; next_input : 'input option
        }

  let default = Idle { output = None }
end

let apply_when_active
  ~(ctx : (_ Action.t, unit) Bonsai.Apply_action_context.t)
  ~sleep
  ~effect_
  (model : _ Model.t)
  (action : _ Action.t)
  : _ Model.t
  =
  let perform_effect input =
    let run_effect_and_wait =
      let open Bonsai.Effect.Let_syntax in
      let%bind output = effect_ input in
      let%bind () = Bonsai.Apply_action_context.inject ctx (Resolve_effect output) in
      let%bind () = sleep in
      let%bind () = Bonsai.Apply_action_context.inject ctx Finish_sleep in
      return ()
    in
    Bonsai.Apply_action_context.schedule_event ctx run_effect_and_wait
  in
  match action, model with
  | Update_input input, Idle { output } ->
    perform_effect input;
    Running { output; next_input = None }
  | Update_input input, Running running ->
    Running { running with next_input = Some input }
  | Update_input input, Waiting waiting ->
    Waiting { waiting with next_input = Some input }
  | Resolve_effect output, Running { next_input; _ } -> Waiting { output; next_input }
  | Finish_sleep, Waiting { output; next_input = None } -> Idle { output = Some output }
  | Finish_sleep, Waiting { output; next_input = Some input } ->
    perform_effect input;
    Running { output = Some output; next_input = None }
  | _ -> (* These are invalid transitions. *) model
;;

let apply_when_inactive (model : _ Model.t) (action : _ Action.t) : _ Model.t =
  match action, model with
  | Update_input _, model -> model
  | Resolve_effect output, _ -> Waiting { output; next_input = None }
  | Finish_sleep, _ -> Model.default
;;

let apply_action ctx input model action =
  match input with
  | Bonsai.Computation_status.Active (sleep_for, effect_) ->
    let sleep =
      Bonsai.Time_source.sleep (Bonsai.Apply_action_context.time_source ctx) sleep_for
    in
    apply_when_active ~ctx ~sleep ~effect_ model action
  | Inactive -> apply_when_inactive model action
;;

let effect_throttle input ~equal ~wait ~effect_ graph =
  let state, inject_action =
    Bonsai.state_machine_with_input
      ~default_model:Model.default
      ~reset:(fun _ _ -> Model.default)
      ~apply_action
      (Bonsai.both (Bonsai.return wait) effect_)
      graph
  in
  let callback =
    let%arr inject_action in
    fun input -> inject_action (Update_input input)
  in
  let () = Bonsai.Edge.on_change ~trigger:`After_display input ~equal ~callback graph in
  let%arr state in
  match state with
  | Idle { output } | Running { output; _ } -> output
  | Waiting { output; _ } -> Some output
;;

let value_throttle ~wait ~equal value graph =
  let opt =
    effect_throttle ~effect_:(Bonsai.return Effect.return) ~equal ~wait value graph
  in
  let%arr value and opt in
  Option.value opt ~default:value
;;
