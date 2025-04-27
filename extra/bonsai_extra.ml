open! Core
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect
open Bonsai.Let_syntax

let with_inject_fixed_point f (local_ graph) =
  let r, _ =
    Bonsai.wrap_n
      ~n:Two
      graph
      ~sexp_of_model:[%sexp_of: Unit.t]
      ~default_model:()
      ~equal:[%equal: Unit.t]
      ~apply_action:(fun context result () action ->
        match result with
        | Inactive ->
          let action = sexp_of_opaque action in
          eprint_s
            [%message
              "An action sent to a [wrap] has been dropped because its input was not \
               present. This happens when the [wrap] is inactive when it receives a \
               message."
                (action : Sexp.t)
                [%here]];
          ()
        | Active (_result, inject) ->
          (* speedy thing go in, speedy thing come out *)
          Bonsai.Apply_action_context.schedule_event context (inject action))
      ~f:(fun _model inject (local_ graph) -> f inject graph)
  in
  r
;;

let with_self_effect
  (type a)
  ?sexp_of_model
  ?equal
  ~(f :
      a Bonsai.Computation_status.t Effect.t Bonsai.t -> local_ Bonsai.graph -> a Bonsai.t)
  : local_ Bonsai.graph -> a Bonsai.t
  =
  fun (local_ graph) ->
  Bonsai.wrap
    graph
    ?sexp_of_model:(Option.map ~f:Option.sexp_of_t sexp_of_model)
    ?equal:(Option.map ~f:Option.equal equal)
    ~default_model:None
    ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) result model () ->
      match result with
      | Inactive ->
        let action = sexp_of_opaque () in
        eprint_s
          [%message
            "An action sent to a [wrap] has been dropped because its input was not \
             present. This happens when the [wrap] is inactive when it receives a \
             message."
              (action : Sexp.t)
              [%here]];
        model
      | Active result -> Some result)
    ~f:(fun model inject (local_ graph) ->
      let current_model =
        let get_model = Bonsai.peek model graph in
        let%arr inject and get_model in
        let%bind.Effect () = inject () in
        let%map.Effect model = get_model in
        match model with
        | Inactive
        (* Active None could happen if the model were reset in between the injection
           and get_model effects *)
        | Active None -> Bonsai.Computation_status.Inactive
        | Active (Some v) -> Active v
      in
      f current_model graph)
;;

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

let value_with_override ?sexp_of_model ?equal value (local_ graph) =
  let state, set_state = Bonsai.state_opt graph ?sexp_of_model ?equal in
  let value =
    match%sub state with
    | Some override -> override
    | None -> value
  in
  let setter =
    let%arr set_state in
    fun v -> set_state (Some v)
  in
  value, setter
;;

let pipe (type a) ?(sexp_of = sexp_of_opaque) (local_ graph) =
  let module Model = struct
    type t =
      { queued_actions : a Fdeque.t
      ; queued_receivers : (unit, a) Effect.Private.Callback.t Fdeque.t
      }

    let equal = phys_equal
    let default = { queued_actions = Fdeque.empty; queued_receivers = Fdeque.empty }
    let sexp_of_t { queued_actions; _ } = Fdeque.sexp_of_t sexp_of queued_actions
  end
  in
  let module Action = struct
    type t =
      | Add_action of a
      | Add_receiver of (unit, a) Effect.Private.Callback.t

    let sexp_of_t = function
      | Add_action a -> sexp_of a
      | Add_receiver r -> sexp_of_opaque r
    ;;
  end
  in
  let _, inject =
    Bonsai.state_machine
      graph
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model:Model.default
      ~apply_action:(fun context model -> function
      | Add_action a ->
        (match Fdeque.dequeue_front model.queued_receivers with
         | None ->
           let queued_actions = Fdeque.enqueue_back model.queued_actions a in
           { model with queued_actions }
         | Some (hd, queued_receivers) ->
           Bonsai.Apply_action_context.schedule_event
             context
             (Effect.Private.Callback.respond_to hd a);
           { model with queued_receivers })
      | Add_receiver r ->
        (match Fdeque.dequeue_front model.queued_actions with
         | None ->
           let queued_receivers = Fdeque.enqueue_back model.queued_receivers r in
           { model with queued_receivers }
         | Some (hd, queued_actions) ->
           Bonsai.Apply_action_context.schedule_event
             context
             (Effect.Private.Callback.respond_to r hd);
           { model with queued_actions }))
  in
  let enqueue =
    let%arr inject in
    fun a -> inject (Add_action a)
  in
  let dequeue =
    let%arr inject in
    Effect.Private.make ~request:() ~evaluator:(fun r ->
      Effect.Expert.handle (inject (Add_receiver r)))
  in
  enqueue, dequeue
;;

module Id_gen (T : Int_intf.S) () = struct
  include T

  let component' ?(reset = (`Reset : [ `Reset | `Do_nothing | `Bump ])) (local_ graph) =
    let reset =
      match reset with
      | `Reset -> None
      | `Do_nothing -> Some (fun _ctx model -> model)
      | `Bump -> Some (fun _ctx model -> T.(model + one))
    in
    let model, fetch =
      Bonsai.actor
        ?reset
        ~sexp_of_model:[%sexp_of: T.t]
        ~equal:[%equal: T.t]
        ~sexp_of_action:[%sexp_of: Unit.t]
        ~default_model:T.minus_one
        ~recv:(fun _ctx i () ->
          let to_return = T.( + ) i T.one in
          to_return, to_return)
        graph
    in
    let fetch =
      let%arr fetch in
      fetch ()
    in
    fetch, model
  ;;

  let component ?(reset = (`Reset : [ `Reset | `Do_nothing ])) (local_ graph) =
    let fetch, _ = component' ~reset:(reset :> [ `Reset | `Do_nothing | `Bump ]) graph in
    fetch
  ;;
end

let mirror'
  (type m)
  ?sexp_of_model
  ~equal
  ~(store_set : (m -> unit Effect.t) Bonsai.t)
  ~(store_value : m option Bonsai.t)
  ~(interactive_set : (m -> unit Effect.t) Bonsai.t)
  ~(interactive_value : m option Bonsai.t)
  (local_ graph)
  =
  let module M = struct
    type t = m

    let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_model
  end
  in
  let module M2 = struct
    type model = M.t

    let equal_model = equal
    let sexp_of_model = M.sexp_of_t

    type t =
      { store : model option
      ; interactive : model option
      }
    [@@deriving sexp_of, equal]
  end
  in
  let callback =
    let%map store_set and interactive_set in
    fun old_pair { M2.store = store_value; interactive = interactive_value } ->
      let stability =
        if Option.equal equal store_value interactive_value then `Stable else `Unstable
      in
      match stability with
      | `Stable ->
        (* if both of the new values are the same, then we're done! Stability
           has already been reached. *)
        Effect.Ignore
      | `Unstable ->
        (match old_pair with
         | None ->
           (* on_change' is triggered when the values flow through this node
              for the first time.  In this scenario, we prioritize the
              value in the store. *)
           (match store_value, interactive_value with
            | Some store_value, _ -> interactive_set store_value
            | None, Some interactive_value -> store_set interactive_value
            | None, None ->
              eprint_s
                [%message
                  "BUG" [%here] {|if both are None, then we shouldn't be `Unstable |}];
              Effect.Ignore)
         | Some { M2.store = old_store_value; interactive = old_interactive_value } ->
           let store_changed = not (Option.equal equal old_store_value store_value) in
           let interactive_changed =
             not (Option.equal equal old_interactive_value interactive_value)
           in
           (match interactive_changed, store_changed with
            (* if both the interactive-value and store values have changed,
               first try to forward it on to the store, but if the interactive value was
               changed to None and the store value was changed to a Some, then the interactive
               value gets set to the new store value. *)
            | true, true ->
              (match interactive_value, store_value with
               | Some interactive_value, (Some _ | None) -> store_set interactive_value
               | None, Some store_value -> interactive_set store_value
               | None, None -> Effect.Ignore)
            (* when the interactive value changed, but the store did not, set the store to
               the new interactive value (if it's Some]. *)
            | true, false ->
              (match interactive_value with
               | Some interactive_value -> store_set interactive_value
               | None -> Effect.Ignore)
            (* finally, if the store changed but interactive did not, update the
               interactive value. *)
            | false, true ->
              (match store_value with
               | Some store_value -> interactive_set store_value
               | None -> Effect.Ignore)
            (* this final case should never happen.  Error message explains why.*)
            | false, false ->
              eprint_s
                [%message
                  "BUG" [%here] "on_change triggered when nothing actually changed?"];
              Effect.Ignore))
  in
  Bonsai.Edge.on_change'
    ~sexp_of_model:[%sexp_of: M2.t]
    ~equal:[%equal: M2.t]
    (let%map store = store_value
     and interactive = interactive_value in
     { M2.store; interactive })
    ~callback
    graph
;;

let mirror
  ?sexp_of_model
  ~equal
  ~store_set
  ~store_value
  ~interactive_set
  ~interactive_value
  (local_ graph)
  =
  let store_value = store_value >>| Option.some in
  let interactive_value = interactive_value >>| Option.some in
  mirror'
    ?sexp_of_model
    ~equal
    ~store_set
    ~store_value
    ~interactive_set
    ~interactive_value
    graph
;;

let with_last_modified_time
  ~equal
  input
  (* Although [Bonsai.Clock.now] is generally discouraged, the cutoff only pays
     attention to [input], so [now] shouldn't cause re-firing of this
     computation's transitive dependencies. *)
  (local_ graph)
  =
  let now = Bonsai.Clock.now graph in
  let result = Bonsai.both input now in
  let%sub result, time =
    Bonsai.Incr.value_cutoff result ~equal:(fun (a, _) (b, _) -> equal a b) graph
  in
  result, time
;;

let is_stable ~equal input ~time_to_stable (local_ graph) =
  let sign =
    let%arr time_to_stable in
    Time_ns.Span.sign time_to_stable
  in
  match%sub sign with
  | Neg ->
    let on_activate =
      Bonsai.return
        (Effect.of_thunk (fun () ->
           eprint_s
             [%message "Bonsai_extra.is_stable: [time_to_stable] should not be negative"]))
    in
    let () = Bonsai.Edge.lifecycle ~on_activate graph in
    Bonsai.return true
  | Zero -> Bonsai.return true
  | Pos ->
    let _, last_modified_time = with_last_modified_time ~equal input graph in
    let next_stable_time =
      let%arr last_modified_time and time_to_stable in
      Time_ns.add last_modified_time time_to_stable
    in
    let at_next_stable_time = Bonsai.Clock.at next_stable_time graph in
    (match%arr at_next_stable_time with
     | Before -> false
     | After -> true)
;;

let most_recent_value_satisfying ?sexp_of_model ~equal input ~condition =
  Bonsai.most_recent_some ?sexp_of_model ~equal input ~f:(fun a ->
    if condition a then Some a else None)
;;

module Stability = struct
  type 'a t =
    | Stable of 'a
    | Unstable of
        { previously_stable : 'a option
        ; unstable_value : 'a
        }
  [@@deriving sexp, equal]

  let most_recent_stable_value = function
    | Stable a -> Some a
    | Unstable { previously_stable; _ } -> previously_stable
  ;;

  let prefer_stable_value = function
    | Stable a -> a
    | Unstable { previously_stable; unstable_value } ->
      (match previously_stable with
       | Some a -> a
       | None -> unstable_value)
  ;;
end

let value_stability
  (type a)
  ?sexp_of_model
  ~equal:input_equal
  input
  ~time_to_stable
  (local_ graph)
  =
  let module M = struct
    type t = a

    let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_model
  end
  in
  let is_stable = is_stable ~equal:input_equal input ~time_to_stable graph in
  let most_recent_stable_and_true =
    let input_and_stability = Bonsai.both input is_stable in
    let module M = struct
      include M

      let equal = input_equal
    end
    in
    most_recent_value_satisfying
      ~sexp_of_model:[%sexp_of: M.t * bool]
      ~equal:[%equal: M.t * bool]
      input_and_stability
      ~condition:(fun (_input, is_stable) -> is_stable)
      graph
  in
  match%sub most_recent_stable_and_true with
  | Some most_recent_stable_and_true ->
    let%arr most_recent_stable, must_be_true = most_recent_stable_and_true
    and is_stable
    and input in
    (match must_be_true with
     | true -> ()
     | false ->
       eprint_s [%message "BUG:" [%here] "value which passed through filter must be true"]);
    if input_equal input most_recent_stable && is_stable
    then Stability.Stable input
    else Unstable { previously_stable = Some most_recent_stable; unstable_value = input }
  | None ->
    let%arr input in
    Stability.Unstable { previously_stable = None; unstable_value = input }
;;

module One_at_a_time = struct
  module Status = struct
    type t =
      | Busy
      | Idle
    [@@deriving sexp, equal]
  end

  module Response = struct
    type 'a t =
      | Result of 'a
      | Busy
    [@@deriving sexp]
  end

  module Lock_action = struct
    type t =
      | Acquire
      | Release
    [@@deriving sexp]
  end

  let effect f (local_ graph) =
    let status, inject_status =
      Bonsai.actor
        graph
        ~sexp_of_model:[%sexp_of: Status.t]
        ~equal:[%equal: Status.t]
        ~sexp_of_action:[%sexp_of: Lock_action.t]
        ~default_model:Idle
        ~recv:(fun _ctx model action ->
          match action with
          | Acquire ->
            let response =
              match model with
              | Busy -> false
              | Idle -> true
            in
            Busy, response
          | Release -> Idle, true)
    in
    let effect =
      let%arr inject_status and f in
      let open Effect.Let_syntax in
      fun query ->
        match%bind inject_status Acquire with
        | false -> return Response.Busy
        | true ->
          let%bind result = f query in
          let%map (_ : bool) = inject_status Release in
          Response.Result result
    in
    effect, status
  ;;
end

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
