open! Core
open! Import
include Proc_min
module Var = Var

module Let_syntax = struct
  let return = read

  module Let_syntax = struct
    let sub = sub
    let switch = switch
    let return = return
    let map ?here t ~f = { (Value.map t ~f) with here }
    let both = Value.both
    let arr ?here t ~f = read (map ?here t ~f)
    let cutoff t ~equal = Value.cutoff ~added_by_let_syntax:true t ~equal

    include (Value : Mapn with type 'a t := 'a Value.t)
  end

  let ( >>| ) a f = Let_syntax.map a ~f
  let ( <*> ) f a = Value.map2 f a ~f:(fun f a -> f a)
  let ( <$> ) f a = Let_syntax.map a ~f
end

open Let_syntax

let pure f i = read (Value.map i ~f)
let const x = read (Value.return x)
let with_model_resetter' = with_model_resetter

let with_model_resetter inside =
  with_model_resetter' (fun ~reset ->
    let%sub r = inside in
    return (Value.both r reset))
;;

let enum (type k) (module E : Enum with type t = k) ~match_ ~with_ =
  let module E = struct
    include E
    include Comparator.Make (E)
  end
  in
  let forward_index = List.to_array E.all in
  let reverse_index =
    Map.of_alist_exn (module E) (List.mapi E.all ~f:(fun i k -> k, i))
  in
  let match_ = match_ >>| Map.find_exn reverse_index in
  let branches = Array.length forward_index in
  let with_ i = with_ (Array.get forward_index i) in
  Let_syntax.switch ~here:[%here] ~match_ ~branches ~with_
;;

let scope_model
      (type a cmp)
      (module M : Comparator with type t = a and type comparator_witness = cmp)
      ~on:v
      computation
  =
  let v = Value.map v ~f:(fun k -> Map.singleton (module M) k ()) in
  let%sub map = assoc (module M) v ~f:(fun _ _ -> computation) in
  let%arr map = map in
  (* This _exn is ok because we know that the map is a singleton *)
  let _k, r = Map.max_elt_exn map in
  r
;;

let of_module1 (type i m a r) (component : (i, m, a, r) component_s) ~default_model input =
  let (module M) = component in
  let%sub input = return input in
  let%sub model_and_inject =
    state_machine1
      (module M.Model)
      (module M.Action)
      ~default_model
      ~apply_action:(fun ~inject ~schedule_event input model action ->
        match input with
        | Active input -> M.apply_action ~inject ~schedule_event input model action
        | Inactive ->
          eprint_s
            [%message
              [%here]
                "An action sent to an [of_module1] has been dropped because its input \
                 was not present. This happens when the [of_module1] is inactive when it \
                 receives a message."
                (action : M.Action.t)];
          model)
      input
  in
  let%arr model, inject = model_and_inject
  and input = input in
  M.compute ~inject input model
;;

let of_module2 c ~default_model i1 i2 = of_module1 c ~default_model (Value.both i1 i2)

let race_dynamic_model
      (type m a)
      (module M : Model with type t = m)
      (module A : Action with type t = a)
      ~model
      ~apply_action
      input
  =
  let model_creator =
    match model with
    | `Given m ->
      Value.map m ~f:(fun m -> function
        | None -> m
        | Some a -> a)
    | `Computed f -> f
  in
  let module M_actual = struct
    type t = M.t option [@@deriving sexp, equal]
  end
  in
  let apply_action ~inject ~schedule_event computation_status model action =
    match computation_status with
    | Computation_status.Active (input, model_creator) ->
      let model = Some (model_creator model) in
      Some
        (apply_action
           ~inject
           ~schedule_event
           (Computation_status.Active input)
           model
           action)
    | Inactive -> Some (apply_action ~inject ~schedule_event Inactive model action)
  in
  let%sub model_and_inject =
    state_machine1
      (module M_actual)
      (module A)
      ~default_model:None
      ~apply_action
      (Value.both input model_creator)
  in
  let%arr model, inject = model_and_inject
  and model_creator = model_creator in
  model_creator model, inject
;;

let of_module0 (type m a r) (component : (unit, m, a, r) component_s) ~default_model =
  let (module M) = component in
  let%sub model_and_inject =
    state_machine0
      (module M.Model)
      (module M.Action)
      ~default_model
      ~apply_action:(M.apply_action ())
  in
  let%arr model, inject = model_and_inject in
  M.compute ~inject () model
;;

let actor1
  : type input model action return.
    (module Model with type t = model)
    -> (module Action with type t = action)
    -> ?reset:
         (inject:(action -> return Effect.t)
          -> schedule_event:(unit Effect.t -> unit)
          -> model
          -> model)
    -> default_model:model
    -> recv:
         (schedule_event:(unit Ui_effect.t -> unit)
          -> input Computation_status.t
          -> model
          -> action
          -> model * return)
    -> input Value.t
    -> (model * (action -> return Effect.t)) Computation.t
  =
  fun (module M : Model with type t = model)
    (module A : Action with type t = action)
    ?reset
    ~default_model
    ~recv
    input ->
    let module Action_with_callback = struct
      type t = (action, return) Effect.Private.Callback.t

      let sexp_of_t cb = A.sexp_of_t (Effect.Private.Callback.request cb)
    end
    in
    let reset =
      Option.map reset ~f:(fun f ~inject ~schedule_event model ->
        let inject action = Effect.Private.make ~request:action ~evaluator:inject in
        f ~inject ~schedule_event model)
    in
    let%sub model, inject =
      state_machine1
        (module M)
        (module Action_with_callback)
        ?reset
        ~default_model
        ~apply_action:(fun ~inject:_ ~schedule_event input model callback ->
          let action = Effect.Private.Callback.request callback in
          let new_model, response = recv ~schedule_event input model action in
          schedule_event (Effect.Private.Callback.respond_to callback response);
          new_model)
        input
    in
    let%sub inject =
      let%arr inject = inject in
      fun action -> Effect.Private.make ~request:action ~evaluator:inject
    in
    let%arr model = model
    and inject = inject in
    model, inject
;;

let actor0 ?reset model action ~default_model ~recv =
  let recv ~schedule_event (_ : unit Computation_status.t) = recv ~schedule_event in
  actor1 model action ?reset ~default_model ~recv (Value.return ())
;;

let state (type m) ?reset (module M : Model with type t = m) ~default_model =
  let reset = Option.map reset ~f:(fun reset ~inject:_ ~schedule_event:_ m -> reset m) in
  state_machine0
    ?reset
    (module M)
    (module M)
    ~apply_action:(fun ~inject:_ ~schedule_event:_ _old_model new_model -> new_model)
    ~default_model
;;

let toggle ~default_model =
  let%sub state, inject =
    state_machine0
      (module Bool)
      (module Unit)
      ~apply_action:(fun ~inject:_ ~schedule_event:_ b () -> not b)
      ~default_model
  in
  let%sub effect =
    (* only compute the effect once for better incrementality *)
    let%arr inject = inject in
    inject ()
  in
  let%arr state = state
  and effect = effect in
  state, effect
;;

let state_opt (type m) ?reset ?default_model (module M : Model with type t = m) =
  state
    ?reset
    ~default_model
    (module struct
      type t = M.t option [@@deriving equal, sexp]
    end)
;;

let path_id =
  let%sub path = path in
  let%arr path = path in
  Path.to_unique_identifier_string path
;;

let yoink a =
  let%sub _, result =
    actor1
      (module Unit)
      (module Unit)
      ~recv:(fun ~schedule_event:_ a () () -> (), a)
      ~default_model:()
      a
  in
  let%arr result = result in
  result ()
;;

module Edge = struct
  include Edge

  let lifecycle' ?on_activate ?on_deactivate ?after_display () =
    let transpose_join : 'a option Value.t option -> 'a option Value.t = function
      | Some a -> a
      | None -> Value.return None
    in
    let%sub triple =
      let%arr a = transpose_join on_activate
      and b = transpose_join on_deactivate
      and c = transpose_join after_display in
      a, b, c
    in
    let%sub t =
      match%arr triple with
      | None, None, None -> None
      | on_activate, on_deactivate, after_display ->
        Some { Lifecycle.on_activate; on_deactivate; after_display }
    in
    lifecycle t
  ;;

  let lifecycle ?on_activate ?on_deactivate ?after_display () =
    lifecycle'
      ?on_activate:(Option.map on_activate ~f:(Value.map ~f:Option.some))
      ?on_deactivate:(Option.map on_deactivate ~f:(Value.map ~f:Option.some))
      ?after_display:(Option.map after_display ~f:(Value.map ~f:Option.some))
      ()
  ;;

  let after_display' event_opt_value = lifecycle' ~after_display:event_opt_value ()

  let after_display event_value =
    let event_value = Value.map event_value ~f:Option.some in
    lifecycle' ~after_display:event_value ()
  ;;

  let on_change' (type a) (module M : Model with type t = a) input ~callback =
    let%sub state, set_state = state_opt (module M) in
    let%sub update =
      match%sub state with
      | None ->
        let%arr set_state = set_state
        and input = input
        and callback = callback in
        Some (Ui_effect.Many [ set_state (Some input); callback None input ])
      | Some state ->
        let%arr state = state
        and set_state = set_state
        and input = input
        and callback = callback in
        if phys_equal state input || M.equal state input
        then None
        else
          lazy (Ui_effect.Many [ set_state (Some input); callback (Some state) input ])
          |> Ui_effect.lazy_
          |> Some
    in
    after_display' update
  ;;

  let on_change model input ~callback =
    let callback = Value.map callback ~f:(fun callback _prev value -> callback value) in
    on_change' model input ~callback
  ;;

  module Poll = struct
    module Starting = struct
      type ('a, 'r) t =
        | Empty : ('a, 'a option) t
        | Initial : 'a -> ('a, 'a) t

      let empty = Empty
      let initial a = Initial a
    end

    let manual_refresh_implementation
          (type r)
          (module Result : Model with type t = r)
          ~initial
          ~wrap_result
          ~effect
      =
      let%sub _, next_seqnum =
        actor0
          (module Int)
          (module Unit)
          ~default_model:0
          ~recv:(fun ~schedule_event:_ i () -> i + 1, i)
      in
      let module State = struct
        type t =
          { last_seqnum : int
          ; last_result : Result.t
          }
        [@@deriving sexp, equal, fields]
      end
      in
      let module Action = struct
        type t = Set of int * Result.t [@@deriving sexp_of]
      end
      in
      let%sub state, inject_change =
        state_machine0
          (module State)
          (module Action)
          ~apply_action:
            (fun ~inject:_ ~schedule_event:_ model (Action.Set (seqnum, res)) ->
               if seqnum < model.State.last_seqnum
               then model
               else { State.last_seqnum = seqnum; last_result = res })
          ~default_model:{ State.last_seqnum = -1; last_result = initial }
      in
      let%sub callback =
        let%arr effect = effect
        and next_seqnum = next_seqnum
        and inject_change = inject_change in
        let%bind.Effect seqnum = next_seqnum () in
        let%bind.Effect result = effect in
        inject_change (Action.Set (seqnum, wrap_result result))
      in
      let%arr { State.last_result; _ } = state
      and callback = callback in
      last_result, callback
    ;;

    let manual_refresh
      : type o r.
        (module Model with type t = o)
        -> (o, r) Starting.t
        -> effect:o Effect.t Value.t
        -> (r * unit Effect.t) Computation.t
      =
      fun (module Result : Model with type t = o) kind ~effect ->
        match kind with
        | Starting.Empty ->
          manual_refresh_implementation
            (module struct
              type t = Result.t option [@@deriving sexp, equal]
            end)
            ~effect
            ~initial:None
            ~wrap_result:Option.some
        | Starting.Initial initial ->
          manual_refresh_implementation (module Result) ~effect ~initial ~wrap_result:Fn.id
    ;;

    let effect_on_change
      : type a o r.
        (module Model with type t = a)
        -> (module Model with type t = o)
        -> (o, r) Starting.t
        -> a Value.t
        -> effect:(a -> o Effect.t) Value.t
        -> r Computation.t
      =
      fun (module Input) (module Result) kind input ~effect ->
        let%sub get_input = yoink input in
        let%sub effect =
          let%arr get_input = get_input
          and effect = effect in
          let%bind.Effect input =
            match%bind.Effect get_input with
            | Active input -> Effect.return input
            | Inactive ->
              Effect.never
          in
          effect input
        in
        let%sub result, refresh = manual_refresh (module Result) kind ~effect in
        let%sub callback =
          let%arr refresh = refresh in
          fun (_ : Input.t) -> refresh
        in
        let%sub () = on_change (module Input) input ~callback in
        return result
    ;;
  end
end

module Effect_throttling = struct
  module Poll_result = struct
    type 'a t =
      | Aborted
      | Finished of 'a
    [@@deriving sexp_of]
  end

  let poll
    : type a b.
      (a -> b Effect.t) Value.t -> (a -> b Poll_result.t Effect.t) Computation.t
    =
    fun effect ->
    let module Action = struct
      type t =
        | Run of (a, b Poll_result.t) Effect.Private.Callback.t
        | Activate
        | Finished

      let sexp_of_t = sexp_of_opaque
    end
    in
    let module Model = struct
      type t =
        { running : bool
        ; next_up : (a, b Poll_result.t) Effect.Private.Callback.t option
        }

      let sexp_of_t = sexp_of_opaque
      let t_of_sexp _ = assert false
      let equal = phys_equal
    end
    in
    let%sub _model, inject =
      state_machine1
        (module Model)
        (module Action)
        (* This computation does nothing on reset because users should be
           oblivious to the fact that it has a model. I don't think there is a
           "correct" decision in this case - this behavior just seems more
           reasonable to me. *)
        ~reset:(fun ~inject:_ ~schedule_event:_ model -> model)
        ~default_model:{ running = false; next_up = None }
        ~apply_action:(fun ~inject ~schedule_event effect { running; next_up } action ->
          let run_effect effect callback =
            schedule_event
              (let%bind.Effect response =
                 effect (Effect.Private.Callback.request callback)
               in
               let%bind.Effect () =
                 Effect.Private.Callback.respond_to
                   callback
                   (Poll_result.Finished response)
               in
               inject Finished)
          in
          let abort callback =
            schedule_event
              (Effect.Private.Callback.respond_to callback Poll_result.Aborted)
          in
          let soft_assert_running here running =
            if not running
            then
              eprint_s
                [%message
                  (here : Source_code_position.t)
                    "BUG:  finished effect even though not running"]
          in
          (* There are a lot of cases, and perhaps this match expression could
             be factored to be shorter, but the advantage to this is that every
             case is extremely short, and it's easy to find which code path a
             set of variable configurations will take. *)
          match action, running, next_up, effect with
          | Run callback, false, None, Inactive ->
            { running = false; next_up = Some callback }
          | Run callback, false, None, Active effect ->
            run_effect effect callback;
            { running = true; next_up = None }
          | Run callback, false, Some next_up, Inactive ->
            abort next_up;
            { running = false; next_up = Some callback }
          | Run callback, false, Some next_up, Active effect ->
            (* This case is untested because I couldn't figure out how to reach
               this code path in tests. It seems impossible. *)
            run_effect effect next_up;
            { running = true; next_up = Some callback }
          | Run callback, true, None, (Inactive | Active _) ->
            { running = true; next_up = Some callback }
          | Run callback, true, Some next_up, (Inactive | Active _) ->
            abort next_up;
            { running = true; next_up = Some callback }
          | Activate, running, next_up, Inactive ->
            (* This case looks impossible because [Activate] events happen
               after a computation is activated, so it should have access to
               the input. However, it can happen if a computation is activated
               and de-activated the next frame. The Activate effect doesn't run
               until the frame in which it was deactivated, which means it
               doesn't have access to the input. *)
            { running; next_up }
          | Activate, false, None, Active _ -> { running = false; next_up = None }
          | Activate, false, Some next_up, Active effect ->
            run_effect effect next_up;
            { running = true; next_up = None }
          | Activate, true, next_up, Active _ -> { running = true; next_up }
          | Finished, running, None, (Inactive | Active _) ->
            soft_assert_running [%here] running;
            { running = false; next_up = None }
          | Finished, running, Some next_up, Inactive ->
            soft_assert_running [%here] running;
            { running = false; next_up = Some next_up }
          | Finished, running, Some next_up, Active effect ->
            soft_assert_running [%here] running;
            run_effect effect next_up;
            { running = true; next_up = None })
        effect
    in
    let%sub on_activate =
      let%arr inject = inject in
      inject Activate
    in
    let%sub () = Edge.lifecycle ~on_activate () in
    let%arr inject = inject in
    fun request ->
      Effect.Private.make ~request ~evaluator:(fun callback -> inject (Run callback))
  ;;
end

let freeze model value =
  let%sub state, set_state = state_opt model in
  match%sub state with
  | Some state -> return state
  | None ->
    let%sub () =
      Edge.lifecycle
        ~on_activate:
          (let%map set_state = set_state
           and value = value in
           set_state (Some value))
        ()
    in
    return value
;;

let thunk (type a) (f : unit -> a) =
  let%sub out = return Value.(map (Var.value (Var.create ())) ~f) in
  freeze
    (module struct
      type t = (a[@sexp.opaque]) [@@deriving sexp]

      let equal = phys_equal
    end)
    out
;;

module Incr = struct
  include Proc_incr

  let compute t ~f = compute_with_clock t ~f:(fun _ input -> f input)
  let with_clock f = compute_with_clock (Value.return ()) ~f:(fun clock _ -> f clock)

  let to_value incr =
    { Value.value = Value.Incr incr
    ; here = None
    ; id = Type_equal.Id.create ~name:"to_value" sexp_of_opaque
    }
  ;;
end

let most_recent_some (type a) (module M : Model with type t = a) input ~f =
  let%sub most_recent_valid_value, set_most_recent_valid_value = state_opt (module M) in
  let%sub input = pure f input in
  let%sub input = Incr.value_cutoff ~equal:[%equal: M.t option] input in
  match%sub input with
  | None -> return most_recent_valid_value
  | Some inner ->
    let%sub callback =
      let%arr set_most_recent_valid_value = set_most_recent_valid_value in
      fun x -> set_most_recent_valid_value (Some x)
    in
    let%sub () = Edge.on_change (module M) inner ~callback in
    return input
;;

let most_recent_value_satisfying m input ~condition =
  most_recent_some m input ~f:(fun a -> if condition a then Some a else None)
;;

module Map_renamed_to_avoid_shadowing = struct
  let of_set = Incr.compute ~f:Ui_incr.Map.of_set
  let keys = Incr.compute ~f:Ui_incr.Map.keys
  let filter_mapi m ~f = Incr.compute m ~f:(Incr_map.filter_mapi ~f)

  let merge a b ~f =
    Incr.compute (Value.both a b) ~f:(fun a_and_b ->
      let%pattern_bind.Ui_incr a, b = a_and_b in
      Incr_map.merge a b ~f)
  ;;
end

let assoc_set m v ~f =
  let%sub as_map = Map_renamed_to_avoid_shadowing.of_set v in
  assoc m as_map ~f:(fun k _ -> f k)
;;

let assoc_list (type key cmp) (m : (key, cmp) comparator) list ~get_key ~f =
  let module M = (val m) in
  let%sub alist =
    let%arr list = list in
    List.map list ~f:(fun x -> get_key x, x)
  in
  let%sub input_map =
    let%arr alist = alist in
    Map.of_alist (module M) alist
  in
  match%sub input_map with
  | `Ok input_map ->
    let%sub output_map = assoc m input_map ~f in
    let%arr alist = alist
    and output_map = output_map in
    `Ok
      (List.map alist ~f:(fun (k, _) ->
         match Map.find output_map k with
         | Some r -> r
         | None ->
           raise_s
             [%message "BUG" [%here] "Incremental glitch" ~key:(k : M.t) "not found"]))
  | `Duplicate_key key ->
    let%arr key = key in
    `Duplicate_key key
;;

module Dynamic_scope = struct
  include Dynamic_scope

  type _ t =
    | Independent :
        { id : 'a Type_equal.Id.t
        ; fallback : 'a
        }
        -> 'a t
    | Derived :
        { base : 'a t
        ; get : 'a -> 'b
        ; set : 'a -> 'b -> 'a
        ; sexp_of : 'b -> Sexp.t
        }
        -> 'b t

  let rec fallback : type a. a t -> a = function
    | Independent { fallback; _ } -> fallback
    | Derived { base; get; set = _; sexp_of = _ } -> get (fallback base)
  ;;

  let rec fetch : type a b. a t -> default:b -> for_some:(a -> b) -> b Computation.t =
    fun t ~default ~for_some ->
      match t with
      | Independent { id; _ } -> Dynamic_scope.fetch ~id ~default ~for_some
      | Derived { base; get; set = _; sexp_of = _ } ->
        fetch base ~default ~for_some:(fun x -> for_some (get x))
  ;;

  let lookup (type a) (var : a t) = fetch var ~default:(fallback var) ~for_some:Fn.id

  let rec store : type a. a t -> a Value.t -> 'r Computation.t -> 'r Computation.t =
    fun var value inner ->
    match var with
    | Independent { id; _ } -> Dynamic_scope.store ~id ~value ~inner
    | Derived { base; get = _; set; sexp_of = _ } ->
      let%sub current = lookup base in
      let%sub new_ =
        let%arr current = current
        and value = value in
        set current value
      in
      store base new_ inner
  ;;

  let create ?(sexp_of = sexp_of_opaque) ~name ~fallback () =
    Independent { id = Type_equal.Id.create ~name sexp_of; fallback }
  ;;

  let derived ?(sexp_of = sexp_of_opaque) base ~get ~set =
    Derived { base; get; set; sexp_of }
  ;;

  type revert = { revert : 'a. 'a Computation.t -> 'a Computation.t }

  let modify var ~change ~f =
    let%sub current = lookup var in
    let revert c = store var current c in
    let value = change current in
    store var value (f { revert })
  ;;

  let set t v ~inside = store t v inside
  let set' var value ~f = modify var ~change:(fun _ -> value) ~f
end

module Clock = struct
  let approx_now ~tick_every =
    Incr.with_clock (fun clock ->
      let%map.Ui_incr () = Ui_incr.Clock.at_intervals clock tick_every in
      Ui_incr.Clock.now clock)
  ;;

  let now = Incr.with_clock Ui_incr.Clock.watch_now

  module Before_or_after = struct
    type t = Ui_incr.Before_or_after.t =
      | Before
      | After
    [@@deriving sexp, equal]
  end

  let at time =
    Incr.compute_with_clock time ~f:(fun clock ->
      Ui_incr.bind ~f:(Ui_incr.Clock.at clock))
  ;;

  let get_current_time =
    Incr.with_clock (fun clock ->
      Ui_incr.return (Effect.of_sync_fun (fun () -> Ui_incr.Clock.now clock) ()))
  ;;

  module Trigger_id = Unique_id.Int ()

  module Every_model = struct
    type t =
      | Waiting_for_effect_to_finish
      | Waiting_for of Trigger_id.t option * Time_ns.Alternate_sexp.t
    [@@deriving sexp, equal]
  end

  module Every_action = struct
    type t =
      | Schedule_effect
      | Wait_for of Time_ns.Alternate_sexp.t
    [@@deriving sexp, equal]
  end

  let generic_every ~create_effect ?(trigger_on_activate = true) span callback =
    let%sub base_time =
      let%sub now = now in
      freeze (module Time_ns.Alternate_sexp) now
    in
    let%sub initial_model =
      let%arr base_time = base_time in
      let start_time =
        if trigger_on_activate then base_time else Time_ns.add base_time span
      in
      Every_model.Waiting_for (None, start_time)
    in
    let%sub get_current_time = get_current_time in
    let%sub race_input =
      let%arr base_time = base_time
      and get_current_time = get_current_time
      and callback = callback in
      base_time, get_current_time, callback
    in
    let apply_action ~inject ~schedule_event input _old_model = function
      | Every_action.Schedule_effect ->
        schedule_event
          (match input with
           | Computation_status.Inactive -> Effect.Ignore
           | Active (base_time, get_current_time, callback) ->
             let%bind.Effect next_time =
               create_effect ~span ~base_time ~get_current_time ~callback
             in
             inject (Every_action.Wait_for next_time));
        Every_model.Waiting_for_effect_to_finish
      | Wait_for next_time -> Waiting_for (Some (Trigger_id.create ()), next_time)
    in
    let%sub every_model, inject =
      race_dynamic_model
        (module Every_model)
        (module Every_action)
        ~model:(`Given initial_model)
        ~apply_action
        race_input
    in
    let%sub before_or_after =
      match%sub every_model with
      | Waiting_for_effect_to_finish -> const None
      | Waiting_for (trigger_id, time) ->
        let%sub before_or_after = at time in
        let%arr trigger_id = trigger_id
        and before_or_after = before_or_after in
        Some (trigger_id, before_or_after)
    in
    let%sub callback =
      let%arr inject = inject in
      function
      | None | Some (_, Before_or_after.Before) -> Effect.Ignore
      | Some (_, After) -> inject Schedule_effect
    in
    Edge.on_change
      (module struct
        type t = (Trigger_id.t option * Before_or_after.t) option [@@deriving sexp, equal]
      end)
      before_or_after
      ~callback
  ;;

  let ensure_clock_advances old_time new_time =
    Time_ns.max (Time_ns.next old_time) new_time
  ;;

  let every_wait_period_after_previous_effect_finishes_blocking =
    generic_every ~create_effect:(fun ~span ~base_time:_ ~get_current_time ~callback ->
      let%bind.Effect () = callback in
      let%map.Effect now = get_current_time in
      ensure_clock_advances now (Time_ns.add now span))
  ;;

  let every_wait_period_after_previous_effect_starts_blocking =
    generic_every ~create_effect:(fun ~span ~base_time:_ ~get_current_time ~callback ->
      let%bind.Effect start = get_current_time in
      let%bind.Effect () = callback in
      let%map.Effect now = get_current_time in
      ensure_clock_advances now (Time_ns.add start span))
  ;;

  let every_multiple_of_period_blocking =
    generic_every ~create_effect:(fun ~span ~base_time ~get_current_time ~callback ->
      let%bind.Effect () = callback in
      let%map.Effect now = get_current_time in
      ensure_clock_advances
        now
        (Time_ns.next_multiple
           ~can_equal_after:false
           ~base:base_time
           ~after:now
           ~interval:(Time_ns.Span.max span (Time_ns.Span.next Time_ns.Span.zero))
           ()))
  ;;

  let every_multiple_of_period_non_blocking ?trigger_on_activate span callback =
    every_multiple_of_period_blocking
      ?trigger_on_activate
      span
      (let%map callback = callback in
       Effect.Many [ callback ])
  ;;

  let every
    :  when_to_start_next_effect:
         [< `Wait_period_after_previous_effect_starts_blocking
         | `Wait_period_after_previous_effect_finishes_blocking
         | `Every_multiple_of_period_non_blocking
         | `Every_multiple_of_period_blocking
         ]
      -> ?trigger_on_activate:bool -> Time_ns.Span.t -> unit Effect.t Value.t
      -> unit Computation.t
    =
    fun ~when_to_start_next_effect ->
      match when_to_start_next_effect with
      | `Wait_period_after_previous_effect_starts_blocking ->
        every_wait_period_after_previous_effect_starts_blocking
      | `Wait_period_after_previous_effect_finishes_blocking ->
        every_wait_period_after_previous_effect_finishes_blocking
      | `Every_multiple_of_period_blocking -> every_multiple_of_period_blocking
      | `Every_multiple_of_period_non_blocking -> every_multiple_of_period_non_blocking
  ;;
end

module Memo = struct
  module Action = struct
    type 'query t =
      | Add of 'query
      | Remove of 'query
      | Change of 'query * 'query
    [@@deriving sexp_of]
  end

  type ('query, 'response) t =
    | T :
        { responses : ('query, 'response, 'cmp) Map.t
        ; inject : 'query Action.t -> unit Effect.t
        }
        -> ('query, 'response) t

  let create
        (type query cmp response)
        (module Query : Comparator with type t = query and type comparator_witness = cmp)
        ~(f : query Value.t -> response Computation.t)
    =
    let module Model = struct
      type t = int Map.M(Query).t [@@deriving sexp, equal]
    end
    in
    let module Action = struct
      type t = Query.t Action.t [@@deriving sexp_of]
    end
    in
    let apply_action ~inject:_ ~schedule_event:_ model (action : Action.t) =
      let add model q =
        Map.update model q ~f:(function
          | None -> 1
          | Some c -> c + 1)
      in
      let remove model q =
        Map.change model q ~f:(function
          | None -> None
          | Some 1 -> None
          | Some c -> Some (c - 1))
      in
      match action with
      | Add q -> add model q
      | Remove q -> remove model q
      | Change (before, after) -> add (remove model before) after
    in
    let%sub queries, inject =
      state_machine0
        (module Model)
        (module Action)
        ~apply_action
        ~default_model:(Map.empty (module Query))
    in
    let%sub responses = assoc (module Query) queries ~f:(fun query _count -> f query) in
    let%arr responses = responses
    and inject = inject in
    T { responses; inject }
  ;;

  let lookup (type query response) q_mod (t : (query, response) t Value.t) query =
    let%sub (T { inject; _ }) = return t in
    let%sub () =
      Edge.lifecycle
        ()
        ~on_activate:
          (let%map inject = inject
           and query = query in
           inject (Add query))
        ~on_deactivate:
          (let%map inject = inject
           and query = query in
           inject (Remove query))
    in
    let%sub () =
      let%sub callback =
        let%arr inject = inject in
        fun prev next ->
          match prev, next with
          | None, _ -> Effect.Ignore
          | Some prev, next -> inject (Change (prev, next))
      in
      Edge.on_change' q_mod query ~callback
    in
    let%arr t = t
    and query = query in
    let (T { responses; _ }) = t in
    Map.find responses query
  ;;
end

module Computation = struct
  type 'a t = 'a Computation.t

  include Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let return = const

      let map2 a b ~f =
        let%sub a = a in
        let%sub b = b in
        let%arr a = a
        and b = b in
        f a b
      ;;

      let map a ~f =
        let%sub a = a in
        let%arr a = a in
        f a
      ;;

      let map = `Custom map
    end)

  module Mapn = struct
    let map2 = map2

    let map3 t1 t2 t3 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      read (Value.map3 t1 t2 t3 ~f)
    ;;

    let map4 t1 t2 t3 t4 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      let%sub t4 = t4 in
      read (Value.map4 t1 t2 t3 t4 ~f)
    ;;

    let map5 t1 t2 t3 t4 t5 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      let%sub t4 = t4 in
      let%sub t5 = t5 in
      read (Value.map5 t1 t2 t3 t4 t5 ~f)
    ;;

    let map6 t1 t2 t3 t4 t5 t6 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      let%sub t4 = t4 in
      let%sub t5 = t5 in
      let%sub t6 = t6 in
      read (Value.map6 t1 t2 t3 t4 t5 t6 ~f)
    ;;

    let map7 t1 t2 t3 t4 t5 t6 t7 ~f =
      let%sub t1 = t1 in
      let%sub t2 = t2 in
      let%sub t3 = t3 in
      let%sub t4 = t4 in
      let%sub t5 = t5 in
      let%sub t6 = t6 in
      let%sub t7 = t7 in
      read (Value.map7 t1 t2 t3 t4 t5 t6 t7 ~f)
    ;;
  end

  include Mapn

  let rec all = function
    | [] -> return []
    | [ t1 ] -> map t1 ~f:(fun a1 -> [ a1 ])
    | [ t1; t2 ] -> map2 t1 t2 ~f:(fun a1 a2 -> [ a1; a2 ])
    | [ t1; t2; t3 ] -> map3 t1 t2 t3 ~f:(fun a1 a2 a3 -> [ a1; a2; a3 ])
    | [ t1; t2; t3; t4 ] -> map4 t1 t2 t3 t4 ~f:(fun a1 a2 a3 a4 -> [ a1; a2; a3; a4 ])
    | [ t1; t2; t3; t4; t5 ] ->
      map5 t1 t2 t3 t4 t5 ~f:(fun a1 a2 a3 a4 a5 -> [ a1; a2; a3; a4; a5 ])
    | [ t1; t2; t3; t4; t5; t6 ] ->
      map6 t1 t2 t3 t4 t5 t6 ~f:(fun a1 a2 a3 a4 a5 a6 -> [ a1; a2; a3; a4; a5; a6 ])
    | [ t1; t2; t3; t4; t5; t6; t7 ] ->
      map7 t1 t2 t3 t4 t5 t6 t7 ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
        [ a1; a2; a3; a4; a5; a6; a7 ])
    | t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: rest ->
      let left =
        map7 t1 t2 t3 t4 t5 t6 t7 ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
          [ a1; a2; a3; a4; a5; a6; a7 ])
      in
      let right = all rest in
      map2 left right ~f:(fun left right -> left @ right)
  ;;

  let reduce_balanced xs ~f =
    List.reduce_balanced xs ~f:(fun a b ->
      let%sub a = a in
      let%sub b = b in
      f a b)
  ;;

  let all_unit xs = all xs |> map ~f:(fun (_ : unit list) -> ())

  let all_map map_of_computations =
    map_of_computations
    |> Map.to_alist
    |> List.map ~f:(fun (key, data) -> map data ~f:(Tuple2.create key))
    |> all
    |> map ~f:(Map.of_alist_exn (Map.comparator_s map_of_computations))
  ;;

  module Let_syntax = struct
    let return = return

    include Applicative_infix

    module Let_syntax = struct
      let return = return
      let map = map
      let both = both

      include Mapn
    end
  end
end

module Value = struct
  include Value

  let cutoff t ~equal = cutoff ~added_by_let_syntax:false t ~equal
end

module Private = struct
  let conceal_value = Fn.id
  let reveal_value = Fn.id
  let conceal_computation = Fn.id
  let reveal_computation = Fn.id
end

module Map = Map_renamed_to_avoid_shadowing
