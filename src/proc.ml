open! Core
open! Import
include Proc_min

module Let_syntax = struct
  let return ~(here : [%call_pos]) v = read ~here v

  module Let_syntax = struct
    let sub = sub
    let switch = switch
    let return = return
    let map ~(here : [%call_pos]) t ~f = { (Value.map t ~f) with here }
    let both = Value.both
    let map2 = Value.map2
    let arr ~(here : [%call_pos]) t ~f = read ~here (map ~here t ~f)

    let cutoff ~(here : [%call_pos]) t ~equal =
      Value.cutoff ~here ~added_by_let_syntax:true t ~equal
    ;;

    include (Value : Mapn with type 'a t := 'a Value.t)

    let arr2 ~(here : [%call_pos]) t1 t2 ~f = read ~here (map2 ~here t1 t2 ~f)
    let arr3 ~(here : [%call_pos]) t1 t2 t3 ~f = read ~here (map3 ~here t1 t2 t3 ~f)
    let arr4 ~(here : [%call_pos]) t1 t2 t3 t4 ~f = read ~here (map4 ~here t1 t2 t3 t4 ~f)

    let arr5 ~(here : [%call_pos]) t1 t2 t3 t4 t5 ~f =
      read ~here (map5 ~here t1 t2 t3 t4 t5 ~f)
    ;;

    let arr6 ~(here : [%call_pos]) t1 t2 t3 t4 t5 t6 ~f =
      read ~here (map6 ~here t1 t2 t3 t4 t5 t6 ~f)
    ;;

    let arr7 ~(here : [%call_pos]) t1 t2 t3 t4 t5 t6 t7 ~f =
      read ~here (map7 ~here t1 t2 t3 t4 t5 t6 t7 ~f)
    ;;
  end

  let ( >>| ) ~(here : [%call_pos]) a f = Let_syntax.map ~here a ~f
  let ( <*> ) ~(here : [%call_pos]) f a = Value.map2 ~here f a ~f:(fun f a -> f a)
  let ( <$> ) ~(here : [%call_pos]) f a = Let_syntax.map ~here a ~f
end

open Let_syntax

module Let_syntax_with_map_location (Arg : sig
    val here : Source_code_position.t
  end) =
struct
  module Let_syntax = struct
    include Let_syntax

    let map ?(here = Arg.here) a ~f = map ~here a ~f
    let return ?(here = Arg.here) a = return ~here a
  end
end

let pure ~(here : [%call_pos]) f i = read ~here (Value.map ~here i ~f)
let const ~(here : [%call_pos]) x = read ~here (Value.return ~here x)
let with_model_resetter' = with_model_resetter

let fix2 ~(here : [%call_pos]) a b ~f =
  let a_and_b = Value.both a b in
  fix a_and_b ~f:(fun ~recurse a_and_b ->
    let recurse a b = recurse (Value.both a b) in
    let%sub a, b = return a_and_b in
    f ~recurse a b)
;;

let with_model_resetter ~(here : [%call_pos]) inside =
  with_model_resetter' ~here (fun ~reset ->
    sub ~here inside ~f:(fun r -> read ~here (Value.both ~here r reset)))
;;

let enum
  (type k)
  ~(here : [%call_pos])
  (module E : Enum with type t = k)
  ~match_
  ~(local_ with_)
  =
  let module E = struct
    include E
    include Comparator.Make (E)
  end
  in
  let forward_index = List.to_array E.all in
  let reverse_index =
    Map.of_alist_exn (module E) (List.mapi E.all ~f:(fun i k -> k, i))
  in
  let match_ = Value.map ~here match_ ~f:(Map.find_exn reverse_index) in
  let branches = Array.length forward_index in
  let with_ i = with_ (Array.get forward_index i) in
  Let_syntax.switch ~here ~match_ ~branches ~with_ [@nontail]
;;

let scope_model
  (type a cmp)
  ~(here : [%call_pos])
  (module M : Comparator.S with type t = a and type comparator_witness = cmp)
  ~on:v
  computation
  =
  let v = Value.map ~here v ~f:(fun k -> Map.singleton (module M) k ()) in
  let%sub map = assoc ~here (module M) v ~f:(fun _ _ -> computation) in
  let%arr map in
  (* This _exn is ok because we know that the map is a singleton *)
  let _k, r = Map.max_elt_exn map in
  r
;;

let of_module_with_input
  (type i m a r)
  ~(here : [%call_pos])
  ?sexp_of_model
  (component : (i, m, a, r) component_s)
  ?equal
  ~default_model
  input
  =
  let (module M) = component in
  let%sub input = return input in
  let%sub model_and_inject =
    state_machine_with_input
      ~sexp_of_action:M.Action.sexp_of_t
      ?sexp_of_model
      ?equal
      ~default_model
      ~apply_action:(fun context input model action ->
        match input with
        | Active input -> M.apply_action context input model action
        | Inactive ->
          eprint_s
            [%message
              [%here]
                "An action sent to an [of_module_with_input] has been dropped because \
                 its input was not present. This happens when the [of_module_with_input] \
                 is inactive when it receives a message."
                (action : M.Action.t)];
          model)
      input
  in
  let%arr model, inject = model_and_inject
  and input in
  M.compute ~inject input model
;;

let of_module2 ~(here : [%call_pos]) ?sexp_of_model c ?equal ~default_model i1 i2 =
  of_module_with_input ~here ?sexp_of_model c ?equal ~default_model (Value.both i1 i2)
;;

let race_dynamic_model
  (type m)
  ~here
  ?sexp_of_action
  ?sexp_of_model
  ~equal
  ~model
  ~apply_action
  input
  =
  let model_creator =
    match model with
    | `Given m ->
      Value.map ~here m ~f:(fun m -> function
        | None -> m
        | Some a -> a)
    | `Computed f -> f
  in
  let module M_actual = struct
    type model = m

    let equal_model = equal

    type t = model option [@@deriving equal]
  end
  in
  let apply_action context computation_status model action =
    let { Apply_action_context.Private.inject; schedule_event; time_source } =
      Apply_action_context.Private.reveal context
    in
    match computation_status with
    | Computation_status.Active (input, model_creator) ->
      let model = Some (model_creator model) in
      Some
        (apply_action
           ~inject
           ~schedule_event
           ~time_source
           (Computation_status.Active input)
           model
           action)
    | Inactive ->
      Some (apply_action ~inject ~schedule_event ~time_source Inactive model action)
  in
  let%sub model_and_inject =
    state_machine_with_input
      ~here
      ?sexp_of_action
      ?sexp_of_model:(Option.map sexp_of_model ~f:Option.sexp_of_t)
      ~equal:[%equal: M_actual.t]
      ~default_model:None
      ~apply_action
      (Value.both ~here input model_creator)
  in
  let%arr model, inject = model_and_inject
  and model_creator in
  model_creator model, inject
;;

let of_module
  (type m a r)
  ~(here : [%call_pos])
  ?sexp_of_model
  ?equal
  (component : (unit, m, a, r) component_s)
  ~default_model
  =
  let (module M) = component in
  let%sub model_and_inject =
    state_machine
      ~here
      ()
      ~sexp_of_action:M.Action.sexp_of_t
      ?sexp_of_model
      ?equal
      ~default_model
      ~apply_action:(fun context model action -> M.apply_action context () model action)
  in
  let%arr model, inject = model_and_inject in
  M.compute ~inject () model
;;

module Actor (Action : T1) = struct
  type get_apply_action_context =
    { get : 'a. unit -> ('a Action.t, 'a) Apply_action_context.t }

  type ('model, 'input) recv_with_input =
    { f :
        'a.
        get_apply_action_context
        -> 'input Computation_status.t
        -> 'model
        -> 'a Action.t
        -> 'model * 'a
    }

  type sexp_of_action = { f : 'a. 'a Action.t -> Sexp.t }

  type hidden_callback =
    | T : ('a Action.t, 'a) Effect.Private.Callback.t -> hidden_callback

  type inject = { f : 'a. 'a Action.t -> 'a Effect.t }

  let create_with_input
    : type input model.
      here:[%call_pos]
      -> ?sexp_of_action:sexp_of_action
      -> ?reset:(get_apply_action_context -> model -> model)
      -> ?sexp_of_model:(model -> Sexp.t)
      -> ?equal:(model -> model -> bool)
      -> default_model:model
      -> recv:(model, input) recv_with_input
      -> input Value.t
      -> (model * inject) Computation.t
    =
    fun ~(here : [%call_pos])
      ?(sexp_of_action = { f = sexp_of_opaque })
      ?reset
      ?sexp_of_model
      ?equal
      ~default_model
      ~recv
      input ->
    let open Let_syntax_with_map_location (struct
        let here = here
      end) in
    let sexp_of_action (T cb) = sexp_of_action.f (Effect.Private.Callback.request cb) in
    let make_inject
      (type a)
      ~(inject : hidden_callback -> unit Effect.t)
      ~schedule_event
      (action : a Action.t)
      : a Effect.t
      =
      Effect.Private.make ~request:action ~evaluator:(fun action ->
        schedule_event (inject (T action)) ~on_exn:(Effect.Private.Callback.on_exn action))
    in
    let ctx ~context =
      let { Apply_action_context.Private.inject; schedule_event; time_source } =
        Apply_action_context.Private.reveal context
      in
      { get =
          (fun () ->
            let inject =
              make_inject ~inject ~schedule_event:(fun action ~on_exn:_ ->
                schedule_event action)
            in
            Apply_action_context.Private.create ~inject ~schedule_event ~time_source)
      }
    in
    let reset = Option.map reset ~f:(fun f context model -> f (ctx ~context) model) in
    let%sub model, inject =
      state_machine_with_input
        ~here
        ~sexp_of_action
        ?sexp_of_model
        ?reset
        ?equal
        ~default_model
        ~apply_action:(fun context input model (T callback) ->
          let%tydi { schedule_event; _ } = Apply_action_context.Private.reveal context in
          let action = Effect.Private.Callback.request callback in
          let new_model, response = recv.f (ctx ~context) input model action in
          schedule_event (Effect.Private.Callback.respond_to callback response);
          new_model)
        input
    in
    let%sub inject =
      let%arr inject in
      let f action =
        make_inject
          ~inject
          ~schedule_event:(fun action ~on_exn -> Effect.Expert.handle action ~on_exn)
          action
      in
      ({ f } : inject)
    in
    let%arr model and inject in
    model, inject
  ;;

  type 'model recv =
    { f : 'a. get_apply_action_context -> 'model -> 'a Action.t -> 'model * 'a }

  let create
    ~(here : [%call_pos])
    ?sexp_of_action
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~recv
    ()
    =
    let recv : _ recv_with_input =
      { f =
          (fun get_apply_action_context (_ : unit Computation_status.t) model action ->
            recv.f get_apply_action_context model action)
      }
    in
    create_with_input
      ~here
      ?sexp_of_action
      ?sexp_of_model
      ?equal
      ?reset
      ~default_model
      ~recv
      (Value.return ~here ())
  ;;
end

let actor_with_input
  : type input model action return.
    here:[%call_pos]
    -> ?sexp_of_action:(action -> Sexp.t)
    -> ?reset:((action, return) Apply_action_context.t -> model -> model)
    -> ?sexp_of_model:(model -> Sexp.t)
    -> ?equal:(model -> model -> bool)
    -> default_model:model
    -> recv:
         ((action, return) Apply_action_context.t
          -> input Computation_status.t
          -> model
          -> action
          -> model * return)
    -> input Value.t
    -> (model * (action -> return Effect.t)) Computation.t
  =
  fun ~(here : [%call_pos])
    ?(sexp_of_action = sexp_of_opaque)
    ?reset
    ?sexp_of_model
    ?equal
    ~default_model
    ~recv
    input ->
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  let module Action_with_callback = struct
    let sexp_of_t cb = sexp_of_action (Effect.Private.Callback.request cb)
  end
  in
  let make_inject_with_on_exn ~inject ~schedule_event action =
    Effect.Private.make ~request:action ~evaluator:(fun action ->
      schedule_event (inject action) ~on_exn:(Effect.Private.Callback.on_exn action))
  in
  let make_inject ~inject ~schedule_event action =
    let schedule_event effect ~on_exn:_ = schedule_event effect in
    make_inject_with_on_exn ~inject ~schedule_event action
  in
  let reset =
    Option.map reset ~f:(fun f context model ->
      let { Apply_action_context.Private.inject; schedule_event; time_source } =
        Apply_action_context.Private.reveal context
      in
      let inject = make_inject ~inject ~schedule_event in
      let ctx =
        Apply_action_context.Private.create ~inject ~schedule_event ~time_source
      in
      f ctx model)
  in
  let%sub model, inject =
    state_machine_with_input
      ~here
      ~sexp_of_action:[%sexp_of: Action_with_callback.t]
      ?sexp_of_model
      ?reset
      ?equal
      ~default_model
      ~apply_action:(fun context input model callback ->
        let { Apply_action_context.Private.inject; schedule_event; time_source } =
          Apply_action_context.Private.reveal context
        in
        let inject = make_inject ~inject ~schedule_event in
        let ctx =
          Apply_action_context.Private.create ~inject ~schedule_event ~time_source
        in
        let action = Effect.Private.Callback.request callback in
        let new_model, response = recv ctx input model action in
        schedule_event (Effect.Private.Callback.respond_to callback response);
        new_model)
      input
  in
  let%sub inject =
    let%arr inject in
    make_inject_with_on_exn ~inject ~schedule_event:(fun effect ~on_exn ->
      Effect.Expert.handle ~on_exn effect)
  in
  let%arr model and inject in
  model, inject
;;

let actor
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~recv
  ()
  =
  let recv ctx (_ : unit Computation_status.t) = recv ctx in
  actor_with_input
    ~here
    ?sexp_of_action
    ?sexp_of_model
    ?equal
    ?reset
    ~default_model
    ~recv
    (Value.return ~here ())
;;

let state' (type model) ~(here : [%call_pos]) ?reset ?sexp_of_model ?equal default_model =
  let module Action = struct
    type t = Source_code_position.t * (model -> model) [@@deriving sexp_of]
  end
  in
  let reset =
    Option.map reset ~f:(fun reset (_ : _ Apply_action_context.t) m -> reset m)
  in
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  let%sub state, set_state =
    state_machine
      ~here
      ?reset
      ~sexp_of_action:[%sexp_of: Action.t]
      ?sexp_of_model
      ?equal
      ~apply_action:(fun (_ : _ Apply_action_context.t) old_model (_location, f) ->
        f old_model)
      ~default_model
      ()
  in
  let%sub set_state =
    let%arr set_state in
    fun prev -> set_state (here, prev)
  in
  let%arr state and set_state in
  state, set_state
;;

let state ~(here : [%call_pos]) ?reset ?sexp_of_model ?equal default_model =
  let sexp_of_action =
    (* NOTE: The model and the action for [state] are the same. *)
    Option.value ~default:sexp_of_opaque sexp_of_model
  in
  let reset =
    Option.map reset ~f:(fun reset (_ : _ Apply_action_context.t) m -> reset m)
  in
  state_machine
    ~here
    ?reset
    ~sexp_of_action
    ?sexp_of_model
    ?equal
    ~apply_action:(fun (_ : _ Apply_action_context.t) _old_model new_model -> new_model)
    ~default_model
    ()
;;

module Toggle = struct
  type t =
    { state : bool
    ; set_state : bool -> unit Effect.t
    ; toggle : unit Effect.t
    }
end

let toggle' ~(here : [%call_pos]) ~default_model () =
  let module Action = struct
    type t =
      | Toggle
      | Set of bool
    [@@deriving sexp]
  end
  in
  let%sub state_and_inject =
    state_machine
      ~here
      ~sexp_of_model:[%sexp_of: Bool.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~equal:[%equal: Bool.t]
      ~default_model
      ~apply_action:(fun (_ : _ Apply_action_context.t) state -> function
        | Toggle -> not state
        | Set state -> state)
      ()
  in
  let%arr state_and_inject in
  let state, inject = state_and_inject in
  { Toggle.state; set_state = (fun state -> inject (Set state)); toggle = inject Toggle }
;;

let toggle ~(here : [%call_pos]) ~default_model () =
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  let%sub { state; toggle; set_state = _ } = toggle' ~here ~default_model () in
  let%arr state and toggle in
  state, toggle
;;

let state_opt ~(here : [%call_pos]) ?reset ?default_model ?sexp_of_model ?equal () =
  state
    ~here
    ?reset
    default_model
    ?equal:(Option.map ~f:Option.equal equal)
    ?sexp_of_model:(Option.map ~f:Option.sexp_of_t sexp_of_model)
;;

let path_id ~(here : [%call_pos]) () =
  let%sub path = path ~here () in
  let%arr path in
  Path.to_unique_identifier_string path
;;

let yoink ~(here : [%call_pos]) a =
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  let%sub _, result =
    actor_with_input
      ~here
      ~sexp_of_model:[%sexp_of: Unit.t]
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~equal:[%equal: Unit.t]
      ~recv:(fun _ctx a () () -> (), a)
      ~default_model:()
      a
  in
  let%arr result in
  result ()
;;

module Edge = struct
  include Edge

  let lifecycle'
    ~(here : [%call_pos])
    ?on_activate
    ?on_deactivate
    ?before_display
    ?after_display
    ()
    =
    let open Let_syntax_with_map_location (struct
        let here = here
      end) in
    let transpose_join : 'a option Value.t option -> 'a option Value.t = function
      | Some a -> a
      | None -> Value.return ~here None
    in
    let%sub all =
      let%arr a = transpose_join on_activate
      and b = transpose_join on_deactivate
      and c = transpose_join before_display
      and d = transpose_join after_display in
      a, b, c, d
    in
    let%sub t =
      match%arr all with
      | None, None, None, None -> None
      | on_activate, on_deactivate, before_display, after_display ->
        Some { Lifecycle.on_activate; on_deactivate; before_display; after_display }
    in
    lifecycle ~here t
  ;;

  let lifecycle
    ~(here : [%call_pos])
    ?on_activate
    ?on_deactivate
    ?before_display
    ?after_display
    ()
    =
    lifecycle'
      ~here
      ?on_activate:(Option.map on_activate ~f:(Value.map ~here ~f:Option.some))
      ?on_deactivate:(Option.map on_deactivate ~f:(Value.map ~here ~f:Option.some))
      ?before_display:(Option.map before_display ~f:(Value.map ~here ~f:Option.some))
      ?after_display:(Option.map after_display ~f:(Value.map ~here ~f:Option.some))
      ()
  ;;

  let before_display' ~(here : [%call_pos]) event_opt_value =
    lifecycle' ~here ~before_display:event_opt_value ()
  ;;

  let after_display' ~(here : [%call_pos]) event_opt_value =
    lifecycle' ~here ~after_display:event_opt_value ()
  ;;

  let after_display ~(here : [%call_pos]) event_value =
    let event_value = Value.map ~here event_value ~f:Option.some in
    lifecycle' ~here ~after_display:event_value ()
  ;;

  let before_display ~(here : [%call_pos]) event_value =
    let event_value = Value.map ~here event_value ~f:Option.some in
    lifecycle' ~here ~before_display:event_value ()
  ;;

  let wait_before_display ~(here : [%call_pos]) () =
    Incr0.with_clock ~here (fun clock ->
      Ui_incr.return (Time_source.wait_before_display clock))
  ;;

  let wait_after_display ~(here : [%call_pos]) () =
    Incr0.with_clock ~here (fun clock ->
      Ui_incr.return (Time_source.wait_after_display clock))
  ;;

  let on_change'
    ~(here : [%call_pos])
    ?sexp_of_model
    ?(trigger = (`After_display : [ `Before_display | `After_display ]))
    ~equal
    input
    ~callback
    =
    let open Let_syntax_with_map_location (struct
        let here = here
      end) in
    let%sub state, set_state = state_opt ~here ?sexp_of_model ~equal () in
    let%sub update =
      match%sub state with
      | None ->
        let%arr set_state and input and callback in
        Some (Ui_effect.Many [ set_state (Some input); callback None input ])
      | Some state ->
        let%arr state and set_state and input and callback in
        if phys_equal state input || equal state input
        then None
        else
          lazy (Ui_effect.Many [ set_state (Some input); callback (Some state) input ])
          |> Ui_effect.lazy_
          |> Some
    in
    match trigger with
    | `Before_display -> before_display' ~here update
    | `After_display -> after_display' ~here update
  ;;

  let on_change ~(here : [%call_pos]) ?sexp_of_model ?trigger ~equal input ~callback =
    let callback =
      Value.map ~here callback ~f:(fun callback _prev value -> callback value)
    in
    on_change' ~here ?sexp_of_model ?trigger ~equal input ~callback
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
      ~here
      ?sexp_of_model
      ?equal
      ~initial
      ~wrap_result
      ~effect
      ()
      =
      let open Let_syntax_with_map_location (struct
          let here = here
        end) in
      let%sub _, next_seqnum =
        actor
          ~here
          ~sexp_of_model:[%sexp_of: Int.t]
          ~sexp_of_action:[%sexp_of: Unit.t]
          ~equal:[%equal: Int.t]
          ~default_model:0
          ~recv:(fun _ctx i () -> i + 1, i)
          ()
      in
      let module State = struct
        type result = r

        let sexp_of_result = Option.value ~default:sexp_of_opaque sexp_of_model
        let equal_result = Option.value equal ~default:[%eta2 phys_equal]

        type t =
          { last_seqnum : int
          ; last_result : result
          }
        [@@deriving sexp_of, equal]
      end
      in
      let module Action = struct
        type result = r

        let sexp_of_result = Option.value ~default:sexp_of_opaque sexp_of_model

        type t = Set of int * result [@@deriving sexp_of]
      end
      in
      let%sub state, inject_change =
        state_machine
          ~here
          ~sexp_of_model:[%sexp_of: State.t]
          ~sexp_of_action:[%sexp_of: Action.t]
          ~equal:[%equal: State.t]
          ~apply_action:
            (fun
              (_ : _ Apply_action_context.t) model (Action.Set (seqnum, res)) ->
            if seqnum < model.State.last_seqnum
            then model
            else { State.last_seqnum = seqnum; last_result = res })
          ~default_model:{ State.last_seqnum = -1; last_result = initial }
          ()
      in
      let%sub callback =
        let%arr effect and next_seqnum and inject_change in
        let%bind.Effect seqnum = next_seqnum () in
        let%bind.Effect result = effect in
        inject_change (Action.Set (seqnum, wrap_result result))
      in
      let%arr { State.last_result; _ } = state
      and callback in
      last_result, callback
    ;;

    let manual_refresh
      : type o r.
        here:[%call_pos]
        -> ?sexp_of_model:(o -> Sexp.t)
        -> ?equal:(o -> o -> bool)
        -> (o, r) Starting.t
        -> effect:o Effect.t Value.t
        -> (r * unit Effect.t) Computation.t
      =
      fun ~(here : [%call_pos]) ?sexp_of_model ?equal kind ~effect ->
      match kind with
      | Starting.Empty ->
        manual_refresh_implementation
          ~here
          ?sexp_of_model:(Option.map ~f:Option.sexp_of_t sexp_of_model)
          ?equal:(Option.map ~f:Option.equal equal)
          ~effect
          ~initial:None
          ~wrap_result:Option.some
          ()
      | Starting.Initial initial ->
        manual_refresh_implementation
          ~here
          ?sexp_of_model
          ?equal
          ~effect
          ~initial
          ~wrap_result:Fn.id
          ()
    ;;

    let effect_on_change
      : type a o r.
        here:[%call_pos]
        -> ?sexp_of_input:(a -> Sexp.t)
        -> ?sexp_of_result:(o -> Sexp.t)
        -> equal_input:(a -> a -> bool)
        -> ?equal_result:(o -> o -> bool)
        -> (o, r) Starting.t
        -> a Value.t
        -> effect:(a -> o Effect.t) Value.t
        -> r Computation.t
      =
      fun ~(here : [%call_pos])
        ?sexp_of_input
        ?sexp_of_result
        ~equal_input
        ?equal_result
        kind
        input
        ~effect ->
      let open Let_syntax_with_map_location (struct
          let here = here
        end) in
      let%sub get_input = yoink ~here input in
      let%sub effect =
        let%arr get_input and effect in
        let%bind.Effect input =
          match%bind.Effect get_input with
          | Active input -> Effect.return input
          | Inactive -> Effect.never
        in
        effect input
      in
      let%sub result, refresh =
        manual_refresh
          ~here
          ?sexp_of_model:sexp_of_result
          ?equal:equal_result
          kind
          ~effect
      in
      let%sub callback =
        let%arr refresh in
        fun (_ : a) -> refresh
      in
      let%sub () =
        on_change ~here ?sexp_of_model:sexp_of_input ~equal:equal_input input ~callback
      in
      return ~here result
    ;;
  end
end

module Effect_throttling = struct
  module Poll_result = struct
    type 'a t =
      | Aborted
      | Finished of 'a
    [@@deriving sexp, equal]

    let request_aborted_error = Error.of_string "request was aborted"

    let collapse_to_or_error ?tag_s =
      let maybe_tag_error error =
        match tag_s with
        | None -> error
        | Some (lazy tag) -> Error.tag_s error ~tag
      in
      function
      | Aborted -> Error (maybe_tag_error request_aborted_error)
      | Finished (Error e) -> Error (maybe_tag_error e)
      | Finished (Ok v) -> Ok v
    ;;

    let collapse_fun_to_or_error ?sexp_of_input f a =
      let tag_s =
        match sexp_of_input with
        | None -> None
        | Some sexp_of_input ->
          Some (lazy (Sexp.List [ Sexp.Atom "for"; sexp_of_input a ]))
      in
      Effect.map (Effect.lazy_ (lazy (f a))) ~f:(collapse_to_or_error ?tag_s)
    ;;
  end

  let poll
    : type a b.
      here:[%call_pos]
      -> (a -> b Effect.t) Value.t
      -> (a -> b Poll_result.t Effect.t) Computation.t
    =
    fun ~(here : [%call_pos]) effect ->
    let open Let_syntax_with_map_location (struct
        let here = here
      end) in
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
      let equal = [%eta2 phys_equal]
    end
    in
    let%sub _model, inject =
      state_machine_with_input
        ~here
        ~sexp_of_action:[%sexp_of: Action.t]
        ~sexp_of_model:[%sexp_of: Model.t]
        ~equal:[%equal: Model.t]
          (* This computation does nothing on reset because users should be oblivious to
             the fact that it has a model. I don't think there is a "correct" decision in
             this case - this behavior just seems more reasonable to me. *)
        ~reset:(fun (_ : _ Apply_action_context.t) model -> model)
        ~default_model:{ running = false; next_up = None }
        ~apply_action:(fun context effect { running; next_up } action ->
          let { Apply_action_context.Private.inject; schedule_event; time_source = _ } =
            Apply_action_context.Private.reveal context
          in
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
               inject Action.Finished)
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
          (* There are a lot of cases, and perhaps this match expression could be factored
             to be shorter, but the advantage to this is that every case is extremely
             short, and it's easy to find which code path a set of variable configurations
             will take. *)
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
            (* This case is untested because I couldn't figure out how to reach this code
               path in tests. It seems impossible. *)
            run_effect effect next_up;
            { running = true; next_up = Some callback }
          | Run callback, true, None, (Inactive | Active _) ->
            { running = true; next_up = Some callback }
          | Run callback, true, Some next_up, (Inactive | Active _) ->
            abort next_up;
            { running = true; next_up = Some callback }
          | Activate, running, next_up, Inactive ->
            (* This case looks impossible because [Activate] events happen after a
               computation is activated, so it should have access to the input. However,
               it can happen if a computation is activated and de-activated the next
               frame. The Activate effect doesn't run until the frame in which it was
               deactivated, which means it doesn't have access to the input. *)
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
      let%arr inject in
      inject Activate
    in
    let%sub () = Edge.lifecycle ~here ~on_activate () in
    let%arr inject in
    fun request ->
      Effect.Private.make ~request ~evaluator:(fun callback ->
        Effect.Expert.handle
          (inject (Run callback))
          ~on_exn:(Effect.Private.Callback.on_exn callback))
  ;;
end

module Incr = struct
  include Proc_min.Proc_incr
  include Incr0
end

module Map_and_set0 = Map_and_set0.Make (struct
    module Value = Value
    module Computation = Computation
    module Incr = Incr
  end)

module Map0 = Map_and_set0.Map

let freeze ~(here : [%call_pos]) ?sexp_of_model ?equal value =
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  let%sub state, set_state = state_opt ~here ?sexp_of_model ?equal () in
  match%sub state with
  | Some state -> return ~here state
  | None ->
    let%sub () =
      let%sub on_activate =
        let%arr set_state and value in
        set_state (Some value)
      in
      Edge.lifecycle ~here ~on_activate ()
    in
    return ~here value
;;

let thunk (type a) ~(here : [%call_pos]) (f : unit -> a) =
  let%sub out = return ~here Value.(map ~here (Var.value ~here (Var.create ())) ~f) in
  freeze ~here ~sexp_of_model:[%sexp_of: opaque] ~equal:[%eta2 phys_equal] out
;;

let most_recent_some ~(here : [%call_pos]) ?sexp_of_model ~equal input ~f =
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  let%sub most_recent_valid_value, set_most_recent_valid_value =
    state_opt ~here ?sexp_of_model ~equal ()
  in
  let%sub input = pure ~here f input in
  let%sub input = Incr.value_cutoff ~here ~equal:(Option.equal equal) input in
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  match%sub input with
  | None -> return ~here most_recent_valid_value
  | Some inner ->
    let%sub callback =
      let%arr set_most_recent_valid_value in
      fun x -> set_most_recent_valid_value (Some x)
    in
    let%sub () = Edge.on_change ~here ?sexp_of_model ~equal inner ~callback in
    return ~here input
;;

let most_recent_value_satisfying
  ~(here : [%call_pos])
  ?sexp_of_model
  ~equal
  input
  ~condition
  =
  most_recent_some ~here ?sexp_of_model ~equal input ~f:(fun a ->
    if condition a then Some a else None)
;;

let previous_value
  :  here:[%call_pos] -> ?sexp_of_model:('a -> Sexp.t) -> equal:('a -> 'a -> bool)
  -> 'a Value.t -> 'a option Computation.t
  =
  fun ~(here : [%call_pos]) ?sexp_of_model ~equal input ->
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  let%sub prev, set_prev = state_opt ~here ?sexp_of_model ~equal () in
  let%sub callback =
    let%arr set_prev in
    fun input -> set_prev (Some input)
  in
  let%sub () = Edge.on_change ~here ?sexp_of_model ~equal input ~callback in
  return ~here prev
;;

let assoc_set ~(here : [%call_pos]) m v ~(local_ f) =
  let%sub as_map = Map0.of_set ~here v in
  assoc ~here m as_map ~f:(fun k _ -> f k) [@nontail]
;;

let assoc_list
  (type key cmp)
  ~(here : [%call_pos])
  (m : (key, cmp) Comparator.Module.t)
  list
  ~get_key
  ~(local_ f)
  =
  let module M = (val m) in
  let open Let_syntax_with_map_location (struct
      let here = here
    end) in
  let%sub alist =
    let%arr list in
    List.map list ~f:(fun x -> get_key x, x)
  in
  let%sub input_map =
    let%arr alist in
    Map.of_alist (module M) alist
  in
  match%sub input_map with
  | `Ok input_map ->
    let%sub output_map = assoc ~here m input_map ~f:(fun k v -> f k v) [@nontail] in
    let%arr alist and output_map in
    `Ok
      (List.map alist ~f:(fun (k, _) ->
         match Map.find output_map k with
         | Some r -> r
         | None ->
           raise_s
             [%message
               "BUG"
                 [%here]
                 "Incremental glitch"
                 ~key:((Comparator.sexp_of_t M.comparator) k : Sexp.t)
                 "not found"]))
  | `Duplicate_key key ->
    let%arr key in
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

  let rec fetch
    : type a b.
      here:[%call_pos] -> a t -> default:b -> for_some:(a -> b) -> b Computation.t
    =
    fun ~(here : [%call_pos]) t ~default ~for_some ->
    match t with
    | Independent { id; _ } -> Dynamic_scope.fetch ~here ~id ~default ~for_some ()
    | Derived { base; get; set = _; sexp_of = _ } ->
      fetch ~here base ~default ~for_some:(fun x -> for_some (get x))
  ;;

  let lookup (type a) ~(here : [%call_pos]) (var : a t) =
    fetch ~here var ~default:(fallback var) ~for_some:Fn.id
  ;;

  let rec store
    : type a. here:[%call_pos] -> a t -> a Value.t -> 'r Computation.t -> 'r Computation.t
    =
    fun ~(here : [%call_pos]) var value inner ->
    match var with
    | Independent { id; _ } -> Dynamic_scope.store ~here ~id ~value ~inner ()
    | Derived { base; get = _; set; sexp_of = _ } ->
      let%sub current = lookup ~here base in
      let%sub new_ =
        let%arr current and value in
        set current value
      in
      store ~here base new_ inner
  ;;

  let create ?(sexp_of = sexp_of_opaque) ~name ~fallback () =
    Independent { id = Type_equal.Id.create ~name sexp_of; fallback }
  ;;

  let derived ?(sexp_of = sexp_of_opaque) base ~get ~set =
    Derived { base; get; set; sexp_of }
  ;;

  type revert = { revert : 'a. 'a Computation.t -> 'a Computation.t }

  let set ~(here : [%call_pos]) t v ~inside = store ~here t v inside
end

module Clock = struct
  let approx_now ~(here : [%call_pos]) ~tick_every () =
    Incr.with_clock ~here (fun clock ->
      let%map.Ui_incr () = Time_source.at_intervals clock tick_every in
      Time_source.now clock)
  ;;

  let now ~(here : [%call_pos]) () = Incr.with_clock ~here Time_source.watch_now

  module Before_or_after = struct
    type t = Ui_incr.Before_or_after.t =
      | Before
      | After
    [@@deriving sexp, equal]
  end

  let at ~(here : [%call_pos]) time =
    Incr.compute_with_clock ~here time ~f:(fun clock ->
      Ui_incr.bind ~f:(Time_source.at clock))
  ;;

  let get_current_time ~(here : [%call_pos]) () =
    Incr.with_clock ~here (fun clock ->
      Ui_incr.return (Effect.of_sync_fun (fun () -> Time_source.now clock) ()))
  ;;

  let sleep ~(here : [%call_pos]) () =
    Incr.with_clock ~here (fun clock -> Ui_incr.return (Time_source.sleep clock))
  ;;

  let until ~(here : [%call_pos]) () =
    Incr.with_clock ~here (fun clock -> Ui_incr.return (Time_source.until clock))
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

  let generic_every ~here ~create_effect ?(trigger_on_activate = true) span callback =
    let open Let_syntax_with_map_location (struct
        let here = here
      end) in
    let%sub base_time =
      let%sub now = now ~here () in
      freeze
        ~here
        ~equal:[%equal: Time_ns.Alternate_sexp.t]
        now
        ~sexp_of_model:[%sexp_of: Time_ns.Alternate_sexp.t]
    in
    let%sub initial_model =
      let%arr base_time and span in
      let start_time =
        if trigger_on_activate then base_time else Time_ns.add_saturating base_time span
      in
      Every_model.Waiting_for (None, start_time)
    in
    let%sub race_input =
      let%arr base_time and callback and span in
      span, base_time, callback
    in
    let apply_action ~inject ~schedule_event ~time_source input _old_model = function
      | Every_action.Schedule_effect ->
        (match input with
         | Computation_status.Inactive -> Every_model.Waiting_for (None, Time_ns.epoch)
         | Active (span, base_time, callback) ->
           let get_current_time () = Time_source.now time_source in
           schedule_event
             (let%bind.Effect next_time =
                create_effect ~span ~base_time ~get_current_time ~callback
              in
              inject (Every_action.Wait_for next_time));
           Every_model.Waiting_for_effect_to_finish)
      | Wait_for next_time -> Waiting_for (Some (Trigger_id.create ()), next_time)
    in
    let%sub every_model, inject =
      race_dynamic_model
        ~here
        ~sexp_of_model:[%sexp_of: Every_model.t]
        ~sexp_of_action:[%sexp_of: Every_action.t]
        ~equal:[%equal: Every_model.t]
        ~model:(`Given initial_model)
        ~apply_action
        race_input
    in
    let%sub before_or_after =
      match%sub every_model with
      | Waiting_for_effect_to_finish -> const ~here None
      | Waiting_for (trigger_id, time) ->
        let%sub before_or_after = at ~here time in
        let%arr trigger_id and before_or_after in
        Some (trigger_id, before_or_after)
    in
    let%sub callback =
      let%arr inject in
      function
      | None | Some (_, Before_or_after.Before) -> Effect.Ignore
      | Some (_, After) -> inject Schedule_effect
    in
    Edge.on_change
      ~here
      ~sexp_of_model:[%sexp_of: (Trigger_id.t option * Before_or_after.t) option]
      ~equal:[%equal: (Trigger_id.t option * Before_or_after.t) option]
      before_or_after
      ~callback
  ;;

  let ensure_clock_advances old_time new_time =
    Time_ns.max (Time_ns.next old_time) new_time
  ;;

  let every_wait_period_after_previous_effect_finishes_blocking =
    generic_every ~create_effect:(fun ~span ~base_time:_ ~get_current_time ~callback ->
      let%map.Effect () = callback in
      let now = get_current_time () in
      ensure_clock_advances now (Time_ns.add_saturating now span))
  ;;

  let every_wait_period_after_previous_effect_starts_blocking =
    generic_every ~create_effect:(fun ~span ~base_time:_ ~get_current_time ~callback ->
      let start = get_current_time () in
      let%map.Effect () = callback in
      let now = get_current_time () in
      ensure_clock_advances now (Time_ns.add_saturating start span))
  ;;

  let every_multiple_of_period_blocking =
    generic_every ~create_effect:(fun ~span ~base_time ~get_current_time ~callback ->
      let%map.Effect () = callback in
      let now = get_current_time () in
      ensure_clock_advances
        now
        (Time_ns.next_multiple
           ~can_equal_after:false
           ~base:base_time
           ~after:now
           ~interval:(Time_ns.Span.max span (Time_ns.Span.next Time_ns.Span.zero))
           ()))
  ;;

  let every_multiple_of_period_non_blocking ~here ?trigger_on_activate span callback =
    let%sub callback =
      let%arr callback in
      Effect.Many [ callback ]
    in
    every_multiple_of_period_blocking ~here ?trigger_on_activate span callback
  ;;

  let every
    :  here:[%call_pos]
    -> when_to_start_next_effect:
         [< `Wait_period_after_previous_effect_starts_blocking
         | `Wait_period_after_previous_effect_finishes_blocking
         | `Every_multiple_of_period_non_blocking
         | `Every_multiple_of_period_blocking
         ]
    -> ?trigger_on_activate:bool -> Time_ns.Span.t Value.t -> unit Effect.t Value.t
    -> unit Computation.t
    =
    fun ~(here : [%call_pos]) ~when_to_start_next_effect ->
    match when_to_start_next_effect with
    | `Wait_period_after_previous_effect_starts_blocking ->
      every_wait_period_after_previous_effect_starts_blocking ~here
    | `Wait_period_after_previous_effect_finishes_blocking ->
      every_wait_period_after_previous_effect_finishes_blocking ~here
    | `Every_multiple_of_period_blocking -> every_multiple_of_period_blocking ~here
    | `Every_multiple_of_period_non_blocking ->
      every_multiple_of_period_non_blocking ~here
  ;;
end

module Memo = struct
  module Action = struct
    type 'query t =
      | Set_subscriber_query of Path.t * 'query
      | Unsubscribe of Path.t
    [@@deriving sexp_of]
  end

  type ('query, 'response) t =
    | T :
        { subscribers : 'query Path.Map.t
        ; responses : ('query, 'response, 'cmp) Map.t
        ; inject : 'query Action.t -> unit Effect.t
        ; path : Path.t
        ; query_equal : 'query -> 'query -> bool
        }
        -> ('query, 'response) t

  let create
    (type query cmp response)
    ~(here : [%call_pos])
    (module Query : Comparator.S with type t = query and type comparator_witness = cmp)
    ~(local_ f : query Value.t -> response Computation.t)
    =
    let module Query = struct
      include Query

      let equal = Comparable.equal (Comparator.compare comparator)
      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let open Let_syntax_with_map_location (struct
        let here = here
      end) in
    let module Model = struct
      type t = Query.t Path.Map.t [@@deriving sexp_of, equal]
    end
    in
    let%sub path = path ~here () in
    let%sub subscribers, inject =
      state_machine
        ~here
        ~sexp_of_model:[%sexp_of: Model.t]
        ~sexp_of_action:[%sexp_of: Query.t Action.t]
        ~equal:[%equal: Model.t]
        ~default_model:Path.Map.empty
        ~apply_action:(fun _ model -> function
          | Set_subscriber_query (path, query) -> Map.set model ~key:path ~data:query
          | Unsubscribe path -> Map.remove model path)
        ()
    in
    let%sub queries =
      Map0.unordered_fold
        ~here
        subscribers
        ~init:(Map.empty (module Query))
        ~add:(fun ~key:_path ~data:query acc ->
          Map.change acc query ~f:(function
            | None -> Some 1
            | Some n -> Some (n + 1)))
        ~remove:(fun ~key:_path ~data:query acc ->
          Map.change acc query ~f:(function
            | None -> None
            | Some 1 -> None
            | Some n -> Some (n - 1)))
    in
    let%sub responses =
      assoc ~here (module Query) queries ~f:(fun query (_ : int Value.t) -> f query)
    in
    let%arr subscribers and responses and inject and path in
    T { subscribers; responses; inject; path; query_equal = Query.equal }
  ;;

  let lookup
    (type query response)
    ~(here : [%call_pos])
    (t : (query, response) t Value.t)
    query
    =
    let open Let_syntax_with_map_location (struct
        let here = here
      end) in
    let%sub (T { inject; path = memo_path; _ }) = return ~here t in
    (* This [scope_model] is needed so that whenever a [lookup] gets reassigned to a new
       [Memo.t] (e.g. one of several [Memo.t]s is returned from a [match%sub], or a
       [Memo.t] is wrapped in a [scope_model]), [lookup]s get reassigned from the old
       [Memo.t] to the new one. *)
    let%sub my_path = path ~here () in
    scope_model
      ~here
      (module Path)
      ~on:memo_path
      (let%sub () =
         let%sub on_deactivate =
           let%arr inject and my_path in
           Some (inject (Unsubscribe my_path))
         in
         let%sub after_display =
           let%arr t and query and my_path in
           let (T { subscribers; inject; query_equal; _ }) = t in
           let resubscribe = Some (inject (Set_subscriber_query (my_path, query))) in
           match Map.find subscribers my_path with
           | None -> resubscribe
           | Some memo_query when not (query_equal query memo_query) -> resubscribe
           | Some _ -> None
         in
         Edge.lifecycle' ~here () ~on_deactivate ~after_display
       in
       let%arr t and query in
       let (T { responses; _ }) = t in
       Map.find responses query)
  ;;

  type ('query, 'response) responses =
    | T : ('query, 'response, 'cmp) Map.t -> ('query, 'response) responses

  let responses (type query response) (t : (query, response) t) =
    let (T { responses; _ }) = t in
    T responses
  ;;
end

module Apply_action_context = Apply_action_context

module Computation = struct
  type 'a t = 'a Computation.t

  include Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let return ~here:(_ : [%call_pos]) x = const x

      let map2 ~(here : [%call_pos]) a b ~f =
        sub ~here a ~f:(fun a ->
          sub ~here b ~f:(fun b -> Let_syntax.return ~here (Let_syntax.map2 ~here a b ~f)))
      ;;

      let map ~(here : [%call_pos]) a ~f =
        sub ~here a ~f:(fun a -> Let_syntax.arr ~here a ~f)
      ;;

      let map = `Custom map
    end)

  module Mapn = struct
    let map2 = map2

    let map3 ~(here : [%call_pos]) t1 t2 t3 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      read ~here (Value.map3 ~here t1 t2 t3 ~f)
    ;;

    let map4 ~(here : [%call_pos]) t1 t2 t3 t4 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      read ~here (Value.map4 ~here t1 t2 t3 t4 ~f)
    ;;

    let map5 ~(here : [%call_pos]) t1 t2 t3 t4 t5 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      let%sub t5 in
      read ~here (Value.map5 ~here t1 t2 t3 t4 t5 ~f)
    ;;

    let map6 ~(here : [%call_pos]) t1 t2 t3 t4 t5 t6 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      let%sub t5 in
      let%sub t6 in
      read ~here (Value.map6 ~here t1 t2 t3 t4 t5 t6 ~f)
    ;;

    let map7 ~(here : [%call_pos]) t1 t2 t3 t4 t5 t6 t7 ~f =
      let%sub t1 in
      let%sub t2 in
      let%sub t3 in
      let%sub t4 in
      let%sub t5 in
      let%sub t6 in
      let%sub t7 in
      read ~here (Value.map7 ~here t1 t2 t3 t4 t5 t6 t7 ~f)
    ;;
  end

  include Mapn

  let rec all ~(here : [%call_pos]) = function
    | [] -> return ~here []
    | [ t1 ] -> map t1 ~here ~f:(fun a1 -> [ a1 ])
    | [ t1; t2 ] -> map2 t1 t2 ~here ~f:(fun a1 a2 -> [ a1; a2 ])
    | [ t1; t2; t3 ] -> map3 t1 t2 t3 ~here ~f:(fun a1 a2 a3 -> [ a1; a2; a3 ])
    | [ t1; t2; t3; t4 ] ->
      map4 t1 t2 t3 t4 ~here ~f:(fun a1 a2 a3 a4 -> [ a1; a2; a3; a4 ])
    | [ t1; t2; t3; t4; t5 ] ->
      map5 t1 t2 t3 t4 t5 ~here ~f:(fun a1 a2 a3 a4 a5 -> [ a1; a2; a3; a4; a5 ])
    | [ t1; t2; t3; t4; t5; t6 ] ->
      map6 t1 t2 t3 t4 t5 t6 ~here ~f:(fun a1 a2 a3 a4 a5 a6 ->
        [ a1; a2; a3; a4; a5; a6 ])
    | [ t1; t2; t3; t4; t5; t6; t7 ] ->
      map7 t1 t2 t3 t4 t5 t6 t7 ~here ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
        [ a1; a2; a3; a4; a5; a6; a7 ])
    | t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: rest ->
      let left =
        map7 t1 t2 t3 t4 t5 t6 t7 ~here ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
          [ a1; a2; a3; a4; a5; a6; a7 ])
      in
      let right = all ~here rest in
      map2 left right ~here ~f:(fun left right -> left @ right)
  ;;

  let reduce_balanced ~(here : [%call_pos]) xs ~f =
    List.reduce_balanced xs ~f:(fun a b ->
      let%sub a in
      let%sub b in
      f a b)
  ;;

  let fold_right ~(here : [%call_pos]) xs ~f ~init =
    List.fold_right xs ~init:(read ~here init) ~f:(fun a b ->
      let%sub a in
      let%sub b in
      f a b)
  ;;

  let all_unit ~(here : [%call_pos]) xs =
    all ~here xs |> map ~here ~f:(fun (_ : unit list) -> ())
  ;;

  let all_map ~(here : [%call_pos]) map_of_computations =
    map_of_computations
    |> Map.to_alist
    |> List.map ~f:(fun (key, data) -> map ~here data ~f:(Tuple2.create key))
    |> all ~here
    |> map ~here ~f:(Map.of_alist_exn (Map.comparator_s map_of_computations))
  ;;

  module Let_syntax = struct
    let return = return

    include Applicative_infix

    module Let_syntax = struct
      let return = return
      let map = map
      let both = both

      include Mapn

      let arr2 = map2
      let arr3 = map3
      let arr4 = map4
      let arr5 = map5
      let arr6 = map6
      let arr7 = map7
    end
  end
end

module Value = struct
  include Value

  let cutoff t ~equal = cutoff ~added_by_let_syntax:false t ~equal
end

module Expert = struct
  let thunk = thunk
  let assoc_on = assoc_on
end

module Map = Map_and_set0
