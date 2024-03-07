open! Core
open! Async_kernel
open! Import
open Js_of_ocaml
module Bonsai_action = Bonsai.Private.Action
module Tracker = Bonsai.Private.Stabilization_tracker

module type Result_spec = sig
  type t
  type extra
  type incoming

  val view : t -> Vdom.Node.t
  val extra : t -> extra
  val incoming : t -> incoming -> unit Vdom.Effect.t
end

module Arrow_deprecated = struct
  module Handle = struct
    module Injector = struct
      type 'a t =
        | Before_app_start of 'a Queue.t
        | Inject of ('a -> unit Vdom.Effect.t)
    end

    type ('input, 'extra, 'incoming, 'outgoing) t =
      { mutable injector : 'incoming Injector.t
      ; stop : unit Ivar.t
      ; started : unit Ivar.t
      ; input_var : 'input Incr.Var.t
      ; outgoing_pipe : 'outgoing Pipe.Reader.t
      ; extra : ('extra -> unit) Bus.Read_write.t
      ; last_extra : 'extra Moption.t
      }

    let create ~input_var ~outgoing_pipe =
      let extra =
        Bus.create_exn
          [%here]
          Arity1
          ~on_subscription_after_first_write:Allow_and_send_last_value
          ~on_callback_raise:(fun error -> eprint_s [%sexp (error : Error.t)])
      in
      let last_extra = Moption.create () in
      Bus.iter_exn extra [%here] ~f:(fun extra -> Moption.set_some last_extra extra);
      { injector = Before_app_start (Queue.create ())
      ; stop = Ivar.create ()
      ; started = Ivar.create ()
      ; input_var
      ; outgoing_pipe
      ; extra
      ; last_extra
      }
    ;;

    let stop t = Ivar.fill_if_empty t.stop ()
    let started t = Ivar.read t.started

    let schedule t a =
      match t.injector with
      | Inject f -> f a |> Vdom.Effect.Expert.handle_non_dom_event_exn
      | Before_app_start queue -> Queue.enqueue queue a
    ;;

    let set_started t = Ivar.fill_if_empty t.started ()

    let set_inject t inject =
      let prev = t.injector in
      t.injector <- Inject inject;
      match prev with
      | Inject _ -> ()
      | Before_app_start queue -> Queue.iter queue ~f:(schedule t)
    ;;

    let input t = Incr.Var.value t.input_var
    let set_input t input = Incr.Var.set t.input_var input
    let update_input t ~f = set_input t (f (input t))
    let outgoing { outgoing_pipe; _ } = outgoing_pipe
    let extra t = Bus.read_only t.extra
    let last_extra t = Moption.get t.last_extra
  end

  module App_input = struct
    type ('input, 'outgoing) t =
      { input : 'input
      ; inject_outgoing : 'outgoing -> unit Vdom.Effect.t
      }
    [@@deriving fields ~getters ~iterators:create]

    let create = Fields.create
  end

  module App_result = struct
    type ('extra, 'incoming) t =
      { view : Vdom.Node.t
      ; extra : 'extra
      ; inject_incoming : 'incoming -> unit Vdom.Effect.t
      }
    [@@deriving fields ~iterators:create]

    let create = Fields.create

    let of_result_spec
      (type result extra incoming)
      (module Result : Result_spec
        with type t = result
         and type extra = extra
         and type incoming = incoming)
      (r : Result.t)
      =
      { view = Result.view r
      ; extra = Result.extra r
      ; inject_incoming = Result.incoming r
      }
    ;;
  end

  let make_instrumented_computation ?host ?port ?worker_name component =
    let open Option.Let_syntax in
    match
      [%map
        let host = host
        and port = port
        and worker_name = worker_name in
        host, port, worker_name]
    with
    | Some (host, port, worker_name) ->
      Forward_performance_entries.instrument ~host ~port ~worker_name component
    | None ->
      print_endline "debugger host and port not be specified";
      { Forward_performance_entries.instrumented_computation = component
      ; shutdown = (fun () -> ())
      }
  ;;

  type debugging_state =
    | Not_debugging
    | Debugging of
        { host : string option
        ; port : int option
        ; worker_name : string option
        }

  let start_bonsai_debugger
    (is_debugging_var : debugging_state Incr.Var.t)
    (host : Js.js_string Js.t Js.Optdef.t)
    (port : int Js.Optdef.t)
    (worker_name : Js.js_string Js.t Js.Optdef.t)
    =
    match Incr.Var.value is_debugging_var with
    | Debugging _ -> print_endline "Already debugging."
    | Not_debugging ->
      print_endline "Starting the debugger.";
      Incr.Var.set
        is_debugging_var
        (Debugging
           { host = Js.Optdef.to_option host |> Option.map ~f:Js.to_string
           ; port = Js.Optdef.to_option port
           ; worker_name = Js.Optdef.to_option worker_name |> Option.map ~f:Js.to_string
           })
  ;;

  let start_generic_poly
    (type input action_input input_and_inject model action result extra incoming outgoing)
    ~(get_app_result : result -> (extra, incoming) App_result.t)
    ~(get_app_input :
        input:input
        -> inject_outgoing:(outgoing -> unit Vdom.Effect.t)
        -> input_and_inject)
    ~(initial_input : input)
    ~bind_to_element_with_id
    ~(computation : result Bonsai.Private.Computation.t)
    ~fresh
    ({ model; input = _; action; apply_action; run; reset = _; can_contain_path = _ } as
     info :
      (model, action, action_input, result) Bonsai.Private.Computation.info)
    : (input, extra, incoming, outgoing) Handle.t
    =
    let outgoing_pipe, pipe_write = Pipe.create () in
    let module Out_event =
      Virtual_dom.Vdom.Effect.Define (struct
        module Action = struct
          type t = outgoing
        end

        let handle = Pipe.write_without_pushback_if_open pipe_write
      end)
    in
    let input_var = Incr.Var.create initial_input in
    let handle = Handle.create ~input_var ~outgoing_pipe in
    let input =
      let%map.Incr input = Incr.Var.watch input_var in
      get_app_input ~input ~inject_outgoing:Out_event.inject
    in
    let prev_lifecycle = ref Bonsai.Private.Lifecycle.Collection.empty in
    let is_debugging_var = Incr.Var.create Not_debugging in
    let debugger_shutdown = ref None in
    let bonsai_clock = Bonsai.Time_source.create ~start:(Time_ns.now ()) in
    let tracker = Tracker.empty () in
    let module Incr_dom_app = struct
      module Model = struct
        type t = model

        let cutoff = phys_equal
      end

      module State = struct
        type t = unit
      end

      module Action = struct
        type t = action Bonsai_action.t

        let sexp_of_t = Bonsai_action.Type_id.to_sexp action
      end

      let action_requires_stabilization (action : Action.t) =
        Tracker.requires_stabilization tracker action
      ;;

      let on_action_application (action : Action.t) = Tracker.insert tracker action
      let on_stabilize () = Tracker.mark_stabilization tracker
      let on_startup ~schedule_action:_ _ = return ()

      let advance_clock_to to_ =
        Bonsai.Time_source.advance_clock bonsai_clock ~to_;
        Bonsai.Time_source.Private.flush bonsai_clock
      ;;

      let create
        model
        ~old_model:_
        ~inject
        (run : (model, action, action_input, result) Bonsai.Private.Computation.eval_fun)
        =
        let open Incr.Let_syntax in
        let environment =
          Bonsai.Private.Environment.(empty |> add_exn ~key:fresh ~data:input)
        in
        let snapshot =
          run
            ~environment
            ~path:Bonsai.Private.Path.empty
            ~clock:bonsai_clock
            ~model
            ~inject
          |> Bonsai.Private.Trampoline.run
        in
        let%map view =
          let%map { App_result.view; extra; inject_incoming } =
            snapshot |> Bonsai.Private.Snapshot.result >>| get_app_result
          in
          Handle.set_inject handle inject_incoming;
          Bus.write handle.extra extra;
          view
        and apply_action =
          let%map input =
            snapshot
            |> Bonsai.Private.Snapshot.input
            |> Bonsai.Private.Input.to_incremental
          in
          fun () ~schedule_event model action ->
            apply_action ~inject ~schedule_event (Some input) model action
        and on_display =
          let%map lifecycle = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot in
          fun () ~schedule_event ->
            Handle.set_started handle;
            schedule_event
              (Bonsai.Private.Lifecycle.Collection.diff !prev_lifecycle lifecycle);
            Bonsai.Time_source.Private.trigger_after_display bonsai_clock;
            prev_lifecycle := lifecycle
        in
        let update_visibility model ~schedule_event:_ = model in
        { Incr_dom.App_intf.Private.view; apply_action; update_visibility; on_display }
      ;;

      let create model ~old_model ~inject =
        let open Incr.Let_syntax in
        let safe_start computation =
          let (T info') = Bonsai.Private.gather computation in
          match
            Bonsai.Private.Meta.(
              ( Model.Type_id.same_witness info.model.type_id info'.model.type_id
              , Bonsai_action.Type_id.same_witness info.action info'.action
              , Input.same_witness info.input info'.input ))
          with
          | Some T, Some T, Some T -> create model ~old_model ~inject info'.run
          | _ ->
            print_endline
              "Not starting debugger. An error occurred while attempting to instrument \
               the computation; the resulting computation does not typecheck. Reusing \
               previously gathered run information to execute";
            create model ~old_model ~inject run
        in
        match%bind Incr.Var.watch is_debugging_var with
        | Debugging { host; port; worker_name } ->
          let { Forward_performance_entries.instrumented_computation; shutdown } =
            make_instrumented_computation ?host ?port ?worker_name computation
          in
          debugger_shutdown := Some shutdown;
          safe_start instrumented_computation
        | Not_debugging -> safe_start computation
      ;;
    end
    in
    Incr_dom.Start_app.Private.start_bonsai
      ~bind_to_element_with_id
      ~initial_model:model.default
      ~stop:(Ivar.read handle.stop)
      (module Incr_dom_app);
    let start_bonsai_debugger dry_run host port worker_name =
      let print_message () =
        print_endline
          "Not starting debugger. Be aware that running the debugger will send \
           performance data to the debugger server, which may be unacceptable if the \
           data you work with is sensitive. Consider running a local server and calling \
           this function again with the local host and port. If you wish to proceed, run \
           this function again, passing \"true\" as the first parameter"
      in
      Js.Optdef.case dry_run print_message (fun dry_run ->
        if Js.to_bool dry_run
        then (
          start_bonsai_debugger is_debugging_var host port worker_name;
          Incr.stabilize ())
        else print_message ())
    in
    let stop_bonsai_debugger () =
      Option.iter !debugger_shutdown ~f:(fun f -> f ());
      debugger_shutdown := None;
      Incr.Var.set is_debugging_var Not_debugging;
      Incr.stabilize ()
    in
    Js.Unsafe.global##.startBonsaiDebugger := Js.Unsafe.callback start_bonsai_debugger;
    Js.Unsafe.global##.stopBonsaiDebugger := Js.Unsafe.callback stop_bonsai_debugger;
    handle
  ;;

  let start_generic
    ~optimize
    ~get_app_result
    ~initial_input
    ~bind_to_element_with_id
    ~component
    =
    let fresh = Type_equal.Id.create ~name:"" sexp_of_opaque in
    let var =
      Bonsai.Private.Value.named App_input fresh |> Bonsai.Private.conceal_value
    in
    let computation =
      component var
      |> Bonsai.Private.top_level_handle
      |> if optimize then Bonsai.Private.pre_process else Fn.id
    in
    let (T info) = Bonsai.Private.gather computation in
    start_generic_poly
      ~get_app_result
      ~initial_input
      ~bind_to_element_with_id
      ~computation
      ~fresh
      info
  ;;

  (* I can't use currying here because of the value restriction. *)
  let start_standalone
    ?(optimize = true)
    ~initial_input
    ~bind_to_element_with_id
    component
    =
    start_generic
      ~optimize
      ~get_app_result:(fun view ->
        { App_result.view; extra = (); inject_incoming = Nothing.unreachable_code })
      ~get_app_input:(fun ~input ~inject_outgoing:_ -> input)
      ~initial_input
      ~bind_to_element_with_id
      ~component
  ;;

  let start ?(optimize = true) ~initial_input ~bind_to_element_with_id component =
    start_generic
      ~optimize
      ~get_app_result:Fn.id
      ~get_app_input:App_input.create
      ~initial_input
      ~bind_to_element_with_id
      ~component
  ;;
end

module Proc = struct
  module Handle = struct
    include Arrow_deprecated.Handle

    type ('extra, 'incoming) t =
      (unit, 'extra, 'incoming, Nothing.t) Arrow_deprecated.Handle.t
  end

  module Result_spec = struct
    module type S = Result_spec

    type ('r, 'extra, 'incoming) t =
      (module S with type t = 'r and type extra = 'extra and type incoming = 'incoming)

    module No_extra = struct
      type extra = unit

      let extra _ = ()
    end

    module No_incoming = struct
      type incoming = Nothing.t

      let incoming _ = Nothing.unreachable_code
    end

    let just_the_view =
      (module struct
        type t = Vdom.Node.t

        let view = Fn.id

        include No_extra
        include No_incoming
      end : S
        with type t = Vdom.Node.t
         and type extra = unit
         and type incoming = Nothing.t)
    ;;
  end

  let default_custom_connector _connector =
    raise_s
      [%message
        "The Bonsai app used a custom connector, but none was provided when the app was \
         started. To fix this, use the [~custom_connector] argument when calling \
         [Bonsai_web.Start.start]"]
  ;;

  let start_and_get_handle
    result_spec
    ?(optimize = true)
    ?(custom_connector = default_custom_connector)
    ~bind_to_element_with_id
    computation
    =
    let computation =
      Rpc_effect.Private.with_connector
        (function
         | Self -> Rpc_effect.Private.self_connector ()
         | Url url -> Rpc_effect.Private.url_connector url
         | Custom custom -> custom_connector custom)
        computation
    in
    let bonsai =
      Fn.const computation
      |> Bonsai.Arrow_deprecated.map
           ~f:(Arrow_deprecated.App_result.of_result_spec result_spec)
    in
    Arrow_deprecated.start ~optimize ~initial_input:() ~bind_to_element_with_id bonsai
  ;;

  let start ?custom_connector ?(bind_to_element_with_id = "app") component =
    let (_ : _ Handle.t) =
      start_and_get_handle
        Result_spec.just_the_view
        ~bind_to_element_with_id
        ?custom_connector
        component
    in
    ()
  ;;
end
