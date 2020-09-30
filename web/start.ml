open! Core_kernel
open! Async_kernel
open! Import

module type Result_spec = sig
  type t
  type extra
  type incoming

  val view : t -> Vdom.Node.t
  val extra : t -> extra
  val incoming : t -> incoming -> Vdom.Event.t
end

module Arrow = struct
  module Handle = struct
    module Injector = struct
      type 'a t =
        | Before_app_start of 'a Queue.t
        | Inject of ('a -> Vdom.Event.t)
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
        Bus.create
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
      | Inject f -> f a |> Vdom.Event.Expert.handle_non_dom_event_exn
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
      ; inject_outgoing : 'outgoing -> Vdom.Event.t
      }
    [@@deriving fields]

    let create = Fields.create
  end

  module App_result = struct
    type ('extra, 'incoming) t =
      { view : Vdom.Node.t
      ; extra : 'extra
      ; inject_incoming : 'incoming -> Vdom.Event.t
      }
    [@@deriving fields]

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

  let start_generic_poly
        (type input input_and_inject model action result extra incoming outgoing)
        ~(get_app_result : result -> (extra, incoming) App_result.t)
        ~(get_app_input :
            input:input -> inject_outgoing:(outgoing -> Vdom.Event.t) -> input_and_inject)
        ~(initial_input : input)
        ~(initial_model : model)
        ~bind_to_element_with_id
        ~(computation : (model, action, result) Bonsai.Private.Computation.t)
        ~fresh
        ~(action_type_id : action Type_equal.Id.t)
    : (input, extra, incoming, outgoing) Handle.t
    =
    let outgoing_pipe, pipe_write = Pipe.create () in
    let module Out_event =
      Virtual_dom.Vdom.Event.Define (struct
        module Action = struct
          type t = outgoing
        end

        let handle = Pipe.write_without_pushback_if_open pipe_write
      end)
    in
    let input_var = Incr.Var.create initial_input in
    let handle = Handle.create ~input_var ~outgoing_pipe in
    let module Incr_dom_app = struct
      module Model = struct
        type t = model

        let cutoff = phys_equal
      end

      module State = struct
        type t = unit
      end

      module Action = struct
        type t = action

        let sexp_of_t = Type_equal.Id.to_sexp action_type_id
      end

      let on_startup ~schedule_action:_ _ = return ()

      let create model ~old_model:_ ~inject =
        let open Incr.Let_syntax in
        let input =
          let%map input = Incr.Var.watch input_var in
          get_app_input ~input ~inject_outgoing:Out_event.inject
        in
        let environment =
          Bonsai.Private.Environment.(empty |> add_exn ~key:fresh ~data:input)
        in
        let%map snapshot =
          Bonsai.Private.eval
            ~environment
            ~path:Bonsai.Private.Path.empty
            ~model
            ~inject
            computation
        and model = model in
        let apply_action = Bonsai.Private.Snapshot.apply_action snapshot in
        let apply_action action () ~schedule_action:_ =
          apply_action ~schedule_event:Vdom.Event.Expert.handle_non_dom_event_exn action
        in
        let result = Bonsai.Private.Snapshot.result snapshot in
        let { App_result.view; extra; inject_incoming } = get_app_result result in
        Handle.set_inject handle inject_incoming;
        Bus.write handle.extra extra;
        let on_display () ~schedule_action:_ = Handle.set_started handle in
        Incr_dom.Component.create ~apply_action ~on_display model view
      ;;
    end
    in
    Incr_dom.Start_app.start
      ~bind_to_element_with_id
      ~initial_model
      ~stop:(Ivar.read handle.stop)
      (module Incr_dom_app);
    handle
  ;;

  let start_generic ~get_app_result ~initial_input ~bind_to_element_with_id ~component =
    let fresh = Type_equal.Id.create ~name:"" sexp_of_opaque in
    let var = Bonsai.Private.Value.named fresh |> Bonsai.Private.conceal_value in
    let computation = component var |> Bonsai.Private.reveal_computation in
    let (Bonsai.Private.Computation.T { t; action; model }) = computation in
    start_generic_poly
      ~get_app_result
      ~initial_input
      ~initial_model:model.default
      ~bind_to_element_with_id
      ~computation:t
      ~fresh
      ~action_type_id:action
  ;;

  (* I can't use currying here because of the value restriction. *)
  let start_standalone ~initial_input ~bind_to_element_with_id component =
    start_generic
      ~get_app_result:(fun view ->
        { App_result.view; extra = (); inject_incoming = Nothing.unreachable_code })
      ~get_app_input:(fun ~input ~inject_outgoing:_ -> input)
      ~initial_input
      ~bind_to_element_with_id
      ~component
  ;;

  let start ~initial_input ~bind_to_element_with_id component =
    start_generic
      ~get_app_result:Fn.id
      ~get_app_input:App_input.create
      ~initial_input
      ~bind_to_element_with_id
      ~component
  ;;
end

module Proc = struct
  module Handle = struct
    include Arrow.Handle

    type ('extra, 'incoming) t = (unit, 'extra, 'incoming, Nothing.t) Arrow.Handle.t
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

  let start result_spec ~bind_to_element_with_id computation =
    let bonsai =
      Fn.const computation
      |> Bonsai.Arrow.map ~f:(Arrow.App_result.of_result_spec result_spec)
    in
    Arrow.start ~initial_input:() ~bind_to_element_with_id bonsai
  ;;
end
