open! Core
open! Async_kernel
open! Import
open Incr.Let_syntax
include To_incr_dom_intf
module Bonsai_action = Bonsai.Private.Action

module State = struct
  type t = { mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t }

  let create () = { last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty }
end

let create_generic run ~fresh ~input ~model ~inject ~apply_action =
  let environment =
    Bonsai.Private.Environment.(empty |> add_exn ~key:fresh ~data:input)
  in
  let snapshot, () =
    run
      ~environment
      ~path:Bonsai.Private.Path.empty
      ~clock:Incr_dom.Start_app.Private.time_source
      ~model
      ~inject
    |> Bonsai.Private.Trampoline.run
  in
  let%map view, extra = Bonsai.Private.Snapshot.result snapshot
  and input = Bonsai.Private.Input.to_incremental (Bonsai.Private.Snapshot.input snapshot)
  and lifecycle = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot
  and model = model in
  let schedule_event = Vdom.Effect.Expert.handle_non_dom_event_exn in
  let apply_action action _state ~schedule_action:_ =
    apply_action ~inject ~schedule_event (Some input) model action
  in
  let on_display state ~schedule_action:_ =
    let diff =
      Bonsai.Private.Lifecycle.Collection.diff state.State.last_lifecycle lifecycle
    in
    state.State.last_lifecycle <- lifecycle;
    Vdom.Effect.Expert.handle_non_dom_event_exn diff;
    Bonsai.Time_source.Private.trigger_after_display
      Incr_dom.Start_app.Private.time_source
  in
  Incr_dom.Component.create_with_extra ~on_display ~extra ~apply_action model view
;;

let convert_generic
  (type input action_input model action extra)
  ~fresh
  ~(run :
      ( model
      , action
      , action_input
      , Vdom.Node.t * extra
      , unit )
      Bonsai.Private.Computation.eval_fun)
  ~default_model
  ~(action_type_id : action Bonsai_action.id)
  ~apply_action
  ~equal_model
  ~sexp_of_model
  : (module S with type Input.t = input and type Extra.t = extra)
  =
  (module struct
    module Input = struct
      type t = input
    end

    module Model = struct
      type t = model [@@deriving equal, sexp_of]

      let default = default_model
    end

    module Action = struct
      type t = action Bonsai_action.t

      let sexp_of_t = Bonsai_action.Type_id.to_sexp action_type_id
    end

    module Extra = struct
      type t = extra
    end

    module State = State

    type t = (Action.t, Model.t, State.t, Extra.t) Incr_dom.Component.with_extra

    let create ~input ~old_model:_ ~model ~inject =
      create_generic run ~fresh ~input ~model ~inject ~apply_action
    ;;
  end)
;;

let convert_with_extra ?(optimize = false) component =
  let fresh = Type_equal.Id.create ~name:"" sexp_of_opaque in
  let var = Bonsai.Private.(Value.named App_input fresh |> conceal_value) in
  let maybe_optimize = if optimize then Bonsai.Private.pre_process else Fn.id in
  let (T { model; input = _; action; apply_action; run; reset = _; can_contain_path = _ })
    =
    component var
    |> Bonsai.Private.top_level_handle
    |> maybe_optimize
    |> Bonsai.Private.gather
  in
  convert_generic
    ~run
    ~fresh
    ~action_type_id:action
    ~apply_action
    ~default_model:model.default
    ~equal_model:model.equal
    ~sexp_of_model:model.sexp_of
;;

let convert ?optimize component =
  convert_with_extra ?optimize (Bonsai.Arrow_deprecated.map component ~f:(fun r -> r, ()))
;;
