open! Core
open! Async_kernel
open! Import
open Incr.Let_syntax
include To_incr_dom_intf

module State = struct
  type t = { mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t }

  let create () = { last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty }
end

module Action = struct
  type ('dynamic_action, 'static_action) t =
    | Dynamic of 'dynamic_action
    | Static of 'static_action
  [@@deriving sexp_of]
end

module Action_unshadowed = Action

let create_generic run ~fresh ~input ~model ~inject_dynamic ~inject_static ~apply_static =
  let environment =
    Bonsai.Private.Environment.(empty |> add_exn ~key:fresh ~data:input)
  in
  let snapshot =
    run
      ~environment
      ~path:Bonsai.Private.Path.empty
      ~clock:Incr.clock
      ~model
      ~inject_dynamic
      ~inject_static
  in
  let%map view, extra = Bonsai.Private.Snapshot.result snapshot
  and dynamic_apply_action =
    Bonsai.Private.Apply_action.to_incremental
      (Bonsai.Private.Snapshot.apply_action snapshot)
  and lifecycle = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot
  and model = model in
  let schedule_event = Vdom.Effect.Expert.handle_non_dom_event_exn in
  let apply_action action _state ~schedule_action:_ =
    match action with
    | Action.Dynamic action -> dynamic_apply_action model action ~schedule_event
    | Action.Static action ->
      apply_static ~inject_dynamic ~inject_static ~schedule_event model action
  in
  let on_display state ~schedule_action:_ =
    let diff =
      Bonsai.Private.Lifecycle.Collection.diff state.State.last_lifecycle lifecycle
    in
    state.State.last_lifecycle <- lifecycle;
    Vdom.Effect.Expert.handle_non_dom_event_exn diff
  in
  Incr_dom.Component.create_with_extra ~on_display ~extra ~apply_action model view
;;

let convert_generic
      (type input model dynamic_action static_action extra)
      ~fresh
      ~(run :
          ( model
          , dynamic_action
          , static_action
          , Vdom.Node.t * extra )
            Bonsai.Private.Computation.eval_fun)
      ~default_model
      ~(dynamic_action_type_id : dynamic_action Bonsai.Private.Meta.Action.t)
      ~(static_action_type_id : static_action Bonsai.Private.Meta.Action.t)
      ~apply_static
      ~equal_model
      ~sexp_of_model
      ~model_of_sexp
  : (module S with type Input.t = input and type Extra.t = extra)
  =
  (module struct
    module Input = struct
      type t = input
    end

    module Model = struct
      type t = model [@@deriving equal, sexp]

      let default = default_model
    end

    module Action = struct
      let sexp_of_dynamic_action =
        Bonsai.Private.Meta.Action.Type_id.to_sexp dynamic_action_type_id
      ;;

      let sexp_of_static_action =
        Bonsai.Private.Meta.Action.Type_id.to_sexp static_action_type_id
      ;;

      type t = (dynamic_action, static_action) Action.t [@@deriving sexp_of]
    end

    module Extra = struct
      type t = extra
    end

    module State = State

    type t = (Action.t, Model.t, State.t, Extra.t) Incr_dom.Component.with_extra

    let create ~input ~old_model:_ ~model ~inject =
      let inject_dynamic a = inject (Action_unshadowed.Dynamic a) in
      let inject_static a = inject (Action_unshadowed.Static a) in
      create_generic run ~fresh ~input ~model ~inject_dynamic ~inject_static ~apply_static
    ;;
  end)
;;

let convert_with_extra ?(optimize = false) component =
  let fresh = Type_equal.Id.create ~name:"" sexp_of_opaque in
  let var = Bonsai.Private.(Value.named App_input fresh |> conceal_value) in
  let maybe_optimize = if optimize then Bonsai.Private.pre_process else Fn.id in
  let (T { model; dynamic_action; static_action; apply_static; run }) =
    component var
    |> Bonsai.Private.reveal_computation
    |> maybe_optimize
    |> Bonsai.Private.gather
  in
  convert_generic
    ~run
    ~fresh
    ~dynamic_action_type_id:dynamic_action
    ~static_action_type_id:static_action
    ~apply_static
    ~default_model:model.default
    ~equal_model:model.equal
    ~sexp_of_model:model.sexp_of
    ~model_of_sexp:model.of_sexp
;;

let convert ?optimize component =
  convert_with_extra ?optimize (Bonsai.Arrow_deprecated.map component ~f:(fun r -> r, ()))
;;
