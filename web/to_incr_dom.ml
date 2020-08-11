open! Core_kernel
open! Async_kernel
open! Import
open Incr.Let_syntax
include To_incr_dom_intf

let create_generic computation ~fresh ~input ~model ~inject =
  let environment =
    Bonsai.Private.Environment.(empty |> add_exn ~key:fresh ~data:input)
  in
  let%map snapshot = Bonsai.Private.eval environment model ~inject computation
  and model = model in
  let apply_action incoming_action _state ~schedule_action:_ =
    let schedule_event = Vdom.Event.Expert.handle_non_dom_event_exn in
    Bonsai.Private.Snapshot.apply_action snapshot ~schedule_event incoming_action
  in
  let view, extra = Bonsai.Private.Snapshot.result snapshot in
  Incr_dom.Component.create_with_extra ~extra ~apply_action model view
;;

let convert_generic
      (type input model action extra)
      ~fresh
      ~(computation : (model, action, Vdom.Node.t * extra) Bonsai.Private.Computation.t)
      ~default_model
      ~(action_type_id : action Type_equal.Id.t)
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
      type t = action

      let sexp_of_t = Type_equal.Id.to_sexp action_type_id
    end

    module Extra = struct
      type t = extra
    end

    type t = (Action.t, Model.t, unit, Extra.t) Incr_dom.Component.with_extra

    let create ~input ~old_model:_ ~model ~inject =
      create_generic computation ~fresh ~input ~model ~inject
    ;;
  end)
;;

let convert_with_extra component =
  let fresh = Type_equal.Id.create ~name:"" sexp_of_opaque in
  let var = Bonsai.Private.(Value.named fresh |> conceal_value) in
  let component = component var |> Bonsai.Private.reveal_computation in
  let (Bonsai.Private.Computation.T { t; model; action }) = component in
  convert_generic
    ~computation:t
    ~fresh
    ~action_type_id:action
    ~default_model:model.default
    ~equal_model:model.equal
    ~sexp_of_model:model.sexp_of
    ~model_of_sexp:model.of_sexp
;;

let convert component =
  convert_with_extra (Bonsai.Arrow.map component ~f:(fun r -> r, ()))
;;
