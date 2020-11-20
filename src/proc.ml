open! Core_kernel
open! Import
module Var = Var

let sub
      (type via)
      (Computation.T { t = from; action = from_action; model = from_model } :
         via Computation.packed)
      ~f
  =
  let via : via Type_equal.Id.t =
    Type_equal.Id.create ~name:(Source_code_position.to_string [%here]) [%sexp_of: opaque]
  in
  let (Computation.T { t = into; action = into_action; model = into_model }) =
    f (Value.named via)
  in
  Computation.T
    { t = Subst { from; via; into }
    ; action = Meta.Action.both from_action into_action
    ; model = Meta.Model.both from_model into_model
    }
;;

let read x =
  Computation.T { t = Return x; model = Meta.Model.unit; action = Meta.Action.nothing }
;;

let switch ~match_ ~branches ~with_ =
  let create_case key =
    let component = with_ key in
    let (Computation.T { model; t = _; action = _ }) = component in
    let default_model = Hidden.Model.create model model.default in
    component, default_model
  in
  let components, models =
    List.fold
      (List.range 0 branches)
      ~init:(Int.Map.empty, Int.Map.empty)
      ~f:(fun (components, models) key ->
        let component, model = create_case key in
        let components = Map.add_exn components ~key ~data:component in
        let models = Map.add_exn models ~key ~data:model in
        components, models)
  in
  let key_type_id = Type_equal.Id.create ~name:"key" Int.sexp_of_t in
  Computation.T
    { t =
        Enum
          { which = match_
          ; out_of = components
          ; sexp_of_key = [%sexp_of: int]
          ; key_equal = [%equal: int]
          ; key_type_id
          ; key_compare = [%compare: int]
          ; key_and_cmp = T
          }
    ; model = Hidden.Multi_model.model_info (module Int) models
    ; action = Hidden.Action.type_id [%sexp_of: int]
    }
;;

module Let_syntax = struct
  let return = read
  let ( <*> ) = Value.( <*> )
  let ( <$> ) f = Value.map ~f
  let ( >>| ) a f = Value.map a ~f

  module Let_syntax = struct
    let sub = sub
    let switch = switch
    let return = return
    let map = Value.map
    let both = Value.both

    include (Value : Mapn with type 'a t := 'a Value.t)
  end
end

let pure f i = read (Value.map i ~f)
let const x = read (Value.return x)

let of_module1 (type i m a r) (component : (i, m, a, r) component_s) ~default_model input =
  let (module M) = component in
  Computation.T
    { t =
        Leaf
          { input
          ; apply_action = M.apply_action
          ; compute = M.compute
          ; name = M.name
          ; kind = "of module"
          }
    ; model = Meta.Model.of_module (module M.Model) ~name:M.name ~default:default_model
    ; action = Meta.Action.of_module (module M.Action) ~name:M.name
    }
;;

let of_module0 c ~default_model = of_module1 c ~default_model (Value.return ())
let of_module2 c ~default_model i1 i2 = of_module1 c ~default_model (Value.both i1 i2)

let with_model_resetter (Computation.T { t; model; action }) =
  let action = Meta.(Action.both unit_type_id action) in
  Computation.T
    { t = With_model_resetter { t; default_model = model.default }; model; action }
;;

let assoc
      (type k v cmp)
      (comparator : (k, cmp) comparator)
      (map : (k, v, cmp) Map.t Value.t)
      ~f
  =
  let key_id : k Type_equal.Id.t =
    Type_equal.Id.create ~name:"key id" [%sexp_of: opaque]
  in
  let data_id : v Type_equal.Id.t =
    Type_equal.Id.create ~name:"data id" [%sexp_of: opaque]
  in
  let key_var = Value.named key_id in
  let data_var = Value.named data_id in
  let (Computation.T { t = by; action; model }) = f key_var data_var in
  match Simplify.function_of_'return'_computation by ~key_id ~data_id with
  | Some by ->
    Computation.T
      { t =
          Assoc_simpl
            { map
            ; key_id
            ; data_id
            ; by
            ; model_info = model
            ; input_by_k = T
            ; result_by_k = T
            ; model_by_k = T
            }
      ; action = Meta.Action.nothing
      ; model = Meta.Model.unit
      }
  | None ->
    let module Cmp = (val comparator) in
    Computation.T
      { t =
          Assoc
            { map
            ; key_compare = Cmp.comparator.compare
            ; key_id
            ; data_id
            ; by
            ; model_info = model
            ; action_info = action
            ; input_by_k = T
            ; result_by_k = T
            ; model_by_k = T
            }
      ; action = Meta.Action.map comparator action
      ; model = Meta.Model.map comparator model
      }
;;

let enum (type k) (module E : Enum with type t = k) ~match_ ~with_ =
  let module E = struct
    include E
    include Comparator.Make (E)
  end
  in
  let create_case key =
    let component = with_ key in
    let (Computation.T { model; t = _; action = _ }) = component in
    let default_model = Hidden.Model.create model model.default in
    component, default_model
  in
  let components, models =
    List.fold
      E.all
      ~init:(Map.empty (module E), Map.empty (module E))
      ~f:(fun (components, models) key ->
        let component, model = create_case key in
        let components = Map.add_exn components ~key ~data:component in
        let models = Map.add_exn models ~key ~data:model in
        components, models)
  in
  let key_type_id = Type_equal.Id.create ~name:"key" E.sexp_of_t in
  Computation.T
    { t =
        Enum
          { which = match_
          ; out_of = components
          ; sexp_of_key = [%sexp_of: E.t]
          ; key_equal = [%equal: E.t]
          ; key_type_id
          ; key_compare = [%compare: E.t]
          ; key_and_cmp = T
          }
    ; model = Hidden.Multi_model.model_info (module E) models
    ; action = Hidden.Action.type_id [%sexp_of: E.t]
    }
;;

let if_ cond ~then_ ~else_ =
  enum
    (module Bool)
    ~match_:cond
    ~with_:(function
      | true -> then_
      | false -> else_)
;;

let match_either value ~first ~second =
  let is_left = Value.map value ~f:Either.is_first in
  if_
    is_left
    ~then_:
      (first
         (Value.map value ~f:(function
            | First f -> f
            | Second _ -> assert false)))
    ~else_:
      (second
         (Value.map value ~f:(function
            | Second f -> f
            | First _ -> assert false)))
;;

let match_result value ~ok ~err =
  match_either (Value.map value ~f:Result.to_either) ~first:ok ~second:err
;;

let option_to_either = function
  | Some a -> Either.First a
  | None -> Either.Second ()
;;

let map_option value ~f =
  let first x = sub (f x) ~f:(fun res -> res |> Value.map ~f:Option.some |> read) in
  match_either
    (Value.map value ~f:option_to_either)
    ~first
    ~second:(fun (_ : unit Value.t) -> const None)
;;

let match_option value ~some ~none =
  match_either
    (Value.map value ~f:option_to_either)
    ~first:some
    ~second:(fun (_ : unit Value.t) -> none)
;;

let state_machine1
      (type m a)
      here
      (module M : Model with type t = m)
      (module A : Action with type t = a)
      ~default_model
      ~apply_action
      input
  =
  let compute ~inject _input model = model, inject in
  let name = Source_code_position.to_string here in
  Computation.T
    { t = Leaf { input; apply_action; compute; name; kind = "state machine" }
    ; model = Meta.Model.of_module (module M) ~name ~default:default_model
    ; action = Meta.Action.of_module (module A) ~name
    }
;;

let state_machine0 here model action ~default_model ~apply_action =
  let apply_action ~inject ~schedule_event () model action =
    apply_action ~inject ~schedule_event model action
  in
  state_machine1 here model action ~default_model ~apply_action (Value.return ())
;;

let actor1
  : type input model action return.
    Source_code_position.t
    -> (module Model with type t = model)
    -> (module Action with type t = action)
    -> default_model:model
    -> recv:
         (schedule_event:(Ui_event.t -> unit)
          -> input
          -> model
          -> action
          -> model * return)
    -> input Value.t
    -> (model * (action -> return Effect.t)) Computation.packed
  =
  fun here
    (module M : Model with type t = model)
    (module A : Action with type t = action)
    ~default_model
    ~recv
    input ->
    let open Let_syntax in
    let module Action_with_callback = struct
      type t = (action, return) Effect.Private.Callback.t

      let sexp_of_t cb = A.sexp_of_t (Effect.Private.Callback.request cb)
    end
    in
    let%sub model_and_inject =
      state_machine1
        here
        (module M)
        (module Action_with_callback)
        ~default_model
        ~apply_action:(fun ~inject:_ ~schedule_event input model callback ->
          let action = Effect.Private.Callback.request callback in
          let new_model, response = recv ~schedule_event input model action in
          schedule_event (Effect.Private.Callback.respond_to callback response);
          new_model)
        input
    in
    return
    @@ let%map model, inject = model_and_inject in
    let inject action = Effect.Private.make ~request:action ~evaluator:inject in
    model, inject
;;

let actor0 here model action ~default_model ~recv =
  let recv ~schedule_event _ = recv ~schedule_event in
  actor1 here model action ~default_model ~recv (Value.return ())
;;

let lazy_ t =
  let open struct
    type model = Hidden.Model.t option [@@deriving equal, sexp_of]
  end in
  let action = Hidden.Action.type_id [%sexp_of: unit] in
  let model =
    { Meta.Model.default = None
    ; equal = equal_model
    ; type_id = Type_equal.Id.create ~name:"lazy-model" [%sexp_of: model]
    ; sexp_of = [%sexp_of: model]
    ; of_sexp = (fun _ -> None)
    }
  in
  Computation.T { t = Lazy t; action; model }
;;

let wrap (type model action) model_module ~default_model ~apply_action ~f =
  let model_id : model Type_equal.Id.t =
    Type_equal.Id.create ~name:"model id" [%sexp_of: opaque]
  in
  let action_id : action Type_equal.Id.t =
    Type_equal.Id.create ~name:"action id" [%sexp_of: opaque]
  in
  let inject_id : (action -> Event.t) Type_equal.Id.t =
    Type_equal.Id.create ~name:"inject id" [%sexp_of: opaque]
  in
  let model_var = Value.named model_id in
  let inject_var = Value.named inject_id in
  let (Computation.T { t = inner; model = inner_model; action = inner_action }) =
    f model_var inject_var
  in
  let action = Meta.Action.both action_id inner_action in
  let model =
    Meta.Model.both
      (Meta.Model.of_module
         model_module
         ~default:default_model
         ~name:"outer model for wrap")
      inner_model
  in
  Computation.T
    { t = Computation.Wrap { model_id; inject_id; inner; apply_action }; action; model }
;;

let state (type m) here (module M : Model with type t = m) ~default_model =
  let apply_action ~inject:_ ~schedule_event:_ () _old_model new_model = new_model in
  let compute ~inject _input model = model, inject in
  let name = Source_code_position.to_string here in
  let input = Value.return () in
  Computation.T
    { t = Leaf { input; apply_action; compute; name; kind = "state" }
    ; model = Meta.Model.of_module (module M) ~name ~default:default_model
    ; action = Meta.Action.of_module (module M) ~name
    }
;;

let state_opt (type m) here ?default_model (module M : Model with type t = m) =
  state
    here
    ~default_model
    (module struct
      type t = M.t option [@@deriving equal, sexp]
    end)
;;

module Edge = struct
  let lifecycle' ?on_activate ?on_deactivate ?after_display () =
    let open Let_syntax in
    let transpose_join : 'a option Value.t option -> 'a option Value.t = function
      | Some a -> a
      | None -> Value.return None
    in
    let tripple =
      let%map a = transpose_join on_activate
      and b = transpose_join on_deactivate
      and c = transpose_join after_display in
      a, b, c
    in
    let t =
      match%map tripple with
      | None, None, None -> None
      | on_activate, on_deactivate, after_display ->
        Some { Lifecycle.on_activate; on_deactivate; after_display }
    in
    Computation.T
      { t = Lifecycle t; model = Meta.Model.unit; action = Meta.Action.nothing }
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

  let on_change' (type a) here (module M : Model with type t = a) input ~callback =
    let open Let_syntax in
    let%sub state, set_state = state_opt here (module M) in
    let%sub update =
      match%sub state with
      | None ->
        return
        @@ let%map set_state = set_state
        and input = input
        and callback = callback in
        Some (Ui_event.Many [ set_state (Some input); callback None input ])
      | Some state ->
        return
        @@ let%map state = state
        and set_state = set_state
        and input = input
        and callback = callback in
        if M.equal state input
        then None
        else
          Some (Ui_event.Many [ set_state (Some input); callback (Some state) input ])
    in
    after_display' update
  ;;

  let on_change here model input ~callback =
    let callback = Value.map callback ~f:(fun callback _prev value -> callback value) in
    on_change' here model input ~callback
  ;;
end

module Incr = struct
  let value_cutoff t ~equal = read (Value.cutoff ~equal t)

  let model_cutoff (Computation.T { t; action; model }) =
    Computation.T { t = Computation.Model_cutoff { t; model }; action; model }
  ;;

  let compute t ~f =
    let apply_action _input model ~inject:_ =
      Incr.map model ~f:(fun _model ~schedule_event:_ -> Nothing.unreachable_code)
    in
    let compute input _model ~inject:_ = f input in
    Computation.T
      { t =
          Computation.Leaf_incr
            { name = "incr-compute"; input = t; apply_action; compute }
      ; action = Meta.Action.nothing
      ; model = Meta.Model.unit
      }
  ;;

  let to_value incr = Value.Incr incr
end

module Computation = struct
  type 'a t = 'a Computation.packed
end

module Value = Value

module Private = struct
  let conceal_value = Fn.id
  let reveal_value = Fn.id
  let conceal_computation = Fn.id
  let reveal_computation = Fn.id
end
