open! Core
open! Import

module Computation = struct
  type ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action =
    inject_dynamic:('dynamic_action -> unit Effect.t)
    -> inject_static:('static_action -> unit Effect.t)
    -> schedule_event:(unit Effect.t -> unit)
    -> 'input
    -> 'model
    -> 'dynamic_action
    -> 'model

  type ('dynamic_action, 'static_action, 'model) static_apply_action =
    inject_dynamic:('dynamic_action -> unit Effect.t)
    -> inject_static:('static_action -> unit Effect.t)
    -> schedule_event:(unit Effect.t -> unit)
    -> 'model
    -> 'static_action
    -> 'model

  type ('model, 'dynamic_action, 'static_action, 'result) eval_fun =
    environment:Environment.t
    -> path:Path.t
    -> clock:Incr.Clock.t
    -> model:'model Incr.t
    -> inject_dynamic:('dynamic_action -> unit Effect.t)
    -> inject_static:('static_action -> unit Effect.t)
    -> ('model, 'dynamic_action, 'result) Snapshot.t

  type ('dynamic_action, 'static_action, 'model) reset =
    inject_dynamic:('dynamic_action -> unit Effect.t)
    -> inject_static:('static_action -> unit Effect.t)
    -> schedule_event:(unit Effect.t -> unit)
    -> 'model
    -> 'model

  type ('model, 'dynamic_action, 'static_action, 'result) info =
    { model : 'model Meta.Model.t
    ; dynamic_action : 'dynamic_action Meta.Action.t
    ; static_action : 'static_action Meta.Action.t
    ; apply_static : ('dynamic_action, 'static_action, 'model) static_apply_action
    ; run : ('model, 'dynamic_action, 'static_action, 'result) eval_fun
    ; reset : ('dynamic_action, 'static_action, 'model) reset
    }

  type 'result packed_info = T : (_, _, _, 'result) info -> 'result packed_info

  type 'result kind =
    | Return : 'result Value.t -> 'result kind
    | Leaf01 :
        { model : 'model Meta.Model.t
        ; dynamic_action : 'dynamic_action Meta.Action.t
        ; static_action : 'static_action Meta.Action.t
        ; apply_dynamic :
            ('input, 'dynamic_action, 'static_action, 'model) dynamic_apply_action
        ; apply_static : ('dynamic_action, 'static_action, 'model) static_apply_action
        ; reset : ('dynamic_action, 'static_action, 'model) reset
        ; input : 'input Value.t
        }
        -> ('model
            * ('dynamic_action -> unit Effect.t)
            * ('static_action -> unit Effect.t))
             kind
    | Leaf1 :
        { model : 'model Meta.Model.t
        ; dynamic_action : 'dynamic_action Meta.Action.t
        ; apply_action : ('input, 'dynamic_action, Nothing.t, 'model) dynamic_apply_action
        ; reset : ('dynamic_action, Nothing.t, 'model) reset
        ; input : 'input Value.t
        }
        -> ('model * ('dynamic_action -> unit Effect.t)) kind
    | Leaf0 :
        { model : 'model Meta.Model.t
        ; static_action : 'static_action Meta.Action.t
        ; apply_action : (Nothing.t, 'static_action, 'model) static_apply_action
        ; reset : (Nothing.t, 'static_action, 'model) reset
        ; compute : inject:('static_action -> unit Effect.t) -> 'model -> 'result
        }
        -> 'result kind
    | Leaf_incr :
        { model : 'model Meta.Model.t
        ; dynamic_action : 'dynamic_action Meta.Action.t
        ; input : 'input Value.t
        ; reset : ('dynamic_action, Nothing.t, 'model) reset
        ; apply_dynamic :
            'input Incr.t
            -> inject:('dynamic_action -> unit Effect.t)
            -> (schedule_event:(unit Effect.t -> unit)
                -> 'model
                -> 'dynamic_action
                -> 'model)
                 Incr.t
        ; compute :
            Incr.Clock.t
            -> 'input Incr.t
            -> 'model Incr.t
            -> inject:('dynamic_action -> unit Effect.t)
            -> 'result Incr.t
        }
        -> 'result kind
    | Model_cutoff : 'result t -> 'result kind
    | Sub :
        { from : 'via t
        ; via : 'via Type_equal.Id.t
        ; into : 'result t
        ; here : Source_code_position.t option
        }
        -> 'result kind
    | Store :
        { id : 'x Type_equal.Id.t
        ; value : 'x Value.t
        ; inner : 'result t
        }
        -> 'result kind
    | Fetch :
        { id : 'a Type_equal.Id.t
        ; default : 'result
        ; for_some : 'a -> 'result
        }
        -> 'result kind
    | Assoc :
        { map : ('k, 'v, 'cmp) Map.t Value.t
        ; key_comparator : ('k, 'cmp) comparator
        ; key_id : 'k Type_equal.Id.t
        ; cmp_id : 'cmp Type_equal.Id.t
        ; data_id : 'v Type_equal.Id.t
        ; by : 'result t
        }
        -> ('k, 'result, 'cmp) Map.t kind
    | Assoc_on :
        { map : ('io_key, 'v, 'io_cmp) Map.t Value.t
        ; io_comparator : ('io_key, 'io_cmp) comparator
        ; model_comparator : ('model_key, 'model_cmp) comparator
        ; io_key_id : 'io_key Type_equal.Id.t
        ; model_key_id : 'model_key Type_equal.Id.t
        ; model_cmp_id : 'model_cmp Type_equal.Id.t
        ; data_id : 'v Type_equal.Id.t
        ; by : 'result t
        ; get_model_key : 'io_key -> 'v -> 'model_key
        }
        -> ('io_key, 'result, 'io_cmp) Map.t kind
    | Assoc_simpl :
        { map : ('k, 'v, 'cmp) Map.t Value.t
        ; by : Path.t -> 'k -> 'v -> 'result
        }
        -> ('k, 'result, 'cmp) Map.t kind
    | Switch :
        { match_ : int Value.t
        ; arms : 'result t Int.Map.t
        }
        -> 'result kind
    | Lazy : 'result t lazy_t -> 'result kind
    | Wrap :
        { wrapper_model : 'outer_model Meta.Model.t
        ; action_id : 'outer_dynamic_action Meta.Action.t
        ; inject_id : ('outer_dynamic_action -> unit Effect.t) Type_equal.Id.t
        ; model_id : 'outer_model Type_equal.Id.t
        ; inner : 'result t
        ; dynamic_apply_action :
            ('result, 'outer_dynamic_action, Nothing.t, 'outer_model) dynamic_apply_action
        ; reset : ('outer_dynamic_action, Nothing.t, 'outer_model) reset
        }
        -> 'result kind
    | With_model_resetter :
        { reset_id : unit Effect.t Type_equal.Id.t
        ; inner : 'result t
        }
        -> 'result kind
    | Path : Path.t kind
    | Lifecycle : Lifecycle.t option Value.t -> unit kind
    | Identity : 'result t -> 'result kind

  and 'result t =
    { kind : 'result kind
    ; id : 'result Type_equal.Id.t
    ; info : 'result packed_info lazy_t
    }
end

module type Gather_impl = sig
  val gather : 'result Computation.t -> 'result Computation.packed_info

  val wrap_computation
    :  pack:
         ('result Computation.kind
          -> 'result Computation.packed_info lazy_t
          -> 'result Computation.t)
    -> 'result Computation.kind
    -> 'result Computation.t
end

module Proc_min (G : Gather_impl) = struct
  let wrap_computation kind =
    G.wrap_computation kind ~pack:(fun kind info ->
      let id = Type_equal.Id.create ~name:"computation result" [%sexp_of: opaque] in
      { kind; id; info })
  ;;

  let gather = G.gather
  let read x = wrap_computation (Return x)

  let sub (type via) ?here (from : via Computation.t) ~f =
    match from.kind with
    | Return { here = there; value = Named _ as named; id } ->
      let here = Option.first_some here there in
      f { Value.here; value = named; id }
    | _ ->
      let via : via Type_equal.Id.t =
        Type_equal.Id.create
          ~name:(Source_code_position.to_string [%here])
          [%sexp_of: opaque]
      in
      let into = f (Value.named (Sub here) via) in
      Sub { from; via; into; here } |> wrap_computation
  ;;

  let switch ~match_ ~branches ~with_ =
    let arms =
      Int.Map.of_increasing_sequence
        (Sequence.map (Sequence.range 0 branches) ~f:(fun key ->
           let computation =
             try with_ key with
             | exn -> read (Value.return_exn exn)
           in
           key, computation))
      |> Or_error.ok_exn
    in
    Switch { match_; arms } |> wrap_computation
  ;;

  let reset_to_default
        ~default_model
        ~inject_dynamic:_
        ~inject_static:_
        ~schedule_event:_
        _prev_model
    =
    default_model
  ;;

  let build_resetter reset ~default_model ~f =
    let ignore_absurd : (Nothing.t -> 'a) -> unit = ignore in
    Option.value_map reset ~default:(reset_to_default ~default_model) ~f:(fun a ->
      f ~ignore_absurd a)
  ;;

  module Proc_incr = struct
    let value_cutoff t ~equal = read (Value.cutoff ~added_by_let_syntax:false ~equal t)
    let model_cutoff t = Model_cutoff t |> wrap_computation

    let compute_with_clock t ~f =
      let dynamic_apply_action _input ~inject:_ =
        Incr.return (fun ~schedule_event:_ _model -> Nothing.unreachable_code)
      in
      let compute clock input _model ~inject:_ = f clock input in
      Computation.Leaf_incr
        { model = Meta.Model.unit
        ; dynamic_action = Meta.Action.nothing
        ; input = t
        ; apply_dynamic = dynamic_apply_action
        ; reset = reset_unit_model
        ; compute
        }
      |> wrap_computation
    ;;

    let of_module
          (type input model result)
          (module M : Component_s_incr
            with type Input.t = input
             and type Model.t = model
             and type Result.t = result)
          ~(default_model : model)
          (input : input Value.t)
      : result Computation.t
      =
      Leaf_incr
        { model =
            Meta.Model.of_module (module M.Model) ~name:M.name ~default:default_model
        ; dynamic_action = Meta.Action.of_module (module M.Action) ~name:M.name
        ; input
        ; apply_dynamic = M.apply_action
        ; reset = reset_to_default ~default_model
        ; compute = (fun _ -> M.compute)
        }
      |> wrap_computation
    ;;
  end

  module Dynamic_scope = struct
    let fetch ~id ~default ~for_some = Fetch { id; default; for_some } |> wrap_computation
    let store ~id ~value ~inner = Store { id; value; inner } |> wrap_computation
  end

  module Edge = struct
    let lifecycle t = wrap_computation (Lifecycle t)
  end

  let state_machine01
        model
        dynamic_action
        static_action
        ?reset
        ~default_model
        ~apply_dynamic
        ~apply_static
        input
    =
    let reset = Option.value reset ~default:(reset_to_default ~default_model) in
    let name = Source_code_position.to_string [%here] in
    Leaf01
      { model = Meta.Model.of_module model ~name ~default:default_model
      ; dynamic_action = Meta.Action.of_module dynamic_action ~name
      ; static_action = Meta.Action.of_module static_action ~name
      ; apply_dynamic
      ; apply_static
      ; input
      ; reset
      }
    |> wrap_computation
  ;;

  let state_machine1 model dynamic_action ?reset ~default_model ~apply_action input =
    let name = Source_code_position.to_string [%here] in
    let reset =
      build_resetter
        reset
        ~default_model
        ~f:(fun ~ignore_absurd reset ~inject_dynamic ~inject_static ->
          ignore_absurd inject_static;
          reset ~inject:inject_dynamic)
    in
    let apply_action ~inject_dynamic ~inject_static:_ =
      apply_action ~inject:inject_dynamic
    in
    Leaf1
      { model = Meta.Model.of_module model ~name ~default:default_model
      ; dynamic_action = Meta.Action.of_module dynamic_action ~name
      ; apply_action
      ; reset
      ; input
      }
    |> wrap_computation
  ;;

  let state_machine0 ?reset model static_action ~default_model ~apply_action =
    let name = Source_code_position.to_string [%here] in
    let apply_action ~inject_dynamic:_ ~inject_static =
      apply_action ~inject:inject_static
    in
    let reset =
      build_resetter
        reset
        ~default_model
        ~f:(fun ~ignore_absurd reset ~inject_dynamic ~inject_static ->
          ignore_absurd inject_dynamic;
          reset ~inject:inject_static)
    in
    Leaf0
      { model = Meta.Model.of_module model ~name ~default:default_model
      ; static_action = Meta.Action.of_module static_action ~name
      ; apply_action
      ; reset
      ; compute = (fun ~inject model -> model, inject)
      }
    |> wrap_computation
  ;;

  let assoc
        (type k v cmp)
        (comparator : (k, cmp) comparator)
        (map : (k, v, cmp) Map.t Value.t)
        ~f
    =
    let module C = (val comparator) in
    let key_id : k Type_equal.Id.t = Type_equal.Id.create ~name:"key id" C.sexp_of_t in
    let cmp_id : cmp Type_equal.Id.t =
      Type_equal.Id.create ~name:"cmp id" [%sexp_of: opaque]
    in
    let data_id : v Type_equal.Id.t =
      Type_equal.Id.create ~name:"data id" [%sexp_of: opaque]
    in
    let key_var = Value.named Assoc_like_key key_id in
    let data_var = Value.named Assoc_like_data data_id in
    let by = f key_var data_var in
    Assoc { map; key_comparator = comparator; key_id; cmp_id; data_id; by }
    |> wrap_computation
  ;;

  let assoc_on
        (type model_k io_k model_cmp io_cmp v)
        (io_comparator : (io_k, io_cmp) comparator)
        (model_comparator : (model_k, model_cmp) comparator)
        (map : (io_k, v, io_cmp) Map.t Value.t)
        ~get_model_key
        ~f
    =
    let module Io_comparator = (val io_comparator) in
    let module Model_comparator = (val model_comparator) in
    let io_key_id : io_k Type_equal.Id.t =
      Type_equal.Id.create ~name:"io key id" Io_comparator.sexp_of_t
    in
    let model_key_id : model_k Type_equal.Id.t =
      Type_equal.Id.create ~name:"model key id" Model_comparator.sexp_of_t
    in
    let model_cmp_id : model_cmp Type_equal.Id.t =
      Type_equal.Id.create ~name:"model key id" [%sexp_of: opaque]
    in
    let data_id : v Type_equal.Id.t =
      Type_equal.Id.create ~name:"data id" [%sexp_of: opaque]
    in
    let key_var = Value.named Assoc_like_key io_key_id in
    let data_var = Value.named Assoc_like_data data_id in
    let by = f key_var data_var in
    Assoc_on
      { map
      ; io_comparator
      ; model_comparator
      ; io_key_id
      ; data_id
      ; model_key_id
      ; model_cmp_id
      ; by
      ; get_model_key
      }
    |> wrap_computation
  ;;

  let lazy_ t = Lazy t |> wrap_computation

  let wrap (type model action) ?reset model_module ~default_model ~apply_action ~f =
    let apply_action ~inject_static:_ = apply_action in
    let model_id : model Type_equal.Id.t =
      Type_equal.Id.create ~name:"model id" [%sexp_of: opaque]
    in
    let module M = struct
      type t = action

      let sexp_of_t = [%sexp_of: opaque]
    end
    in
    let reset =
      build_resetter
        reset
        ~default_model
        ~f:(fun ~ignore_absurd reset ~inject_dynamic ~inject_static ->
          ignore_absurd inject_static;
          reset ~inject:inject_dynamic)
    in
    let action_id = Meta.Action.of_module (module M) ~name:"action id" in
    let inject_id : (action -> unit Effect.t) Type_equal.Id.t =
      Type_equal.Id.create ~name:"inject id" [%sexp_of: opaque]
    in
    let apply_action ~inject_dynamic = apply_action ~inject:inject_dynamic in
    let model_var = Value.named Wrap_model model_id in
    let inject_var = Value.named Wrap_inject inject_id in
    let inner = f model_var inject_var in
    let wrapper_model =
      Meta.Model.of_module
        model_module
        ~default:default_model
        ~name:"outer model for wrap"
    in
    Wrap
      { wrapper_model
      ; action_id
      ; inject_id
      ; model_id
      ; inner
      ; dynamic_apply_action = apply_action
      ; reset
      }
    |> wrap_computation
  ;;

  let with_model_resetter f =
    let reset_id = Type_equal.Id.create ~name:"reset-model" [%sexp_of: opaque] in
    let inner = f ~reset:(Value.named Model_resetter reset_id) in
    With_model_resetter { reset_id; inner } |> wrap_computation
  ;;

  let path = Path |> wrap_computation
end

include Computation
