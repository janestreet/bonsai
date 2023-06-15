open! Core
open! Import
open Computation

let read x = Return x

let sub (type via) ?here (from : via Computation.t) ~f =
  match from with
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
    Sub { from; via; into; here }
;;

let switch ~here ~match_ ~branches ~with_ =
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
  Switch { match_; arms; here }
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

module Dynamic_scope = struct
  let fetch ~id ~default ~for_some = Fetch { id; default; for_some }
  let store ~id ~value ~inner = Store { id; value; inner }
end

module Edge = struct
  let lifecycle t = Lifecycle t
end

let state_machine01
      ?(sexp_of_dynamic_action = sexp_of_opaque)
      ?(sexp_of_static_action = sexp_of_opaque)
      ?reset
      ?sexp_of_model
      ?equal
      ~default_model
      ~apply_dynamic
      ~apply_static
      input
  =
  let reset = Option.value reset ~default:(reset_to_default ~default_model) in
  let name = Source_code_position.to_string [%here] in
  let apply_dynamic ~inject_dynamic ~inject_static ~schedule_event input model action =
    match input with
    | Some input ->
      apply_dynamic ~inject_dynamic ~inject_static ~schedule_event input model action
    | None ->
      let action = sexp_of_dynamic_action action in
      eprint_s
        [%message
          "An action sent to a [state_machine01] has been dropped because its input was \
           not present. This happens when the [state_machine01] is inactive when it \
           receives a message."
            (action : Sexp.t)];
      model
  in
  Leaf01
    { model =
        Meta.Model.of_module
          ~sexp_of_model:(Option.value sexp_of_model ~default:sexp_of_opaque)
          ~equal
          ~name
          ~default:default_model
    ; input_id = Meta.Input.create ()
    ; dynamic_action = Meta.Action.of_module ~sexp_of_action:sexp_of_dynamic_action ~name
    ; static_action = Meta.Action.of_module ~sexp_of_action:sexp_of_static_action ~name
    ; apply_dynamic
    ; apply_static
    ; input
    ; reset
    }
;;

let state_machine1_safe
      ?(sexp_of_action = sexp_of_opaque)
      ~sexp_of_model
      ?reset
      ~equal
      ~default_model
      ~apply_action
      input
  =
  let name = Source_code_position.to_string [%here] in
  let reset =
    build_resetter
      reset
      ~default_model
      ~f:(fun ~ignore_absurd reset ~inject_dynamic ~inject_static ~schedule_event ->
        ignore_absurd inject_static;
        reset (Apply_action_context.create ~inject:inject_dynamic ~schedule_event))
  in
  let apply_action ~inject_dynamic ~inject_static:_ ~schedule_event =
    apply_action (Apply_action_context.create ~inject:inject_dynamic ~schedule_event)
  in
  Leaf1
    { model = Meta.Model.of_module ~sexp_of_model ~equal ~name ~default:default_model
    ; input_id = Meta.Input.create ()
    ; dynamic_action = Meta.Action.of_module ~sexp_of_action ~name
    ; apply_action
    ; reset
    ; input
    }
;;

module Computation_status = struct
  type 'input t =
    | Active of 'input
    | Inactive
  [@@deriving sexp_of]

  let of_option = function
    | Some x -> Active x
    | None -> Inactive
  ;;
end

let state_machine1
      ?sexp_of_action
      ?reset
      ?sexp_of_model
      ?equal
      ~default_model
      ~apply_action
      input
  =
  let apply_action context input model action =
    let input = Computation_status.of_option input in
    apply_action context input model action
  in
  state_machine1_safe
    ?sexp_of_action
    ~sexp_of_model:(Option.value sexp_of_model ~default:sexp_of_opaque)
    ?reset
    ~equal
    ~default_model
    ~apply_action
    input
;;

let state_machine0
      ?reset
      ?sexp_of_model
      ?(sexp_of_action = sexp_of_opaque)
      ?equal
      ~default_model
      ~apply_action
      ()
  =
  let name = Source_code_position.to_string [%here] in
  let apply_action ~inject_dynamic:_ ~inject_static ~schedule_event =
    apply_action (Apply_action_context.create ~inject:inject_static ~schedule_event)
  in
  let reset =
    build_resetter
      reset
      ~default_model
      ~f:(fun ~ignore_absurd reset ~inject_dynamic ~inject_static ~schedule_event ->
        ignore_absurd inject_dynamic;
        reset (Apply_action_context.create ~inject:inject_static ~schedule_event))
  in
  Leaf0
    { model =
        Meta.Model.of_module
          ~sexp_of_model:(Option.value ~default:sexp_of_opaque sexp_of_model)
          ~equal
          ~name
          ~default:default_model
    ; static_action = Meta.Action.of_module ~sexp_of_action ~name
    ; apply_action
    ; reset
    }
;;

module Proc_incr = struct
  let value_cutoff t ~equal = read (Value.cutoff ~added_by_let_syntax:false ~equal t)

  let compute_with_clock t ~f =
    Computation.Leaf_incr { input = t; input_id = Meta.Input.create (); compute = f }
  ;;

  let of_module
        (type input model result)
        (module M : Component_s_incr
          with type Input.t = input
           and type Model.t = model
           and type Result.t = result)
        ?sexp_of_model
        ~equal
        ~(default_model : model)
        (input : input Value.t)
    : result Computation.t
    =
    sub
      (state_machine1
         ~sexp_of_action:M.Action.sexp_of_t
         ?sexp_of_model
         ~equal
         ~default_model
         ~apply_action:(fun context input model action ->
           let%tydi { inject; schedule_event } =
             Apply_action_context.Private.reveal context
           in
           match input with
           | Active input -> M.apply_action input ~inject ~schedule_event model action
           | Inactive ->
             eprint_s
               [%message
                 [%here]
                   "An action sent to an [of_module] has been dropped because its input \
                    was not present. This happens when the [of_module] is inactive when \
                    it receives a message."
                   (action : M.Action.t)];
             model)
         input)
      ~f:(fun state ->
        compute_with_clock (Value.both input state) ~f:(fun _clock input_and_state ->
          let%pattern_bind.Ui_incr input, (model, inject) = input_and_state in
          M.compute input model ~inject))
  ;;
end

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
  let io_cmp_id : io_cmp Type_equal.Id.t =
    Type_equal.Id.create ~name:"io cmp id" [%sexp_of: opaque]
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
    ; io_cmp_id
    ; data_id
    ; model_key_id
    ; model_cmp_id
    ; by
    ; get_model_key
    }
;;

let lazy_ t = Lazy t

let wrap
      (type model action)
      ?reset
      ?sexp_of_model
      ?equal
      ~default_model
      ~apply_action
      ~f
      ()
  =
  let model_id : model Type_equal.Id.t =
    Type_equal.Id.create ~name:"model id" [%sexp_of: opaque]
  in
  let reset =
    build_resetter
      reset
      ~default_model
      ~f:(fun ~ignore_absurd reset ~inject_dynamic ~inject_static ~schedule_event ->
        ignore_absurd inject_static;
        reset (Apply_action_context.create ~inject:inject_dynamic ~schedule_event))
  in
  let action_id =
    Meta.Action.of_module ~sexp_of_action:sexp_of_opaque ~name:"action id"
  in
  let result_id = Meta.Input.create () in
  let inject_id : (action -> unit Effect.t) Type_equal.Id.t =
    Type_equal.Id.create ~name:"inject id" [%sexp_of: opaque]
  in
  let apply_action ~inject_dynamic ~inject_static:_ ~schedule_event result model action =
    match result with
    | Some result ->
      apply_action
        (Apply_action_context.create ~inject:inject_dynamic ~schedule_event)
        result
        model
        action
    | None ->
      let action = sexp_of_opaque action in
      eprint_s
        [%message
          "An action sent to a [wrap] has been dropped because its input was not \
           present. This happens when the [wrap] is inactive when it receives a message."
            (action : Sexp.t)];
      model
  in
  let model_var = Value.named Wrap_model model_id in
  let inject_var = Value.named Wrap_inject inject_id in
  let inner = f model_var inject_var in
  let wrapper_model =
    Meta.Model.of_module
      ~sexp_of_model:(Option.value sexp_of_model ~default:sexp_of_opaque)
      ~equal
      ~default:default_model
      ~name:"outer model for wrap"
  in
  Wrap
    { wrapper_model
    ; action_id
    ; result_id
    ; inject_id
    ; model_id
    ; inner
    ; dynamic_apply_action = apply_action
    ; reset
    }
;;

let with_model_resetter f =
  let reset_id = Type_equal.Id.create ~name:"reset-model" [%sexp_of: opaque] in
  let inner = f ~reset:(Value.named Model_resetter reset_id) in
  With_model_resetter { reset_id; inner }
;;

let path = Path

include Computation
