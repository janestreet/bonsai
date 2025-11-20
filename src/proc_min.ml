open! Core
open! Import
open Computation

let read ~(here : [%call_pos]) value = Return { value; here }

let watch_computation
  ~here
  ~log_model_before
  ~log_model_after
  ~log_action
  ~log_incr_info
  ~log_watcher_positions
  ~log_dependency_definition_position
  ~label
  inner
  =
  Computation_watcher
    { here
    ; inner
    ; free_vars = Computation_watcher.Type_id_location_map.empty
    ; config =
        { log_model_before
        ; log_model_after
        ; log_action
        ; log_incr_info
        ; log_watcher_positions
        ; log_dependency_definition_position
        ; label
        }
    ; queue = None
    ; value_id_observation_definition_positions = None
    ; enable_watcher =
        false
        (* [enable_watcher] will be set during the transformation stage if computation
           watchers are enabled *)
    }
;;

let sub (type via) ~(here : [%call_pos]) (from : via Computation.t) ~f =
  match from with
  | Return { value = { here = _; value = Named _ as named }; here } ->
    f { Value.here; value = named }
  | _ ->
    let via : via Type_equal.Id.t =
      Type_equal.Id.create
        ~name:(Source_code_position.to_string [%here])
        [%sexp_of: opaque]
    in
    let into = f (Value.named ~here (Sub here) via) in
    Sub
      { from
      ; via
      ; into
      ; (* We only invert lifecycles for explicit calls to
           [Bonsai.with_inverted_lifecycles]. *)
        invert_lifecycles = false
      ; here
      }
;;

let with_inverted_lifecycle_ordering ~(here : [%call_pos]) ~compute_dep f =
  let via = Type_equal.Id.create ~name:"inverted-lifecycle-dep-id" [%sexp_of: opaque] in
  let into = f (Value.named ~here Inverted_lifecycles_dependency via) in
  Sub { from = compute_dep; via; into; invert_lifecycles = true; here }
;;

let switch ~here ~match_ ~branches ~(local_ with_) =
  let arms =
    List.init branches ~f:(fun key ->
      let computation =
        try with_ key with
        | exn -> read ~here (Value.return_exn exn)
      in
      key, computation)
    |> Int.Map.of_alist_exn
  in
  Switch { match_; arms; here }
;;

module Dynamic_scope = struct
  let fetch ~(here : [%call_pos]) ~id ~default ~for_some () =
    Fetch { id; default; for_some; here }
  ;;

  let store ~(here : [%call_pos]) ~id ~value ~inner () = Store { id; value; inner; here }
end

module Edge = struct
  let lifecycle ~(here : [%call_pos]) lifecycle = Lifecycle { lifecycle; here }
end

let state_machine_with_input_safe
  ~here
  ?(sexp_of_action = sexp_of_opaque)
  ~sexp_of_model
  ?reset
  ~equal
  ~default_model
  ~apply_action
  input
  =
  let name = Source_code_position.to_string here in
  let reset =
    match reset with
    | None -> fun ~inject:_ ~schedule_event:_ ~time_source:_ _ -> default_model
    | Some reset ->
      fun ~inject ~schedule_event ~time_source ->
        reset (Apply_action_context.Private.create ~inject ~schedule_event ~time_source)
  in
  let apply_action ~inject ~schedule_event ~time_source =
    apply_action
      (Apply_action_context.Private.create ~inject ~schedule_event ~time_source)
  in
  Leaf1
    { model = Meta.Model.of_module ~sexp_of_model ~equal ~name ~default:default_model
    ; input_id = Meta.Input.create ()
    ; dynamic_action = Type_equal.Id.create ~name sexp_of_action
    ; apply_action
    ; reset
    ; input
    ; here
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

let state_machine_with_input
  ~(here : [%call_pos])
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
  state_machine_with_input_safe
    ~here
    ?sexp_of_action
    ~sexp_of_model:(Option.value sexp_of_model ~default:sexp_of_opaque)
    ?reset
    ~equal
    ~default_model
    ~apply_action
    input
;;

let state_machine
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?(sexp_of_action = sexp_of_opaque)
  ?equal
  ~default_model
  ~apply_action
  ()
  =
  let name = Source_code_position.to_string here in
  let apply_action ~inject ~schedule_event ~time_source =
    apply_action
      (Apply_action_context.Private.create ~inject ~schedule_event ~time_source)
  in
  let reset =
    match reset with
    | None -> fun ~inject:_ ~schedule_event:_ ~time_source:_ _ -> default_model
    | Some reset ->
      fun ~inject ~schedule_event ~time_source ->
        reset (Apply_action_context.Private.create ~inject ~schedule_event ~time_source)
  in
  Leaf0
    { model =
        Meta.Model.of_module
          ~sexp_of_model:(Option.value ~default:sexp_of_opaque sexp_of_model)
          ~equal
          ~name
          ~default:default_model
    ; static_action = Type_equal.Id.create ~name sexp_of_action
    ; apply_action
    ; reset
    ; here
    }
;;

module Proc_incr = struct
  let value_cutoff ~(here : [%call_pos]) t ~equal =
    read ~here (Value.cutoff ~here ~added_by_let_syntax:false ~equal t)
  ;;

  let compute_with_clock ~(here : [%call_pos]) t ~f =
    Computation.Leaf_incr { input = t; compute = f; here }
  ;;

  let of_module
    (type input model result)
    ~(here : [%call_pos])
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
      ~here
      (state_machine_with_input
         ~sexp_of_action:M.Action.sexp_of_t
         ?sexp_of_model
         ~equal
         ~default_model
         ~apply_action:(fun context input model action ->
           let { Apply_action_context.Private.inject; schedule_event; time_source = _ } =
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
  ~(here : [%call_pos])
  (comparator : (k, cmp) Comparator.Module.t)
  (map : (k, v, cmp) Map.t Value.t)
  ~f
  =
  let module C = (val comparator) in
  let key_id : k Type_equal.Id.t =
    Type_equal.Id.create ~name:"key id" (Comparator.sexp_of_t C.comparator)
  in
  let cmp_id : cmp Type_equal.Id.t =
    Type_equal.Id.create ~name:"cmp id" [%sexp_of: opaque]
  in
  let data_id : v Type_equal.Id.t =
    Type_equal.Id.create ~name:"data id" [%sexp_of: opaque]
  in
  let key_var = Value.named ~here Assoc_like_key key_id in
  let data_var = Value.named ~here Assoc_like_data data_id in
  let by = f key_var data_var in
  Assoc { map; key_comparator = comparator; key_id; cmp_id; data_id; by; here }
;;

let assoc_on
  (type model_k io_k model_cmp io_cmp v)
  ~(here : [%call_pos])
  (io_comparator : (io_k, io_cmp) Comparator.Module.t)
  (model_comparator : (model_k, model_cmp) Comparator.Module.t)
  (map : (io_k, v, io_cmp) Map.t Value.t)
  ~get_model_key
  ~f
  =
  let module Io_comparator = (val io_comparator) in
  let module Model_comparator = (val model_comparator) in
  let io_key_id : io_k Type_equal.Id.t =
    Type_equal.Id.create ~name:"io key id" (Comparator.sexp_of_t Io_comparator.comparator)
  in
  let io_cmp_id : io_cmp Type_equal.Id.t =
    Type_equal.Id.create ~name:"io cmp id" [%sexp_of: opaque]
  in
  let model_key_id : model_k Type_equal.Id.t =
    Type_equal.Id.create
      ~name:"model key id"
      (Comparator.sexp_of_t Model_comparator.comparator)
  in
  let model_cmp_id : model_cmp Type_equal.Id.t =
    Type_equal.Id.create ~name:"model key id" [%sexp_of: opaque]
  in
  let data_id : v Type_equal.Id.t =
    Type_equal.Id.create ~name:"data id" [%sexp_of: opaque]
  in
  let key_var = Value.named ~here Assoc_like_key io_key_id in
  let data_var = Value.named ~here Assoc_like_data data_id in
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
    ; here
    }
;;

let lazy_ ~(here : [%call_pos]) t = Lazy { t; here }

let fix
  (type input result)
  ~(here : [%call_pos])
  (input : input Value.t)
  ~(local_ f :
             recurse:(input Value.t -> result Computation.t)
             -> input Value.t
             -> result Computation.t)
  =
  let fix_id : result Fix_id.t = Fix_id.create () in
  let input_id : input Type_equal.Id.t =
    Type_equal.Id.create ~name:"fix input" sexp_of_opaque
  in
  let arg_value = Value.named ~here Fix_recurse input_id in
  let fix_recurse input = Fix_recurse { input; input_id; fix_id; here } in
  let result = f ~recurse:fix_recurse arg_value in
  Fix_define { fix_id; initial_input = input; input_id; result; here }
;;

let wrap
  (type model action)
  ~(here : [%call_pos])
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
    match reset with
    | None -> fun ~inject:_ ~schedule_event:_ ~time_source:_ _ -> default_model
    | Some reset ->
      fun ~inject ~schedule_event ~time_source ->
        reset (Apply_action_context.Private.create ~inject ~schedule_event ~time_source)
  in
  let action_id : action Type_equal.Id.t =
    Type_equal.Id.create ~name:"action id" [%sexp_of: opaque]
  in
  let result_id = Meta.Input.create () in
  let inject_id : (action -> unit Effect.t) Type_equal.Id.t =
    Type_equal.Id.create ~name:"inject id" [%sexp_of: opaque]
  in
  let apply_action ~inject ~schedule_event ~time_source result model action =
    let result =
      match result with
      | Some result -> Computation_status.Active result
      | None -> Computation_status.Inactive
    in
    apply_action
      (Apply_action_context.Private.create ~inject ~schedule_event ~time_source)
      result
      model
      action
  in
  let model_var = Value.named ~here Wrap_model model_id in
  let inject_var = Value.named ~here Wrap_inject inject_id in
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
    ; here
    }
;;

let with_model_resetter ~(here : [%call_pos]) f =
  let reset_id = Type_equal.Id.create ~name:"reset-model" [%sexp_of: opaque] in
  let inner = f ~reset:(Value.named ~here Model_resetter reset_id) in
  With_model_resetter { reset_id; inner; here }
;;

let path ~(here : [%call_pos]) () = Path { here }
