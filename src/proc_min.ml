open! Core
open! Import
open Computation

let read ?(here = Stdlib.Lexing.dummy_pos) value = Return { value; here }

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
    ; free_vars = Computation_watcher.Var_id_location_map.empty
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

let sub (type via) ?(here = Stdlib.Lexing.dummy_pos) (from : via Computation.t) ~f =
  match from with
  | Return { value = { here = _; value = Named _ as named }; here } ->
    f { Value.here; value = named }
  | _ ->
    let via : via Var_id.t = Var_id.create () in
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

let with_inverted_lifecycle_ordering ?(here = Stdlib.Lexing.dummy_pos) ~compute_dep f =
  let via = Var_id.create () in
  let into = f (Value.named ~here Inverted_lifecycles_dependency via) in
  Sub { from = compute_dep; via; into; invert_lifecycles = true; here }
;;

let bind ?(here = Stdlib.Lexing.dummy_pos) comparator bound ~f =
  let bound_id = Var_id.create () in
  let cmp_id = Var_id.create () in
  Bind { bound; comparator; bound_id; cmp_id; f; here }
;;

let switch ~here ~match_ ~branches ~with_ =
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
  let fetch ?(here = Stdlib.Lexing.dummy_pos) ~id ~default ~for_some () =
    Fetch { id; default; for_some; here }
  ;;

  let store ?(here = Stdlib.Lexing.dummy_pos) ~id ~value ~inner () =
    Store { id; value; inner; here }
  ;;
end

module Edge = struct
  let lifecycle ?(here = Stdlib.Lexing.dummy_pos) lifecycle =
    Lifecycle { lifecycle; here }
  ;;
end

module Computation_status = Bonsai_private_base.Computation_status

let state_machine_with_input
  ?(here = Stdlib.Lexing.dummy_pos)
  ?sexp_of_action
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~apply_action
  input
  =
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
    { model =
        Meta.Model.of_module
          ~sexp_of_model
          ~equal
          ~name:"state_machine_with_input"
          ~default:default_model
    ; input_id = Meta.Input.create ()
    ; dynamic_action = Var_id.create ()
    ; apply_action
    ; reset
    ; input
    ; action_name = "state_machine_with_input"
    ; sexp_of_action
    ; here
    }
;;

let state_machine
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~default_model
  ~apply_action
  ()
  =
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
          ~sexp_of_model
          ~equal
          ~name:"state_machine"
          ~default:default_model
    ; static_action = Var_id.create ()
    ; apply_action
    ; reset
    ; action_name = "state_machine"
    ; sexp_of_action
    ; here
    }
;;

module Proc_incr = struct
  let value_cutoff ?(here = Stdlib.Lexing.dummy_pos) t ~equal =
    read ~here (Value.cutoff ~here ~added_by_let_syntax:false ~equal t)
  ;;

  let compute_with_clock ?(here = Stdlib.Lexing.dummy_pos) t ~f =
    Computation.Leaf_incr { input = t; compute = f; here }
  ;;

  let of_module
    (type input model result)
    ?(here = Stdlib.Lexing.dummy_pos)
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
  ?(here = Stdlib.Lexing.dummy_pos)
  (comparator : (k, cmp) Comparator.Module.t)
  (map : (k, v, cmp) Map.t Value.t)
  ~f
  =
  let key_id : k Var_id.t = Var_id.create () in
  let cmp_id : cmp Var_id.t = Var_id.create () in
  let data_id : v Var_id.t = Var_id.create () in
  let key_var = Value.named ~here Assoc_like_key key_id in
  let data_var = Value.named ~here Assoc_like_data data_id in
  let by = f key_var data_var in
  Assoc { map; key_comparator = comparator; key_id; cmp_id; data_id; by; here }
;;

let assoc_on
  (type model_k io_k model_cmp io_cmp v)
  ?(here = Stdlib.Lexing.dummy_pos)
  (io_comparator : (io_k, io_cmp) Comparator.Module.t)
  (model_comparator : (model_k, model_cmp) Comparator.Module.t)
  (map : (io_k, v, io_cmp) Map.t Value.t)
  ~get_model_key
  ~f
  =
  let io_key_id : io_k Var_id.t = Var_id.create () in
  let io_cmp_id : io_cmp Var_id.t = Var_id.create () in
  let model_key_id : model_k Var_id.t = Var_id.create () in
  let model_cmp_id : model_cmp Var_id.t = Var_id.create () in
  let data_id : v Var_id.t = Var_id.create () in
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

let lazy_ ?(here = Stdlib.Lexing.dummy_pos) t = Lazy { t; here }

let fix
  (type input result)
  ?(here = Stdlib.Lexing.dummy_pos)
  (input : input Value.t)
  ~(f :
      recurse:(input Value.t -> result Computation.t)
      -> input Value.t
      -> result Computation.t)
  =
  let fix_id : result Fix_id.t = Fix_id.create () in
  let input_id : input Var_id.t = Var_id.create () in
  let arg_value = Value.named ~here Fix_recurse input_id in
  let fix_recurse input = Fix_recurse { input; input_id; fix_id; here } in
  let result = f ~recurse:fix_recurse arg_value in
  Fix_define { fix_id; initial_input = input; input_id; result; here }
;;

let wrap
  (type model action)
  ?(here = Stdlib.Lexing.dummy_pos)
  ?reset
  ?sexp_of_model
  ?equal
  ~default_model
  ~apply_action
  ~f
  ()
  =
  let model_id : model Var_id.t = Var_id.create () in
  let reset =
    match reset with
    | None -> fun ~inject:_ ~schedule_event:_ ~time_source:_ _ -> default_model
    | Some reset ->
      fun ~inject ~schedule_event ~time_source ->
        reset (Apply_action_context.Private.create ~inject ~schedule_event ~time_source)
  in
  let action_id : action Var_id.t = Var_id.create () in
  let result_id = Meta.Input.create () in
  let inject_id : (action -> unit Effect.t) Var_id.t = Var_id.create () in
  let apply_action ~inject ~schedule_event ~time_source result model action =
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
      ~sexp_of_model
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

let with_model_resetter ?(here = Stdlib.Lexing.dummy_pos) f =
  let reset_id = Var_id.create () in
  let inner = f ~reset:(Value.named ~here Model_resetter reset_id) in
  With_model_resetter { reset_id; inner; here }
;;

let path ?(here = Stdlib.Lexing.dummy_pos) () = Path { here }
