open! Core
open! Import

module Computation_info = struct
  (* Builds a [Computation.packed_info] with an environmnt-threading run function from a
     generic unit-returning [Computation.packed_info]. *)
  let with_threaded_environment
    :  ('r, unit) Computation.packed_info
    -> ('r, Environment.t option) Computation.packed_info
    =
    fun (T { model; input; action; apply_action; reset; run; can_contain_path }) ->
    let run ~environment ~path ~clock ~model ~inject =
      let%bind.Trampoline snapshot, () = run ~environment ~path ~clock ~model ~inject in
      Trampoline.return (snapshot, (None : Environment.t option))
    in
    Computation.T { model; input; action; apply_action; reset; run; can_contain_path }
  ;;

  (* Produces a "[unit]-extra" [Computation.packed_info] from an [eval_sub]-custom packed
     info that threads an environment through the run function. This function is used to
     convert back to a type that the general-purpose [gather] implementation can handle. *)
  let drop_threaded_environment
    :  ('r, Environment.t option) Computation.packed_info
    -> ('r, unit) Computation.packed_info
    =
    fun (Computation.T
          { model; input; action; apply_action; reset; run; can_contain_path }) ->
    let run ~environment ~path ~clock ~model ~inject =
      let%bind.Trampoline snapshot, (_ : Environment.t option) =
        run ~environment ~path ~clock ~model ~inject
      in
      Trampoline.return (snapshot, ())
    in
    Computation.T { model; input; action; apply_action; reset; run; can_contain_path }
  ;;
end

let ( >>> ) f inject b = inject (f b)
let wrap_sub_from inject = Action.sub_from >>> inject
let wrap_sub_into inject = Action.sub_into >>> inject

module Thread_env = struct
  (* values of this type are used to control if the environment of a sub is threaded 
     into the next sub in a chain, or if there's no threading (which is the standard 
     scoping rule) *)
  type 'a t =
    | None : unit t
    | Thread_env : Environment.t option t

  (* given a standard environment and what might be a threaded environment, pick an 
     environment to evaluate the [into] branch of a sub. *)
  let pick (type a) (thread_env : a t) ~environment ~(maybe_env : a) : Environment.t =
    match thread_env with
    | None -> environment
    | Thread_env ->
      (match maybe_env with
       | None -> environment
       | Some env -> env)
  ;;

  (* given a standard environment and what might be a threaded environment, capture
     an environment (if any) to thread into subsequent subs in the chain. *)
  let capture (type a) (thread_env : a t) ~environment ~(maybe_env : a) : a =
    match thread_env, maybe_env with
    | None, () -> ()
    | Thread_env, None -> Some environment
    | Thread_env, Some env -> Some env
  ;;
end

let baseline
  (type thread_env)
  ~here
  ~(info_from : (_, _, _, _, thread_env) Computation.info)
  ~(info_into : (_, _, _, _, thread_env) Computation.info)
  ~via
  ~(thread_environment : thread_env Thread_env.t)
  : (_, thread_env) Computation.packed_info
  =
  let reset ~inject ~schedule_event (model_from, model_into) =
    let model_from =
      info_from.reset ~inject:(wrap_sub_from inject) ~schedule_event model_from
    in
    let model_into =
      info_into.reset ~inject:(wrap_sub_into inject) ~schedule_event model_into
    in
    model_from, model_into
  in
  let apply_action ~inject ~schedule_event input (model_from, model_into) = function
    | Action.Sub_from action ->
      let model_from =
        info_from.apply_action
          ~inject:(wrap_sub_from inject)
          ~schedule_event
          (Option.map input ~f:fst)
          model_from
          action
      in
      model_from, model_into
    | Sub_into action ->
      let model_into =
        info_into.apply_action
          ~inject:(wrap_sub_into inject)
          ~schedule_event
          (Option.map input ~f:snd)
          model_into
          action
      in
      model_from, model_into
  in
  let both_use_path = info_from.can_contain_path && info_into.can_contain_path in
  let run ~environment ~path ~clock ~model ~inject =
    let%bind.Trampoline from, maybe_env =
      let model = Incr.map model ~f:Tuple2.get1 in
      let path = if both_use_path then Path.append path Path.Elem.Subst_from else path in
      info_from.run ~environment ~path ~clock ~model ~inject:(wrap_sub_from inject)
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Thread_env.pick thread_environment ~environment ~maybe_env in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let%bind.Trampoline into, maybe_env =
      let model = Incr.map model ~f:Tuple2.get2 in
      let path = if both_use_path then Path.append path Path.Elem.Subst_into else path in
      info_into.run ~environment ~path ~clock ~model ~inject:(wrap_sub_into inject)
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Input.merge (Snapshot.input from) (Snapshot.input into) in
    Trampoline.return
      ( Snapshot.create ~result ~input ~lifecycle
      , Thread_env.capture thread_environment ~environment ~maybe_env )
  in
  let model = Meta.Model.both info_from.model info_into.model in
  let input = Meta.Input.both info_from.input info_into.input in
  T
    { model
    ; input
    ; action = Action.Type_id.sub ~from:info_from.action ~into:info_into.action
    ; apply_action
    ; run
    ; reset
    ; can_contain_path = info_from.can_contain_path || info_into.can_contain_path
    }
;;

let from_stateless
  (type thread_env)
  ~here
  ~(info_from : (_, _, _, _, thread_env) Computation.info)
  ~(info_into : (_, _, _, _, thread_env) Computation.info)
  ~via
  ~(thread_environment : thread_env Thread_env.t)
  : (_, thread_env) Computation.packed_info
  =
  let both_use_path = info_from.can_contain_path && info_into.can_contain_path in
  let run ~environment ~path ~clock ~model ~inject =
    let%bind.Trampoline from, maybe_env =
      let path = if both_use_path then Path.append path Path.Elem.Subst_from else path in
      info_from.run ~environment ~path ~clock ~model:unit_model ~inject:unreachable_action
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Thread_env.pick thread_environment ~environment ~maybe_env in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let%bind.Trampoline into, maybe_env =
      let path = if both_use_path then Path.append path Path.Elem.Subst_into else path in
      info_into.run ~environment ~path ~clock ~model ~inject
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Snapshot.input into in
    Trampoline.return
      ( Snapshot.create ~result ~input ~lifecycle
      , Thread_env.capture thread_environment ~environment ~maybe_env )
  in
  T
    { run
    ; input = info_into.input
    ; model = info_into.model
    ; action = info_into.action
    ; apply_action = info_into.apply_action
    ; reset = info_into.reset
    ; can_contain_path = info_from.can_contain_path || info_into.can_contain_path
    }
;;

let into_stateless
  (type thread_env)
  ~here
  ~(info_from : (_, _, _, _, thread_env) Computation.info)
  ~(info_into : (_, _, _, _, thread_env) Computation.info)
  ~via
  ~(thread_environment : thread_env Thread_env.t)
  : (_, thread_env) Computation.packed_info
  =
  let both_use_path = info_from.can_contain_path && info_into.can_contain_path in
  let run ~environment ~path ~clock ~model ~inject =
    let%bind.Trampoline from, maybe_env =
      let path = if both_use_path then Path.append path Path.Elem.Subst_from else path in
      info_from.run ~environment ~path ~clock ~model ~inject
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Thread_env.pick thread_environment ~environment ~maybe_env in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let%bind.Trampoline into, maybe_env =
      let path = if both_use_path then Path.append path Path.Elem.Subst_into else path in
      info_into.run ~environment ~path ~clock ~model:unit_model ~inject:unreachable_action
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Snapshot.input from in
    Trampoline.return
      ( Snapshot.create ~result ~input ~lifecycle
      , Thread_env.capture thread_environment ~environment ~maybe_env )
  in
  T
    { run
    ; input = info_from.input
    ; model = info_from.model
    ; action = info_from.action
    ; apply_action = info_from.apply_action
    ; reset = info_from.reset
    ; can_contain_path = info_from.can_contain_path || info_into.can_contain_path
    }
;;

let gather
  (type thread_env a b c d e f g h)
  ~here
  ~(info_from : (a, b, c, d, thread_env) Computation.info)
  ~(info_into : (e, f, g, h, thread_env) Computation.info)
  ~via
  ~(thread_environment : thread_env Thread_env.t)
  : (_, thread_env) Computation.packed_info
  =
  let is_unit x = Meta.Model.Type_id.same_witness Meta.Model.unit.type_id x in
  let from_model = is_unit info_from.model.type_id in
  let from_action = Action.Type_id.same_witness info_from.action Action.Type_id.nothing in
  let open Option.Let_syntax in
  let can_run_from_stateless =
    let%bind a = from_model in
    let%bind b = from_action in
    Some (a, b)
  in
  match can_run_from_stateless with
  | Some (T, T) -> from_stateless ~here ~info_from ~info_into ~via ~thread_environment
  | None ->
    let into_model = is_unit info_into.model.type_id in
    let into_action =
      Action.Type_id.same_witness info_into.action Action.Type_id.nothing
    in
    let can_run_into_stateless =
      let%bind a = into_model in
      let%bind b = into_action in
      Some (a, b)
    in
    (match can_run_into_stateless with
     | Some (T, T) -> into_stateless ~here ~info_from ~info_into ~via ~thread_environment
     | None -> baseline ~here ~info_from ~info_into ~via ~thread_environment)
;;

module Chain = struct
  module Link = struct
    type t =
      | T :
          { bound : ('a, Environment.t option) Computation.packed_info
          ; via : 'a Type_equal.Id.t
          ; here : Source_code_position.t option
          }
          -> t
  end

  type 'a t =
    { init : Link.t list
    ; length_of_init : int
    ; final : ('a, unit) Computation.packed_info
    }

  type recurse =
    { f : 'a. 'a Computation.t -> ('a, unit) Computation.packed_info Trampoline.t }

  let rec build_chain computation ~acc ~length_of_acc ~recurse =
    match (computation : _ Computation.t) with
    | Sub { from; via; into; here } ->
      let%bind.Trampoline from = recurse.f from in
      let bound = Computation_info.with_threaded_environment from in
      build_chain
        into
        ~acc:Reversed_list.(Link.T { bound; via; here } :: acc)
        ~length_of_acc:(length_of_acc + 1)
        ~recurse
    | final ->
      let%bind.Trampoline final = recurse.f final in
      Trampoline.return
        { final; init = Reversed_list.rev acc; length_of_init = length_of_acc }
  ;;

  let reduce
    (Link.T { bound = T bound_left; via = via_left; here = here_left })
    (Link.T { bound = T bound_right; via = via_right; here = here_right })
    =
    let bound =
      gather
        ~here:here_left
        ~info_from:bound_left
        ~info_into:bound_right
        ~via:via_left
        ~thread_environment:Thread_env
    in
    Link.T { bound; via = via_right; here = here_right }
  ;;

  let gather = function
    | { init = []; final; _ } -> final
    | { init; final; length_of_init } ->
      let reducer = Balanced_reducer.create_exn ~len:length_of_init ~reduce () in
      List.iteri init ~f:(fun i link -> Balanced_reducer.set_exn reducer i link);
      let (T { bound = T info_from; via; here }) = Balanced_reducer.compute_exn reducer in
      let (T info_into) = Computation_info.with_threaded_environment final in
      gather ~here ~info_from ~info_into ~via ~thread_environment:Thread_env
      |> Computation_info.drop_threaded_environment
  ;;

  let gather computation ~recurse =
    let%bind.Trampoline t = build_chain computation ~recurse ~length_of_acc:0 ~acc:[] in
    Trampoline.return (gather t)
  ;;
end

let gather ~here ~info_from ~info_into ~via =
  gather ~here ~info_from ~info_into ~via ~thread_environment:None
;;

type generic_gather = Chain.recurse =
  { f : 'a. 'a Computation.t -> ('a, unit) Computation.packed_info Trampoline.t }

let chain c ~gather = Chain.gather c ~recurse:gather
