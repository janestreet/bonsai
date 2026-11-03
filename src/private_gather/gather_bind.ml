open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base
open Incr.Let_syntax

(* The interesting thing about the [Bind] computation is how it runs [gather].

   Almost every other computation will run [gather] for subcomputations, but [bind] is
   dynamic, so we need to do it at runtime. The first time [gather] on a subcomputation is
   called will be in [run] as the incremental graph is being built.

   But we _also_ need the [gather]ed [Computation.info] inside of [apply_action]. The
   trick that I use is to smuggle the packed [Computation.info] into the action itself, so
   that you can use it from inside of [apply_action].

   But we _also_ need the [gather]ed [Computation.info] inside of [reset]. For this
   reason, the [apply_action] function sneaks the [Computation.info] into the model for
   the component.

   Finally, [apply_action] needs to know which bound value passed to [f] is _currently
   live_, so that it can drop a stale action (one injected by a previous call to [f]) even
   when the multi-model has no entry for the bound key to compare the action's type-ids
   against. Everything in the action was smuggled there by the (possibly stale) invocation
   that created it, so the live [Computation.info] travels through the snapshot's input
   instead, alongside the bound key.

   Having the [Computation.info] inside of the model is also useful for the [run]
   implementation. If it's present, then [run] can skip calling [gather], which can save a
   lot of time. *)

let f
  (type a b cmp)
  ~gather
  ~recursive_scopes
  ~time_source
  ~bound
  ~(comparator : (a, cmp) Comparator.Module.t)
  ~bound_id
  ~cmp_id
  ~(f : a -> b Computation.t)
  ~here
  =
  let extra_id = Var_id.create () in
  let action_type_id = Action.Type_id.bind ~bound_id ~extra_id in
  let sexp_of_bound, bound_equal =
    let module M = (val comparator) in
    let equal a b = Comparator.compare M.comparator a b = 0 in
    let sexp_of = Comparator.sexp_of_t M.comparator in
    sexp_of, equal
  in
  let keyed_path_element = unstage (Path.Elem.keyed ~comparator bound_id) in
  let wrap_inject ~inject ~type_id ~bound ~extra action =
    inject (Action.bind ~bound_id ~bound ~comparator ~type_id ~extra_id ~extra action)
  in
  let run ~environment ~fix_envs ~path ~model ~inject =
    let bound = Value.eval environment bound in
    let result_input_and_lifecycle =
      let gathered_from_model =
        let%map bound and model in
        match Meta.Multi_model.find model bound with
        | None -> None
        | Some (Meta.Model.Hidden.T { extra; _ }) -> Some (Trampoline.return extra)
      in
      let bound_and_gathered_from_model =
        let node = Incr.both bound gathered_from_model in
        (* Fetching the gathered computation info from the model is an optimization. It
           would be bad if this value changing from [None] to [Some] caused us to rebuild
           the entire incremental graph, so we set the cutoff for this node so that it
           only fires when the [bound] node changes. *)
        Incremental.set_cutoff
          node
          (Incremental.Cutoff.of_equal (fun (before, _) (after, _) ->
             bound_equal before after));
        node
      in
      let%bind bound, gathered_from_model = bound_and_gathered_from_model in
      Trampoline.run
        (let path =
           (* always append a path element because we don't know what the _other_
              computations [f] might return, and we may need paths for disambiguation even
              if the current computation doesn't use them. *)
           Path.append path (Path.Elem.Bind (keyed_path_element bound))
         in
         let%bind.Trampoline (Computation.T
                                { model = model_info
                                ; input = input_info
                                ; action = action_info
                                ; run
                                ; apply_action = _
                                ; reset = _
                                ; may_contain = _
                                } as packed_info_for_bound_gathered)
           =
           Option.value_or_thunk gathered_from_model ~default:(fun () ->
             gather ~recursive_scopes ~time_source (f bound))
         in
         let inject =
           wrap_inject
             ~inject
             ~bound
             ~type_id:action_info
             ~extra:packed_info_for_bound_gathered
         in
         let chosen_model =
           Incremental.map model ~f:(fun map ->
             match Meta.Multi_model.find map bound with
             | None -> model_info.default
             | Some (Meta.Model.Hidden.T { model; info; extra = _ }) ->
               let equal =
                 Meta.Model.Type_id.same_witness_exn info.type_id model_info.type_id
               in
               Type_equal.conv equal model)
         in
         let%bind.Trampoline snapshot, () =
           run ~environment ~fix_envs ~path ~model:chosen_model ~inject
         in
         let input =
           let key = bound, packed_info_for_bound_gathered in
           let%mapn input = Input.to_incremental (Snapshot.input snapshot) in
           Meta.Input.Hidden.T { input; type_id = input_info; key }
         in
         Trampoline.return
           (Incr.return
              (Snapshot.result snapshot, input, Snapshot.lifecycle_or_empty ~here snapshot)))
    in
    let result = Incr.bind result_input_and_lifecycle ~f:Tuple3.get1
    and input = Incr.bind result_input_and_lifecycle ~f:Tuple3.get2
    and lifecycle = Incr.bind result_input_and_lifecycle ~f:Tuple3.get3 in
    let lifecycle = Some lifecycle in
    let input = Input.dynamic input in
    Trampoline.return (Snapshot.create ~here ~result ~input ~lifecycle, ())
  in
  let apply_action
    ~(inject : (a, _) Action.bind Action.t -> unit Effect.t)
    ~schedule_event
    (input :
      (a * (b, unit) Computation.packed_info) Meta.Input.Hidden.t Computation_status.t)
    model
    (Action.Bind
      { action
      ; type_id = action_type_id
      ; bound
      ; extra
      ; bound_id = _
      ; comparator = _
      ; extra_id = _
      })
    =
    let (Computation.T { model = tm; input = im; action = am; apply_action; _ }) =
      extra
    in
    let (T { model = chosen_model; info = chosen_model_info; _ }) =
      Option.value_or_thunk (Meta.Multi_model.find model bound) ~default:(fun () ->
        (* If the bind node is active and the bound value matches the one for which the
           action is destined for, use the model info that was already computed when the
           action was created instead of minting a new one. *)
        match input with
        | Computation_status.Active (Meta.Input.Hidden.T { key = bound', live_extra; _ })
          when bound_equal bound bound' ->
          let (Computation.T { model = live_model_info; _ }) = live_extra in
          Meta.Model.Hidden.create live_model_info live_model_info.default live_extra
        | Active _ | Inactive -> Meta.Model.Hidden.create tm tm.default extra)
    in
    match
      ( Action.Type_id.same_witness action_type_id am
      , Meta.Model.Type_id.same_witness chosen_model_info.type_id tm.type_id )
    with
    | Some T, Some T ->
      let new_model =
        match input with
        | Computation_status.Active
            (Meta.Input.Hidden.T
              { input = chosen_input; type_id = chosen_input_info; key = bound', _ }) ->
          (match
             bound_equal bound bound', Meta.Input.same_witness chosen_input_info im
           with
           | true, Some T ->
             apply_action
               ~inject:(wrap_inject ~type_id:am ~bound ~inject ~extra)
               ~schedule_event
               (Active chosen_input)
               chosen_model
               action
           | _ ->
             apply_action
               ~inject:(wrap_inject ~type_id:am ~bound ~inject ~extra)
               ~schedule_event
               Inactive
               chosen_model
               action)
        | Inactive ->
          apply_action
            ~inject:(wrap_inject ~type_id:am ~bound ~inject ~extra)
            ~schedule_event
            Inactive
            chosen_model
            action
      in
      if chosen_model_info.equal new_model chosen_model_info.default
      then Meta.Multi_model.remove model bound
      else (
        let new_model = Meta.Model.Hidden.create tm new_model extra in
        Meta.Multi_model.set model ~key:bound ~data:new_model)
    | None, None | Some T, None | None, Some T ->
      let action = Action.Type_id.to_sexp action_type_id action in
      Ui_metrics.Counters.observe Bonsai_switch_action_dropped;
      eprint_s
        [%message
          "an action inside of Bonsai.stitch has been dropped because the computation is \
           no longer active"
            ~bound:(sexp_of_bound bound : Sexp.t)
            (action : Sexp.t)];
      model
  in
  let empty_model = Meta.Multi_model.empty comparator ~id:bound_id ~cmp_id ~extra_id in
  (* [reset] is the reason that the model needs to store the [gathered] result in addition
     to it being present in the [action] because we don't have access to an action inside
     of [reset]. *)
  let reset ~inject ~schedule_event model =
    let f ~key:bound ~data:(model : _ Meta.Model.Hidden.t) =
      let (T { model = chosen_model; info = chosen_model_info; extra; _ }) = model in
      let (Computation.T { model = tm; action = am; reset; _ }) = extra in
      let T = Meta.Model.Type_id.same_witness_exn tm.type_id chosen_model_info.type_id in
      let inject = wrap_inject ~type_id:am ~bound ~inject ~extra in
      let new_model = reset ~inject ~schedule_event chosen_model in
      if chosen_model_info.equal new_model chosen_model_info.default
      then None
      else Some (Meta.Model.Hidden.create tm new_model extra)
    in
    let sub_models = Map.filter_mapi (Meta.Multi_model.to_models model) ~f in
    if Map.is_empty sub_models
    then empty_model
    else Meta.Multi_model.of_models sub_models ~id:bound_id ~cmp_id ~extra_id
  in
  Trampoline.return
    (Computation.T
       { model = Meta.Multi_model.model_info empty_model
       ; input = Meta.Input.create ()
       ; action = action_type_id
       ; apply_action
       ; run
       ; reset
       ; may_contain = May_contain.Unresolved.bind
       })
;;
