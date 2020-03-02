open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

type ('data, 'result, 'k, 'cmp, 'r_by_k, 'd_by_k) t =
  { r_by_k : ('r_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
  ; d_by_k : ('d_by_k, ('k, 'data, 'cmp) Map.t) Type_equal.t
  }

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | By_model :
        { (* We need the Type_equal witnesses here because the typechecker's rules aren't
             powerful enough to just have the Comparator.t here. *)
          t : ('k * 'input, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; action_type_id : 'action Type_equal.Id.t
        ; sexp_of_key : 'k -> Sexp.t
        ; assoc : ('model, 'result, 'k, 'cmp, 'r_by_k, 'm_by_k) t
        }
        -> ('input, 'm_by_k, 'k * 'action, 'r_by_k, 'incr, 'event) unpacked
    | By_input :
        { (* We need the Type_equal witnesses here because the typechecker's rules aren't
             powerful enough to just have the Comparator.t here. *)
          t : ('k * 'input, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; action_type_id : 'action Type_equal.Id.t
        ; sexp_of_key : 'k -> Sexp.t
        ; assoc : ('input, 'result, 'k, 'cmp, 'r_by_k, 'i_by_k) t
        }
        -> ('i_by_k, 'model, 'k * 'action, 'r_by_k, 'incr, 'event) unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | By_model { t; action_type_id = _; sexp_of_key = _; assoc = _ } ->
      [%sexp Assoc_by_model (t : unpacked)]
    | By_input { t; action_type_id = _; sexp_of_key = _; assoc = _ } ->
      [%sexp Assoc_by_input (t : unpacked)]
    | _ -> assert false
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id:_ ~incr_state t ->
      match t with
      | By_model { t; action_type_id; assoc = { r_by_k = T; d_by_k = T }; _ } ->
        (* I couldn't figure out how to pull this out into another function *)
        let%bind comparator = model >>| Map.comparator_s in
        let (module Current_comparator) = comparator in
        (* While incremental binds are avoided as much as possible, in this case,
           because the comparator for a map is stable for the type, we can bind
           here and incur no real performance penalty.  This pattern is pervasive
           throughout the Incr_map library. *)
        let old_model =
          match%map old_model with
          | Some m -> m
          | None -> Map.empty comparator
        in
        let model_and_old_model_map =
          Incr_map.merge model old_model ~f:(fun ~key:_ ->
            function
            | `Left model -> Some (model, None)
            | `Right _ -> None
            | `Both (model, old_model) -> Some (model, Some old_model))
        in
        let snapshot_map =
          Incr_map.mapi' model_and_old_model_map ~f:(fun ~key ~data:model_and_old_model ->
            let input =
              let%map input = input in
              key, input
            in
            let return = Incremental.return incr_state in
            let%pattern_bind model, old_model = model_and_old_model in
            let inject action = inject (key, action) in
            eval_ext ~input ~model ~old_model ~inject ~action_type_id ~incr_state t)
        in
        let results_map =
          Incr_map.mapi snapshot_map ~f:(fun ~key:_ ~data:snapshot ->
            Snapshot.result snapshot)
        in
        let apply_action =
          let%map snapshot_map = snapshot_map
          and model = model in
          fun ~schedule_event action ->
            let id, action = action in
            match Map.find snapshot_map id with
            | None -> model
            (* drop it on the floor *)
            | Some snapshot ->
              let data = Snapshot.apply_action snapshot ~schedule_event action in
              Map.set model ~key:id ~data
        in
        let%map apply_action = apply_action
        and result = results_map in
        Snapshot.create ~result ~apply_action
      | By_input { t; action_type_id; assoc = { r_by_k = T; d_by_k = T }; _ } ->
        (* I couldn't figure out how to pull this out into another function *)
        let%bind comparator = input >>| Map.comparator_s in
        let (module Current_comparator) = comparator in
        let snapshot_map =
          Incr_map.mapi' input ~f:(fun ~key ~data:input ->
            let input =
              let%map input = input in
              key, input
            in
            let inject action = inject (key, action) in
            eval_ext ~input ~model ~old_model ~inject ~action_type_id ~incr_state t)
        in
        let results_map =
          Incr_map.mapi snapshot_map ~f:(fun ~key:_ ~data:snapshot ->
            Snapshot.result snapshot)
        in
        let apply_action =
          let%map snapshot_map = snapshot_map
          and model = model in
          fun ~schedule_event action ->
            let id, action = action in
            match Map.find snapshot_map id with
            | None -> model
            (* drop it on the floor *)
            | Some snapshot -> Snapshot.apply_action snapshot ~schedule_event action
        in
        let%map apply_action = apply_action
        and result = results_map in
        Snapshot.create ~result ~apply_action
      | _ -> assert false
  ;;

  let visit (type i m r incr event) (component : (i, m, r, incr, event) Packed.t) visitor
    : (i, m, r, incr, event) Packed.t
    =
    match component with
    | Packed.T (By_model { t; action_type_id; assoc; sexp_of_key }, typ_id) ->
      let (Packed.T (t, atid)) = visit_ext (Packed.T (t, action_type_id)) visitor in
      let typ_id =
        Type_equal.Id.create
          ~name:(Type_equal.Id.name typ_id)
          (sexp_of_pair sexp_of_key (Type_equal.Id.to_sexp atid))
      in
      visitor.visit
        (Packed.T (By_model { t; action_type_id = atid; sexp_of_key; assoc }, typ_id))
    | Packed.T (By_input { t; action_type_id; assoc; sexp_of_key }, typ_id) ->
      let (Packed.T (t, atid)) = visit_ext (Packed.T (t, action_type_id)) visitor in
      let typ_id =
        Type_equal.Id.create
          ~name:(Type_equal.Id.name typ_id)
          (sexp_of_pair sexp_of_key (Type_equal.Id.to_sexp atid))
      in
      visitor.visit
        (Packed.T (By_input { t; action_type_id = atid; sexp_of_key; assoc }, typ_id))
    | _ -> assert false
  ;;
end

include T

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor By_input]
    end)
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor By_model]
    end)
;;
