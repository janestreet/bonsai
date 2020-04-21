open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { t : ('k * 'input * 'extra, 'model, 'action, 'result, 'incr, 'event) unpacked
        ; action_type_id : 'action Type_equal.Id.t
        ; inner_model : 'model Packed.model_info
        ; comparator : ('k, 'cmp) comparator
        ; result_by_k : ('result_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
        ; input_by_k : ('input_by_k, ('k, 'input, 'cmp) Map.t) Type_equal.t
        ; model_by_k : ('model_by_k, ('k, 'model, 'cmp) Map.t) Type_equal.t
        (* We need the Type_equal witnesses here because the typechecker's rules aren't
           powerful enough to just have the Comparator.t here. *)
        }
        -> ( 'input_by_k * 'extra
           , 'model_by_k
           , 'k * 'action
           , 'result_by_k
           , 'incr
           , 'event )
             unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C
        { t
        ; action_type_id = _
        ; inner_model = _
        ; comparator = _
        ; result_by_k = _
        ; input_by_k = _
        ; model_by_k = _
        } -> [%sexp Assoc_by_input (t : unpacked)]
    | _ -> assert false
  ;;

  let associ_input
        (type k cmp)
        ((module Key_comparator) : (k, cmp) comparator)
        (Packed.T
           { unpacked
           ; action_type_id
           ; model =
               { type_id = model_type_id
               ; default = default_model
               ; equal = model_equal
               ; sexp_of = sexp_of_model
               ; of_sexp = model_of_sexp
               }
           })
    =
    let whole_action_type_id =
      let sexp_of_action = Type_equal.Id.to_sexp action_type_id in
      Type_equal.Id.create
        ~name:(Type_equal.Id.name action_type_id)
        [%sexp_of: Key_comparator.t * action]
    in
    let sexp_of_map_model = [%sexp_of: model Map.M(Key_comparator).t] in
    let model_map_type_id =
      Type_equal.Id.create
        ~name:(Source_code_position.to_string [%here])
        sexp_of_map_model
    in
    let model_map_equal = Map.equal model_equal in
    Packed.T
      { unpacked =
          C
            { t = unpacked
            ; action_type_id
            ; comparator = (module Key_comparator)
            ; model_by_k = T
            ; result_by_k = T
            ; input_by_k = T
            ; inner_model =
                { default = default_model
                ; equal = model_equal
                ; type_id = model_type_id
                ; sexp_of = sexp_of_model
                ; of_sexp = model_of_sexp
                }
            }
      ; action_type_id = whole_action_type_id
      ; model =
          { type_id = model_map_type_id
          ; default = Map.empty (module Key_comparator)
          ; equal = model_map_equal
          ; sexp_of = sexp_of_map_model
          ; of_sexp = [%of_sexp: model Map.M(Key_comparator).t]
          }
      }
  ;;
end

include T

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor C]

      let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
        fun ~input ~old_model ~model ~inject ~action_type_id:_ ~environment ~incr_state t ->
        match t with
        | C
            { t
            ; action_type_id
            ; result_by_k = T
            ; input_by_k = T
            ; model_by_k = T
            ; inner_model
            ; comparator = _
            } ->
          let map_input = input >>| Tuple2.get1 in
          let extra_input = input >>| Tuple2.get2 in
          let return = Incremental.return incr_state in
          let%bind comparator = map_input >>| Map.comparator_s in
          let (module Current_comparator) = comparator in
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
          let input_and_models_map =
            Incr_map.merge map_input model_and_old_model_map ~f:(fun ~key:_ ->
              function
              | `Left input -> Some (input, (inner_model.default, None))
              | `Right _ -> None
              | `Both input_and_models -> Some input_and_models)
          in
          let snapshot_map =
            Incr_map.mapi' input_and_models_map ~f:(fun ~key ~data:input_and_models ->
              let%pattern_bind input_from_map, (model, old_model) = input_and_models in
              let input =
                let%map input = input_from_map
                and extra = extra_input in
                key, input, extra
              in
              let inject action = inject (key, action) in
              eval_ext
                ~input
                ~model
                ~old_model
                ~inject
                ~action_type_id
                ~environment
                ~incr_state
                t)
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
        | _ -> assert false
      ;;

      let visit
            (type i r incr event)
            (T { unpacked; action_type_id = _; model = _ } : (i, r, incr, event) Packed.t)
            visitor
        : (i, r, incr, event) Packed.t
        =
        match unpacked with
        | C
            { t
            ; action_type_id
            ; comparator
            ; model_by_k
            ; input_by_k
            ; result_by_k
            ; inner_model
            } ->
          let T = model_by_k in
          let (module Key_comparator) = comparator in
          let visited =
            let (Packed.T { unpacked; action_type_id; model = inner_model }) =
              visit_ext
                (Packed.T { unpacked = t; action_type_id; model = inner_model })
                visitor
            in
            let ({ type_id = _
                 ; default = _
                 ; equal = equal_inner_model
                 ; of_sexp = inner_model_of_sexp
                 ; sexp_of = sexp_of_inner_model
                 }
                 : _ Packed.model_info)
              =
              inner_model
            in
            let sexp_of_action = Type_equal.Id.to_sexp action_type_id in
            let whole_action_type_id =
              Type_equal.Id.create
                ~name:(Type_equal.Id.name action_type_id)
                [%sexp_of: Key_comparator.t * action]
            in
            let sexp_of_map_model = [%sexp_of: inner_model Map.M(Key_comparator).t] in
            let map_model_of_sexp = [%of_sexp: inner_model Map.M(Key_comparator).t] in
            let model_map_type_id =
              Type_equal.Id.create
                ~name:(Source_code_position.to_string [%here])
                sexp_of_map_model
            in
            Packed.T
              { unpacked =
                  C
                    { t = unpacked
                    ; action_type_id
                    ; input_by_k
                    ; result_by_k
                    ; comparator
                    ; model_by_k = T
                    ; inner_model
                    }
              ; action_type_id = whole_action_type_id
              ; model =
                  { type_id = model_map_type_id
                  ; default = Map.empty (module Key_comparator)
                  ; equal = Map.equal equal_inner_model
                  ; sexp_of = sexp_of_map_model
                  ; of_sexp = map_model_of_sexp
                  }
              }
          in
          visitor.visit visited
        | _ -> assert false
      ;;
    end)
;;
