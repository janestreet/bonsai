open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module Case_action = struct
  type t =
    | T :
        { action : 'a
        ; type_id : 'a Type_equal.Id.t
        ; userdata : 'u
        ; sexp_of_userdata : 'u -> Sexp.t
        }
        -> t

  let sexp_of_t (T { action; type_id; userdata; sexp_of_userdata }) =
    let sexp_of_action = Type_equal.Id.to_sexp type_id in
    [%message "Bonsai.Switch.Case_action" (action : action) (userdata : userdata)]
  ;;

  let type_id =
    Type_equal.Id.create ~name:(Source_code_position.to_string [%here]) [%sexp_of: t]
  ;;
end

module Poly_model = struct
  module Packed = struct
    type t =
      | T :
          { model : 'm
          ; info : 'm Packed.model_info
          ; t_of_sexp : Sexp.t -> t
          }
          -> t

    let sexp_of_t (T { model; info = { sexp_of; _ }; _ }) = sexp_of model

    let equal
          (T { model = m1; info = { type_id = t1; equal; _ }; _ })
          (T { model = m2; info = { type_id = t2; _ }; _ })
      =
      match Type_equal.Id.same_witness t1 t2 with
      | Some T -> equal m1 m2
      | None -> false
    ;;

    let type_id =
      Type_equal.Id.create ~name:(Source_code_position.to_string [%here]) sexp_of_t
    ;;
  end

  type ('k, 'cmp) t = ('k, Packed.t, 'cmp) Map.t

  let sexp_of_t (type k) (sexp_of_k : k -> Sexp.t) =
    let module K = struct
      type t = k

      let sexp_of_t = sexp_of_k
    end
    in
    [%sexp_of: Packed.t Map.M(K).t]
  ;;

  let t_of_sexp
        (type k cmp)
        (module K : Comparator with type t = k and type comparator_witness = cmp)
        (default_models : (K.t, Packed.t, K.comparator_witness) Map.t)
        sexp
    =
    let k_to_sexp_map = [%of_sexp: Sexp.t Map.M(K).t] sexp in
    Map.merge k_to_sexp_map default_models ~f:(fun ~key:_ ->
      function
      | `Both (sexp, Packed.T { t_of_sexp; _ }) -> Some (t_of_sexp sexp)
      | `Left _sexp -> None
      | `Right default_model -> Some default_model)
  ;;
end

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | Erase_model :
        { t : ('i, 'm, 'a, 'r, 'incr, 'event) unpacked
        ; model : 'm Packed.model_info
        ; sexp_of_key : 'k -> Sexp.t
        ; key : 'k
        }
        -> ('i, Poly_model.Packed.t, 'a, 'r, 'incr, 'event) unpacked
    | Erase_action :
        { t : ('i, 'm, 'a, 'r, 'incr, 'event) unpacked
        ; action_type_id : 'a Type_equal.Id.t
        ; sexp_of_key : 'k -> Sexp.t
        ; key : 'k
        ; on_action_mismatch : on_action_mismatch
        }
        -> ('i, 'm, Case_action.t, 'r, 'incr, 'event) unpacked
    | Enum :
        { components :
            ( 'key
            , ( 'input
              , Poly_model.Packed.t
              , Case_action.t
              , 'result
              , 'incr
              , 'event )
                unpacked
            , 'cmp )
              Map.t
        ; sexp_of_key : 'key -> Sexp.t
        ; which : 'input -> 'key
        ; key_and_cmp : ('key_and_cmp, ('key, 'cmp) Poly_model.t) Type_equal.t
        }
        -> ('input, 'key_and_cmp, Case_action.t, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked
        (type i m a r incr event)
        (component : (i, m, a, r, incr, event) unpacked)
    =
    match component with
    | Enum { components; key_and_cmp = T; _ } ->
      let sexp_of_k = (Map.comparator components).sexp_of_t in
      Sexp.List
        (Sexp.Atom "Enum"
         :: (components |> Map.to_alist |> List.map ~f:[%sexp_of: k * unpacked]))
    | Erase_action { t; action_type_id = _; sexp_of_key; key; on_action_mismatch = _ } ->
      [%sexp Erase_action, (key : key), (t : unpacked)]
    | Erase_model { t; model = _; sexp_of_key; key } ->
      [%sexp Erase_model, (key : key), (t : unpacked)]
    | _ -> assert false
  ;;

  let wrap (info : _ Packed.model_info) =
    let rec t_of_sexp sexp = wrap (info.of_sexp sexp)
    and wrap m = Poly_model.Packed.T { model = m; info; t_of_sexp } in
    wrap
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
      match t with
      | Enum { components; which; key_and_cmp = T; sexp_of_key = _ } ->
        let map_model = model in
        let key = input >>| which in
        Incremental.set_cutoff
          key
          (Incremental.Cutoff.of_compare (Map.comparator components).compare);
        let%bind key = key in
        let component = Map.find_exn components key in
        let model = Incremental.map model ~f:(fun map -> Map.find_exn map key) in
        let old_model =
          match%pattern_bind old_model with
          | Some old_model -> Incremental.map old_model ~f:(fun map -> Map.find map key)
          | None -> Incremental.return incr_state None
        in
        let%map snapshot =
          eval_ext ~input ~model ~old_model ~inject ~action_type_id ~incr_state component
        and map_model = map_model in
        let apply_action ~schedule_event action =
          let new_model = Snapshot.apply_action snapshot ~schedule_event action in
          Map.set map_model ~key ~data:new_model
        in
        Snapshot.create ~apply_action ~result:(Snapshot.result snapshot)
      | Erase_model { t; model = info; sexp_of_key = _; key = _ } ->
        let unwrap_exn (type t) : t Type_equal.Id.t -> Poly_model.Packed.t -> t =
          fun model_type_id (Poly_model.Packed.T { model; info = { type_id; _ }; _ }) ->
            let T = Type_equal.Id.same_witness_exn type_id model_type_id in
            model
        in
        let wrap m = wrap info m in
        let model = model >>| unwrap_exn info.type_id in
        let old_model = old_model >>| Option.map ~f:(unwrap_exn info.type_id) in
        let%map snapshot =
          eval_ext ~old_model ~model ~inject ~action_type_id ~incr_state ~input t
        in
        let apply_action ~schedule_event a =
          let new_model = Snapshot.apply_action snapshot ~schedule_event a in
          wrap new_model
        in
        let result = Snapshot.result snapshot in
        Snapshot.create ~result ~apply_action
      | Erase_action
          { t = component
          ; action_type_id = erased_action_type_id
          ; key
          ; sexp_of_key
          ; on_action_mismatch
          } ->
        let inject action =
          inject
            (Case_action.T
               { action
               ; type_id = erased_action_type_id
               ; userdata = key
               ; sexp_of_userdata = sexp_of_key
               })
        in
        let error_message ~action_key ~current_key =
          [%message
            "Component received an action for key"
              (action_key : Sexp.t)
              "while in the key"
              (current_key : key)]
        in
        let%map model = model
        and snapshot =
          eval_ext
            ~input
            ~model
            ~old_model
            ~inject
            ~action_type_id:erased_action_type_id
            ~incr_state
            component
        in
        let apply_action ~schedule_event a =
          let (Case_action.T { action; type_id; userdata; sexp_of_userdata }) = a in
          match Type_equal.Id.same_witness type_id erased_action_type_id with
          | Some T -> Snapshot.apply_action snapshot ~schedule_event action
          | None ->
            (match on_action_mismatch with
             | `Ignore -> model
             | `Raise ->
               raise_s
                 (error_message ~action_key:([%sexp_of: userdata] userdata) ~current_key:key)
             | `Warn ->
               eprint_s
                 (error_message ~action_key:([%sexp_of: userdata] userdata) ~current_key:key);
               model)
        in
        let result = Snapshot.result snapshot in
        Snapshot.create ~result ~apply_action
      | _ -> assert false
  ;;

  module type Enum = sig
    type t [@@deriving sexp, compare, enumerate]
  end

  let enum
        (type k)
        (module E : Enum with type t = k)
        ~which
        ~(handle : k -> (_, _, _, _) Packed.t)
    =
    let module C = struct
      include E
      include Comparator.Make (E)
    end
    in
    let f key =
      let (T { unpacked; action_type_id; model }) = handle key in
      let component =
        Erase_action
          { t = Erase_model { t = unpacked; sexp_of_key = E.sexp_of_t; model; key }
          ; action_type_id
          ; key
          ; sexp_of_key = E.sexp_of_t
          ; on_action_mismatch = `Ignore
          }
      in
      let default_model = wrap model model.default in
      component, default_model
    in
    let components, models =
      List.fold
        E.all
        ~init:(Map.empty (module C), Map.empty (module C))
        ~f:(fun (component_map, model_map) key ->
          let component, model = f key in
          let component_map = Map.add_exn component_map ~key ~data:component in
          let model_map = Map.add_exn model_map ~key ~data:model in
          component_map, model_map)
    in
    let sexp_of = Poly_model.sexp_of_t E.sexp_of_t in
    let of_sexp = Poly_model.t_of_sexp (module C) models in
    let model_type_id = Type_equal.Id.create ~name:"poly-model" sexp_of in
    let model_equal = Map.equal Poly_model.Packed.equal in
    Packed.T
      { unpacked = Enum { components; which; key_and_cmp = T; sexp_of_key = E.sexp_of_t }
      ; action_type_id = Case_action.type_id
      ; model =
          { default = models
          ; type_id = model_type_id
          ; equal = model_equal
          ; sexp_of
          ; of_sexp
          }
      }
  ;;

  let if_ cond ~then_ ~else_ =
    enum
      (module Bool)
      ~which:cond
      ~handle:(function
        | true -> then_
        | false -> else_)
  ;;
end

include T

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Erase_model]

      let make (Packed.T { unpacked; action_type_id; model }) ~key ~sexp_of_key =
        Packed.T
          { unpacked = Erase_model { t = unpacked; model; key; sexp_of_key }
          ; action_type_id
          ; model =
              { type_id = Poly_model.Packed.type_id
              ; default = wrap model model.default
              ; equal = Poly_model.Packed.equal
              ; sexp_of = Poly_model.Packed.sexp_of_t
              ; of_sexp =
                  (fun _ -> assert false)
                  (* Erase_model should never have its of_sexp function called because
                     the Enum of_sexp will handle it (and bypass this of_sexp) *)
              }
          }
      ;;

      let visit
            (type i r incr event)
            (component : (i, r, incr, event) Packed.t)
            (visitor : Visitor.t)
        =
        match component with
        | Packed.T
            { unpacked = Erase_model { t; model; sexp_of_key; key }
            ; action_type_id
            ; model = _
            } ->
          let repacked = Packed.T { unpacked = t; action_type_id; model } in
          let visited = visit_ext repacked visitor in
          visitor.visit (make visited ~key ~sexp_of_key)
        | _ -> assert false
      ;;
    end)
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Erase_action]

      let make
            (Packed.T { unpacked = t; action_type_id; model })
            ~key
            ~sexp_of_key
            ~on_action_mismatch
        =
        Packed.T
          { unpacked =
              Erase_action { t; action_type_id; sexp_of_key; key; on_action_mismatch }
          ; action_type_id = Case_action.type_id
          ; model
          }
      ;;

      let visit
            (type i r incr event)
            (component : (i, r, incr, event) Packed.t)
            (visitor : Visitor.t)
        =
        match component with
        | Packed.T
            { unpacked =
                Erase_action { t; action_type_id; sexp_of_key; key; on_action_mismatch }
            ; action_type_id = _typ_id
            ; model
            } ->
          let visited = visit_ext (T { unpacked = t; action_type_id; model }) visitor in
          visitor.visit (make visited ~key ~sexp_of_key ~on_action_mismatch)
        | _ -> assert false
      ;;
    end)
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Enum]

      let visit
            (type i r incr event)
            (component : (i, r, incr, event) Packed.t)
            (visitor : Visitor.t)
        =
        match component with
        | Packed.T
            { unpacked = Enum { components; which; key_and_cmp; sexp_of_key }
            ; action_type_id = _
            ; model
            } ->
          let T = key_and_cmp in
          let unpack_exn
            :  _ Packed.t
              -> Poly_model.Packed.t
                 * (i, Poly_model.Packed.t, Case_action.t, r, incr, event) unpacked
            =
            fun (Packed.T
                   { unpacked
                   ; action_type_id = atid1
                   ; model = { default = default_model; type_id = mtid1; _ }
                   }) ->
              let T = Type_equal.Id.same_witness_exn atid1 Case_action.type_id in
              let T = Type_equal.Id.same_witness_exn mtid1 Poly_model.Packed.type_id in
              default_model, unpacked
          in
          let components_empty = Map.empty (Map.comparator_s components) in
          let models_empty = Map.empty (Map.comparator_s components) in
          let components, models =
            Map.fold
              components
              ~init:(components_empty, models_empty)
              ~f:(fun ~key ~data:unpacked (components, models) ->
                let packed =
                  Packed.T
                    { unpacked
                    ; action_type_id = Case_action.type_id
                    ; model =
                        { type_id = Poly_model.Packed.type_id
                        ; default = Map.find_exn model.default key
                        ; equal = Poly_model.Packed.equal
                        ; sexp_of = Poly_model.Packed.sexp_of_t
                        ; of_sexp =
                            (fun _ ->
                               (* of_sexp for a packed poly-model is never called *)
                               assert false)
                        }
                    }
                in
                let visited = visit_ext packed visitor in
                let model, component = unpack_exn visited in
                ( Map.add_exn components ~key ~data:component
                , Map.add_exn models ~key ~data:model ))
          in
          let repacked =
            Packed.T
              { unpacked = Enum { components; which; key_and_cmp = T; sexp_of_key }
              ; action_type_id = Case_action.type_id
              ; model = { model with default = models }
              }
          in
          visitor.visit repacked
        | _ -> assert false
      ;;
    end)
;;
