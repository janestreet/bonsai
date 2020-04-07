open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component

module type Enum = sig
  type t [@@deriving compare, enumerate, sexp]
end

module Case_action = struct
  type 'key t =
    | T :
        { action : 'a
        ; type_id : 'a Type_equal.Id.t
        ; key : 'key
        }
        -> 'key t

  let sexp_of_t sexp_of_key (T { action; type_id; key }) =
    let sexp_of_action = Type_equal.Id.to_sexp type_id in
    [%message "Bonsai.Switch.Case_action" (action : action) (key : key)]
  ;;

  let type_id sexp_of_key =
    Type_equal.Id.create ~name:(Source_code_position.to_string [%here]) [%sexp_of: key t]
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
    | Enum :
        { components : ('key, ('input, 'result, 'incr, 'event) Packed.t, 'cmp) Map.t
        ; sexp_of_key : 'key -> Sexp.t
        ; key_equal : 'key -> 'key -> bool
        ; which : 'input -> 'key
        ; key_and_cmp : ('key_and_cmp, ('key, 'cmp) Poly_model.t) Type_equal.t
        }
        -> ('input, 'key_and_cmp, 'key Case_action.t, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked
        (type i m a r incr event)
        (component : (i, m, a, r, incr, event) unpacked)
    =
    match component with
    | Enum { components; key_and_cmp = T; _ } ->
      let sexp_of_k = (Map.comparator components).sexp_of_t in
      Sexp.List
        (Sexp.Atom "Enum"
         :: (components
             |> Map.to_alist
             |> List.map ~f:(fun (k, Packed.T { unpacked; _ }) ->
               [%sexp_of: k * unpacked] (k, unpacked))))
    | _ -> assert false
  ;;

  let wrap (info : _ Packed.model_info) =
    let rec t_of_sexp sexp = wrap (info.of_sexp sexp)
    and wrap m = Poly_model.Packed.T { model = m; info; t_of_sexp } in
    wrap
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id:_ ~incr_state t ->
      match t with
      | Enum { components; which; key_and_cmp = T; sexp_of_key = _; key_equal } ->
        let map_model = model in
        let key = input >>| which in
        Incremental.set_cutoff
          key
          (Incremental.Cutoff.of_compare (Map.comparator components).compare);
        let%bind key = key in
        let (Packed.T { unpacked; model = model_info; action_type_id }) =
          Map.find_exn components key
        in
        let model =
          Incremental.map model ~f:(fun map ->
            let (Poly_model.Packed.T { model; info; t_of_sexp = _ }) =
              Map.find_exn map key
            in
            let t = Type_equal.Id.same_witness_exn info.type_id model_info.type_id in
            Type_equal.conv t model)
        in
        let old_model =
          match%pattern_bind old_model with
          | Some old_model ->
            Incremental.map old_model ~f:(fun map ->
              let (Poly_model.Packed.T { model; info; t_of_sexp = _ }) =
                Map.find_exn map key
              in
              let t = Type_equal.Id.same_witness_exn info.type_id model_info.type_id in
              Some (Type_equal.conv t model))
          | None -> Incremental.return incr_state None
        in
        let inject action =
          inject (Case_action.T { action; type_id = action_type_id; key })
        in
        let%map snapshot =
          eval_ext ~input ~model ~old_model ~inject ~action_type_id ~incr_state unpacked
        and map_model = map_model in
        let apply_action ~schedule_event (Case_action.T { action; type_id; key = key' }) =
          match key_equal key' key, Type_equal.Id.same_witness type_id action_type_id with
          | true, Some T ->
            let new_model = Snapshot.apply_action snapshot ~schedule_event action in
            let new_model = wrap model_info new_model in
            Map.set map_model ~key:key' ~data:new_model
          | _ -> map_model
        in
        Snapshot.create ~apply_action ~result:(Snapshot.result snapshot)
      | _ -> assert false
  ;;

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
      let component = handle key in
      let (T { model; unpacked = _; action_type_id = _ }) = component in
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
      { unpacked =
          Enum
            { components
            ; which
            ; key_and_cmp = T
            ; sexp_of_key = E.sexp_of_t
            ; key_equal = [%compare.equal: E.t]
            }
      ; action_type_id = Case_action.type_id E.sexp_of_t
      ; model =
          { default = models
          ; type_id = model_type_id
          ; equal = model_equal
          ; sexp_of
          ; of_sexp
          }
      }
  ;;

  let extension_constructor = [%extension_constructor Enum]

  let visit (Packed.T { unpacked; model; action_type_id }) visitor =
    match unpacked with
    | Enum { components; which; key_and_cmp = T; sexp_of_key; key_equal } ->
      let components = Map.map components ~f:(fun c -> visit_ext c visitor) in
      let models =
        Map.map components ~f:(fun (Packed.T { model; _ }) -> wrap model model.default)
      in
      let repacked =
        Packed.T
          { unpacked =
              Enum { components; which; key_and_cmp = T; sexp_of_key; key_equal }
          ; action_type_id
          ; model = { model with default = models }
          }
      in
      visitor.visit repacked
    | _ -> assert false
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

let () = Component.define (module T)
