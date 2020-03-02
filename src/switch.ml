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

  let sexp_of_t (T { action; type_id; _ }) =
    let sexp_of_action = Type_equal.Id.to_sexp type_id in
    [%message "Bonsai.Switch.Case_action" (action : action)]
  ;;

  let type_id =
    Type_equal.Id.create ~name:(Source_code_position.to_string [%here]) [%sexp_of: t]
  ;;
end

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | Erase_action :
        { t : ('i, 'm, 'a, 'r, 'incr, 'event) unpacked
        ; action_type_id : 'a Type_equal.Id.t
        ; sexp_of_key : 'k -> Sexp.t
        ; key : 'k
        ; on_action_mismatch : ('k * 'm, 'm) on_action_mismatch
        }
        -> ('i, 'm, Case_action.t, 'r, 'incr, 'event) unpacked
    | Enum :
        { components :
            ( 'key
            , ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
            , 'cmp )
              Map.t
        ; which : 'input -> 'model -> 'key
        }
        -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked
        (type i m a r incr event)
        (component : (i, m, a, r, incr, event) unpacked)
    =
    match component with
    | Enum { components; which = _ } ->
      let sexp_of_k = (Map.comparator components).sexp_of_t in
      Sexp.List
        (Sexp.Atom "Enum"
         :: (components |> Map.to_alist |> List.map ~f:[%sexp_of: k * unpacked]))
    | Erase_action { t; action_type_id = _; sexp_of_key; key; on_action_mismatch = _ } ->
      [%sexp Erase_action, (key : key), (t : unpacked)]
    | _ -> assert false
  ;;

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
      match t with
      | Enum { components; which } ->
        let key =
          let%map model = model
          and input = input in
          which input model
        in
        components
        |> Map.comparator
        |> (fun c -> c.Comparator.compare)
        |> Incremental.Cutoff.of_compare
        |> Incremental.set_cutoff key;
        let%bind component = key >>| Map.find_exn components in
        eval_ext ~input ~model ~old_model ~inject ~action_type_id ~incr_state component
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
               model
             | `Custom f -> f (key, model))
        in
        let result = Snapshot.result snapshot in
        Snapshot.create ~result ~apply_action
      | _ -> assert false
  ;;

  let visit_with_same_action
        (type i m a r incr event)
        (typ_id : a Type_equal.Id.t)
        visitor
        (component : (i, m, a, r, incr, event) unpacked)
    : (i, m, a, r, incr, event) unpacked
    =
    let (T (c, typ_id2)) = visit_ext (T (component, typ_id)) visitor in
    let T = Type_equal.Id.same_witness_exn typ_id typ_id2 in
    c
  ;;

  let visit
        (type i m r incr event)
        (component : (i, m, r, incr, event) Packed.t)
        (visitor : Visitor.t)
    =
    match component with
    | Packed.T (Enum { components; which }, typ_id) ->
      let components = Map.map components ~f:(visit_with_same_action typ_id visitor) in
      Packed.T (Enum { components; which }, typ_id)
    | Packed.T
        ( Erase_action { t; action_type_id; sexp_of_key; key; on_action_mismatch }
        , _typ_id ) ->
      let (Packed.T (t, action_type_id)) = visit_ext (T (t, action_type_id)) visitor in
      Packed.T
        ( Erase_action { t; action_type_id; sexp_of_key; key; on_action_mismatch }
        , Case_action.type_id )
    | _ -> assert false
  ;;
end

include T

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Erase_action]
    end)
;;

let () =
  Component.define
    (module struct
      include T

      let extension_constructor = [%extension_constructor Enum]
    end)
;;
