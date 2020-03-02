open! Core_kernel
open! Import
open Incremental.Let_syntax
open Component
open Composition_infix

(* {v
                  outer             outer'
                    |                 ^
                    |                 |
               get  |            set  +----- outer
                    |                 |
                    v                 |
                  inner             inner'
         v} *)
type ('outer, 'inner) t =
  { get : 'outer -> 'inner
  ; set : 'outer -> 'inner -> 'outer
  }

(* {v
                     outer             outer'
                       |                 ^
                       |                 |
               p1.get  |         p1.set  +----- outer
                       |                 |
                       v                 |
                      mid               mid'
                       |                 ^
                       |                 |
               p2.get  |         p2.set  +----- mid
                       |                 |
                       v                 |
                     inner             inner'
        v} *)
let compose p1 p2 =
  { set =
      (fun outer inner ->
         let mid = p1.get outer in
         p1.set outer (p2.set mid inner))
  ; get = p1.get >> p2.get
  }
;;

module T = struct
  type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
    | C :
        { t : ('input, 'm1, 'action, 'result, 'incr, 'event) unpacked
        ; projection : ('m2, 'm1) t
        }
        -> ('input, 'm2, 'action, 'result, 'incr, 'event) unpacked

  let sexp_of_unpacked (type i m a r) (component : (i, m, a, r, _, _) unpacked) =
    match component with
    | C { t; projection = _ } -> [%sexp Projection (t : unpacked)]
    | _ -> assert false
  ;;

  let extension_constructor = [%extension_constructor C]

  let eval (type i m a r incr event) : (i, m, a, r, incr, event) eval_type =
    fun ~input ~old_model ~model ~inject ~action_type_id ~incr_state t ->
    match t with
    | C { t; projection } ->
      let model_inner = Incremental.map model ~f:projection.get in
      let old_model_inner =
        Incremental.map old_model ~f:(Option.map ~f:projection.get)
      in
      let%map snapshot =
        Component.eval_ext
          t
          ~input
          ~old_model:old_model_inner
          ~model:model_inner
          ~inject
          ~action_type_id
          ~incr_state
      and model = model in
      let result = Snapshot.result snapshot in
      let apply_action ~schedule_event a =
        let inner_model = Snapshot.apply_action snapshot ~schedule_event a in
        projection.set model inner_model
      in
      Snapshot.create ~result ~apply_action
    | _ -> assert false
  ;;

  let visit component visitor =
    match component with
    | Packed.T (C { t; projection }, typ_id) ->
      let (T (t, typ_id)) = visit_ext (T (t, typ_id)) visitor in
      visitor.visit (T (C { t; projection }, typ_id))
    | _ -> assert false
  ;;
end

include T

let () = Component.define (module T)
