open! Core
open! Import

type ('model, 'action, 'result) t =
  { apply_action : ('model, 'action) Apply_action.t
  ; lifecycle : Lifecycle.Collection.t Incr.t option
  ; result : 'result Incr.t
  }
[@@deriving fields]

let create ~apply_action ~lifecycle ~result =
  (match (apply_action : _ Apply_action.t) with
   | Incremental apply_action -> annotate Apply_action apply_action
   | Join { incr; _ } -> annotate Apply_action incr
   | Impossible _ -> ());
  Option.iter lifecycle ~f:(annotate Lifecycle);
  annotate Result result;
  Fields.create ~apply_action ~lifecycle ~result
;;

let attribute_positions here t =
  (match t.apply_action with
   | Join { incr; _ } -> attribute here incr
   | Incremental apply_action -> attribute here apply_action
   | Impossible _ -> ());
  Option.iter t.lifecycle ~f:(attribute here);
  attribute here t.result
;;

let lifecycle_or_empty t =
  match lifecycle t with
  | None ->
    let r = Incr.const Lifecycle.Collection.empty in
    annotate Empty_lifecycle r;
    r
  | Some l -> l
;;
