open! Core
open! Import

type ('model, 'action, 'input, 'result) t =
  { input : 'input Input.t
  ; lifecycle : Lifecycle.Collection.t Incr.t option
  ; result : 'result Incr.t
  }
[@@deriving fields]

let create ~input ~lifecycle ~result =
  Input.iter_incremental input ~f:(annotate_packed Input);
  Option.iter lifecycle ~f:(annotate Lifecycle);
  annotate Result result;
  Fields.create ~input ~lifecycle ~result
;;

let attribute_positions here t =
  Input.iter_incremental t.input ~f:(attribute_packed here);
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
