open! Core
open! Import

type ('model, 'input, 'result) t =
  { input : 'input Input.t
  ; lifecycle : Lifecycle.Collection.t Incr.t option
  ; result : 'result Incr.t
  }
[@@deriving fields ~getters ~iterators:create]

let create ~here ~input ~lifecycle ~result =
  Input.iter_incremental input ~f:(annotate_packed ~here Input);
  Option.iter lifecycle ~f:(annotate ~here Lifecycle);
  annotate ~here Result result;
  Fields.create ~input ~lifecycle ~result
;;

let attribute_positions here t =
  Input.iter_incremental t.input ~f:(attribute_packed here);
  Option.iter t.lifecycle ~f:(attribute here);
  attribute here t.result
;;

let lifecycle_or_empty ~here t =
  match lifecycle t with
  | None ->
    let r = Incr.const Lifecycle.Collection.empty in
    annotate ~here Empty_lifecycle r;
    r
  | Some l -> l
;;
