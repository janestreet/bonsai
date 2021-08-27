open! Core
open! Import

module Apply_action = struct
  type ('m, 'a) transition = schedule_event:(unit Effect.t -> unit) -> 'm -> 'a -> 'm

  type ('m, 'a) t =
    | Incremental of ('m, 'a) transition Incr.t
    | Non_incremental of ('m, 'a) transition

  let incremental i = Incremental i
  let non_incremental i = Non_incremental i

  let to_incremental = function
    | Incremental i -> i
    | Non_incremental i -> Incr.return i
  ;;

  let map t ~f =
    match t with
    | Incremental i ->
      Incremental
        (let%map.Incr i = i in
         f i)
    | Non_incremental i -> Non_incremental (f i)
  ;;

  let merge t1 t2 =
    let join t1 t2 ~schedule_event (m1, m2) action =
      match action with
      | First action1 -> t1 ~schedule_event m1 action1, m2
      | Second action2 -> m1, t2 ~schedule_event m2 action2
    in
    match t1, t2 with
    | Non_incremental t1, Non_incremental t2 -> Non_incremental (join t1 t2)
    | _ ->
      Incremental
        (let%map.Incr t1 = to_incremental t1
         and t2 = to_incremental t2 in
         join t1 t2)
  ;;
end

type ('model, 'action, 'result) t =
  { apply_action : ('model, 'action) Apply_action.t
  ; lifecycle : Lifecycle.Collection.t Incr.t option
  ; result : 'result Incr.t
  }
[@@deriving fields]

let create = Fields.create

let lifecycle_or_empty t =
  lifecycle t |> Option.value ~default:(Incr.return Lifecycle.Collection.empty)
;;
