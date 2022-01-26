open! Core
open! Import

type ('m, 'a) transition = schedule_event:(unit Effect.t -> unit) -> 'm -> 'a -> 'm

type ('m, 'a) t =
  | Incremental : ('m, 'a) transition Incr.t -> ('m, 'a) t
  | Impossible : ('a -> Nothing.t) -> (_, 'a) t
  | Join :
      { incr : ('m1, 'a1) transition Incr.t
      ; witness : 'a2 -> Nothing.t
      ; map_model : 'm -> 'm1 * 'm2
      ; unmap_model : 'm1 * 'm2 -> 'm
      ; map_action : 'a -> ('a1, 'a2) Either.t
      }
      -> ('m, 'a) t

let iter_incremental t ~f =
  match t with
  | Incremental incr -> f (Incr.pack incr)
  | Join { incr; _ } -> f (Incr.pack incr)
  | Impossible _ -> ()
;;

let incremental i = Incremental i
let impossible = Impossible Fn.id

let join t1 t2 ~schedule_event (m1, m2) action =
  match action with
  | First action1 -> t1 ~schedule_event m1 action1, m2
  | Second action2 -> m1, t2 ~schedule_event m2 action2
;;

let join_witness t1 t2 action =
  match action with
  | First action1 -> t1 action1
  | Second action2 -> t2 action2
;;

let to_incremental = function
  | Incremental i -> i
  | Impossible witness ->
    Incr.return (fun ~schedule_event:_ _model action ->
      Nothing.unreachable_code (witness action))
  | Join { incr; witness; map_model; map_action; unmap_model } ->
    let%map.Incr incr = incr in
    fun ~schedule_event model action ->
      (match map_action action with
       | First a ->
         let m1, m2 = map_model model in
         unmap_model (incr ~schedule_event m1 a, m2)
       | Second n -> Nothing.unreachable_code (witness n))
;;

let rec map t ~f =
  match t with
  | Impossible witness ->
    (fun ~schedule_event:_ _m a -> Nothing.unreachable_code (witness a))
    |> Incr.return
    |> Incremental
    |> map ~f
  | Incremental i -> Incremental (Incr.map i ~f)
  | Join _ -> to_incremental t |> Incremental |> map ~f
;;

let merge t1 t2 =
  match t1, t2 with
  | Impossible a, Impossible b ->
    Impossible
      (function
        | First x -> a x
        | Second y -> b y)
  | Incremental t1, Impossible witness ->
    Join
      { incr = t1; witness; map_model = Fn.id; unmap_model = Fn.id; map_action = Fn.id }
  | Impossible witness, Incremental t2 ->
    Join
      { incr = t2
      ; witness
      ; map_model = Tuple2.swap
      ; unmap_model = Tuple2.swap
      ; map_action = Either.swap
      }
  | Join t1, Join t2 ->
    let map_model (a, b) =
      let (i1, i2), (s1, s2) = t1.map_model a, t2.map_model b in
      (i1, s1), (i2, s2)
    in
    let unmap_model ((i1, i2), (s1, s2)) =
      t1.unmap_model (i1, s1), t2.unmap_model (i2, s2)
    in
    let map_action = function
      | First a ->
        (match t1.map_action a with
         | First a -> First (First a)
         | Second a -> Second (First a))
      | Second a ->
        (match t2.map_action a with
         | First a -> First (Second a)
         | Second a -> Second (Second a))
    in
    Join
      { incr = Incr.map2 ~f:join t1.incr t2.incr
      ; witness = join_witness t1.witness t2.witness
      ; map_model
      ; unmap_model
      ; map_action
      }
  | Join { incr; witness; map_model; unmap_model; map_action }, Incremental t2 ->
    let map_model (conjoined, new_) =
      let left, right = map_model conjoined in
      (left, new_), right
    in
    let unmap_model ((left, new_), right) =
      let conjoined = unmap_model (left, right) in
      conjoined, new_
    in
    let map_action = function
      | First conjoined ->
        (match map_action conjoined with
         | First a -> First (First a)
         | Second b -> Second b)
      | Second new_ -> First (Second new_)
    in
    Join { incr = Incr.map2 ~f:join incr t2; witness; map_model; unmap_model; map_action }
  | Join { incr; witness; map_model; unmap_model; map_action }, Impossible t2 ->
    let map_model (conjoined, new_) =
      let left, right = map_model conjoined in
      left, (right, new_)
    in
    let unmap_model (left, (right, new_)) =
      let conjoined = unmap_model (left, right) in
      conjoined, new_
    in
    let map_action = function
      | Second a -> Second (Second a)
      | First conjoined ->
        (match map_action conjoined with
         | First a -> First a
         | Second b -> Second (First b))
    in
    Join { incr; witness = join_witness witness t2; map_model; unmap_model; map_action }
  | Incremental t2, Join { incr; witness; map_model; unmap_model; map_action } ->
    let map_model (new_, conjoined) =
      let left, right = map_model conjoined in
      (new_, left), right
    in
    let unmap_model ((new_, left), right) =
      let conjoined = unmap_model (left, right) in
      new_, conjoined
    in
    let map_action = function
      | Second conjoined ->
        (match map_action conjoined with
         | First a -> First (Second a)
         | Second b -> Second b)
      | First new_ -> First (First new_)
    in
    Join { incr = Incr.map2 ~f:join t2 incr; witness; map_model; unmap_model; map_action }
  | Impossible t2, Join { incr; witness; map_model; unmap_model; map_action } ->
    let map_model (new_, conjoined) =
      let left, right = map_model conjoined in
      left, (new_, right)
    in
    let unmap_model (left, (new_, right)) =
      let conjoined = unmap_model (left, right) in
      new_, conjoined
    in
    let map_action = function
      | Second conjoined ->
        (match map_action conjoined with
         | First a -> First a
         | Second b -> Second (Second b))
      | First new_ -> Second (First new_)
    in
    Join { incr; witness = join_witness t2 witness; map_model; unmap_model; map_action }
  | Incremental t1, Incremental t2 -> Incremental (Incr.map2 t1 t2 ~f:join)
;;
