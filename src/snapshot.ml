open! Core
open! Import

module Apply_action = struct
  type ('m, 'a) transition = schedule_event:(unit Effect.t -> unit) -> 'm -> 'a -> 'm

  type ('m, 'a) t =
    | Incremental of ('m, 'a) transition Incr.t
    | Non_incremental of ('m, 'a) transition
    | Impossible of ('a -> Nothing.t)

  let incremental i = Incremental i
  let non_incremental i = Non_incremental i
  let impossible = Impossible Fn.id

  let to_incremental = function
    | Incremental i -> i
    | Non_incremental i -> Incr.return i
    | Impossible witness ->
      Incr.return (fun ~schedule_event:_ _model action ->
        Nothing.unreachable_code (witness action))
  ;;

  let rec map t ~f =
    match t with
    | Impossible witness ->
      Non_incremental (fun ~schedule_event:_ _m a -> Nothing.unreachable_code (witness a))
      |> map ~f
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
    | Impossible a, Impossible b ->
      Impossible
        (function
          | First x -> a x
          | Second y -> b y)
    | Impossible a, Non_incremental b ->
      let a ~schedule_event:_ _model action = Nothing.unreachable_code (a action) in
      Non_incremental (join a b)
    | Non_incremental a, Impossible b ->
      let b ~schedule_event:_ _model action = Nothing.unreachable_code (b action) in
      Non_incremental (join a b)
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

let annotate ~name ~color t =
  Incr.append_user_info_graphviz
    t
    ~label:[ name ]
    ~attrs:(String.Map.of_alist_exn [ "style", "filled"; "fillcolor", color ])
;;

let create ~apply_action ~lifecycle ~result =
  (match apply_action with
   | Apply_action.Incremental apply_action ->
     annotate ~name:"apply_action" ~color:"cornsilk" apply_action
   | Non_incremental _ -> ()
   | Impossible _ -> ());
  Option.iter lifecycle ~f:(annotate ~name:"lifecycle" ~color:"lightsalmon");
  annotate ~name:"result" ~color:"lightcoral" result;
  Fields.create ~apply_action ~lifecycle ~result
;;

let lifecycle_or_empty t =
  lifecycle t |> Option.value ~default:(Incr.return Lifecycle.Collection.empty)
;;
