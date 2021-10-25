open! Core
open Bonsai.Let_syntax
module Id = Int

type 'a t =
  { contents : 'a Id.Map.t
  ; append : unit Ui_effect.t
  ; set_length : int -> unit Ui_effect.t
  ; remove : Id.t -> unit Ui_effect.t
  }

module Model = struct
  type t =
    { data : unit Id.Map.t
    ; count : int
    }
  [@@deriving fields, equal, sexp]

  let default = { data = Int.Map.empty; count = 0 }

  let add_one model =
    let key = model.count in
    { data = Map.add_exn model.data ~key ~data:(); count = key + 1 }
  ;;

  let remove model ~key = { model with data = Map.remove model.data key }
end

module Action = struct
  type t =
    | Add of { how_many : int }
    | Remove of int
  [@@deriving sexp_of]
end

let state_component here =
  Bonsai.state_machine0
    here
    (module Model)
    (module Action)
    ~default_model:Model.default
    ~apply_action:
      (fun ~inject:_ ~schedule_event:_ (model : Model.t) -> function
         | Add { how_many } -> Fn.apply_n_times ~n:how_many Model.add_one model
         | Remove key -> Model.remove model ~key)
;;

let component' here t ~wrap_remove =
  let%sub { Model.data; count = _ }, inject_action = state_component here in
  let%sub map =
    Bonsai.assoc
      (module Int)
      data
      ~f:(fun key _data ->
        (* Model-resetter allows assoc to reclaim space after a node has been removed *)
        let%sub result = Bonsai.with_model_resetter t in
        return
        @@ let%map out, reset = result
        and key = key
        and inject_action = inject_action in
        let inject_remove =
          Ui_effect.Many [ reset; inject_action (Action.Remove key) ]
        in
        out, inject_remove)
  in
  let%sub contents_map =
    Bonsai.Incr.compute map ~f:(fun map ->
      Incr_map.map map ~f:(Tuple2.uncurry wrap_remove))
  in
  return
  @@ let%map contents = contents_map
  and map = map
  and inject_action = inject_action in
  let append = inject_action (Action.Add { how_many = 1 }) in
  let remove id = inject_action (Action.Remove id) in
  let set_length length =
    let difference = length - Map.length map in
    match Int.sign difference with
    | Zero -> Ui_effect.Ignore
    | Pos -> inject_action (Action.Add { how_many = difference })
    | Neg ->
      map
      |> Map.data
      |> List.rev_map ~f:Tuple2.get2
      |> Fn.flip List.take (-difference)
      |> Ui_effect.Many
  in
  { contents; append; set_length; remove }
;;

let component here t = component' here t ~wrap_remove:(fun a _ -> a)
