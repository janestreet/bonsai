open! Core
open Bonsai_web
open Bonsai.Let_syntax
module Id = Int

type 'a t =
  { contents : 'a Id.Map.t
  ; append : unit Effect.t
  ; set_length : int -> unit Effect.t
  ; remove : Id.t -> unit Effect.t
  }

module Model = struct
  type t =
    { data : unit Id.Map.t
    ; count : int
    }
  [@@deriving equal, sexp]

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

let state_component =
  Bonsai.state_machine0
    ()
    ~sexp_of_model:[%sexp_of: Model.t]
    ~equal:[%equal: Model.t]
    ~sexp_of_action:[%sexp_of: Action.t]
    ~default_model:Model.default
    ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) (model : Model.t) -> function
    | Add { how_many } -> Fn.apply_n_times ~n:how_many Model.add_one model
    | Remove key -> Model.remove model ~key)
;;

let component' t ~wrap_remove =
  let%sub { Model.data; count = _ }, inject_action = state_component in
  let%sub map =
    Bonsai.assoc
      (module Int)
      data
      ~f:(fun key _data ->
        (* Model-resetter allows assoc to reclaim space after a node has been removed *)
        let%sub result = Bonsai.with_model_resetter t in
        let%arr out, reset = result
        and key = key
        and inject_action = inject_action in
        let inject_remove = Effect.Many [ reset; inject_action (Action.Remove key) ] in
        out, inject_remove)
  in
  let%sub contents_map =
    Bonsai.Incr.compute map ~f:(fun map ->
      Incr_map.map map ~f:(Tuple2.uncurry wrap_remove))
  in
  let%arr contents = contents_map
  and map = map
  and inject_action = inject_action in
  let append = inject_action (Action.Add { how_many = 1 }) in
  let remove id = inject_action (Action.Remove id) in
  let set_length length =
    let difference = length - Map.length map in
    match Int.sign difference with
    | Zero -> Effect.Ignore
    | Pos -> inject_action (Action.Add { how_many = difference })
    | Neg ->
      let effects_in_map = List.rev_map (Map.data map) ~f:snd in
      Effect.Many (List.take effects_in_map (-difference))
  in
  { contents; append; set_length; remove }
;;

let component t = component' t ~wrap_remove:(fun a _ -> a)
