open! Core_kernel
open Bonsai_web

module Model = struct
  type t =
    { data : unit Int.Map.t
    ; count : int
    }
  [@@deriving fields, equal, sexp]

  let default = { data = Int.Map.empty; count = 0 }
end

module Action = struct
  type t =
    | Add
    | Remove of int
  [@@deriving sexp_of]
end

let state_component =
  Bonsai.state_machine0
    [%here]
    (module Model)
    (module Action)
    ~default_model:Model.default
    ~apply_action:(fun ~inject:_ ~schedule_event:_ model -> function
      | Action.Add ->
        let key = model.Model.count in
        { data = Map.add_exn model.data ~key ~data:(); count = key + 1 }
      | Action.Remove key -> { model with data = Map.remove model.Model.data key })
;;

let add_remove component ~wrap_remove =
  let open Bonsai.Let_syntax in
  let%sub state = state_component in
  let%pattern_bind { Model.data; count = _ }, inject_action = state in
  let%sub results =
    Bonsai.assoc
      (module Int)
      data
      ~f:(fun _key _data -> Bonsai.with_model_resetter component)
  in
  return
  @@ let%map results = results
  and inject_action = inject_action in
  let data =
    results
    |> Map.mapi ~f:(fun ~key ~data:(data, revert_event) ->
      let inject_remove =
        Vdom.Event.Many [ revert_event; inject_action (Action.Remove key) ]
      in
      wrap_remove data inject_remove)
    |> Map.data
  in
  data, inject_action Action.Add
;;

let add_remove_vdom component =
  let open Bonsai.Let_syntax in
  let open Vdom in
  let%sub views_and_add_event =
    add_remove component ~wrap_remove:(fun view remove_event ->
      Node.div
        []
        [ Node.button [ Attr.on_click (fun _ -> remove_event) ] [ Node.text "X" ]
        ; view
        ])
  in
  return
  @@ let%map views, add_event = views_and_add_event in
  Node.div
    []
    (Node.button [ Attr.on_click (fun _ -> add_event) ] [ Node.text "Add" ] :: views)
;;

let (_ : _ Start.Handle.t) =
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (add_remove_vdom Bonsai_web_counters_example.single_counter)
;;

