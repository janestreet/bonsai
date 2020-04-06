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
  Bonsai.state_machine
    (module Model)
    (module Action)
    [%here]
    ~default_model:Model.default
    ~apply_action:(fun ~inject:_ ~schedule_event:_ _ model -> function
      | Action.Add ->
        let key = model.Model.count in
        { data = Map.add_exn model.data ~key ~data:(); count = key + 1 }
      | Action.Remove key -> { model with data = Map.remove model.Model.data key })
;;

let add_remove component ~wrap_remove =
  let open Bonsai.Arrow in
  let open Bonsai.Let_syntax in
  let associated = Bonsai.Map.assoc_input (module Int) component in
  let%map results, inject_action =
    state_component >>> first (Model.data @>> associated)
  in
  let data =
    results
    |> Map.mapi ~f:(fun ~key ~data ->
      let inject_remove = inject_action (Action.Remove key) in
      wrap_remove data inject_remove)
    |> Map.data
  in
  data, inject_action Action.Add
;;

let add_remove_vdom component =
  let open Vdom in
  let%map.Bonsai views, add_event =
    add_remove component ~wrap_remove:(fun view remove_event ->
      Node.div
        []
        [ Node.button [ Attr.on_click (fun _ -> remove_event) ] [ Node.text "X" ]
        ; view
        ])
  in
  Node.div
    []
    (Node.button [ Attr.on_click (fun _ -> add_event) ] [ Node.text "Add" ] :: views)
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    (add_remove_vdom Bonsai_web_counters_example.single_counter)
    ~initial_input:()
    ~bind_to_element_with_id:"app"
;;

