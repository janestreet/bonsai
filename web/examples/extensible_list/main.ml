open! Core_kernel
open Bonsai_web

let counter_component =
  let bump_counter ~inject:_ ~schedule_event:_ () model action = model + action in
  let%map.Bonsai inject_bump =
    Bonsai.Model.state_machine
      [%here]
      ~sexp_of_action:[%sexp_of: int]
      ~apply_action:bump_counter
  and model = Bonsai.model in
  Vdom.Node.div
    []
    [ Vdom.Node.button
        [ Vdom.Attr.on_click (fun _ -> inject_bump (-1)) ]
        [ Vdom.Node.text "-1" ]
    ; Vdom.Node.textf "%d" model
    ; Vdom.Node.button
        [ Vdom.Attr.on_click (fun _ -> inject_bump 1) ]
        [ Vdom.Node.text "+1" ]
    ]
;;

module Model = struct
  type 'a t =
    { data : 'a Int.Map.t
    ; count : int
    }
  [@@deriving fields]

  let init ?initial () =
    let data =
      match initial with
      | None -> Int.Map.empty
      | Some initial -> initial |> List.mapi ~f:(fun i v -> i, v) |> Int.Map.of_alist_exn
    in
    { data; count = Map.length data }
  ;;
end

module Action = struct
  type t =
    | Add
    | Remove of int
  [@@deriving sexp_of]
end

let state_component default =
  Bonsai.Model.state_machine
    [%here]
    ~sexp_of_action:[%sexp_of: Action.t]
    ~apply_action:(fun ~inject:_ ~schedule_event:_ _ model ->
      function
      | Action.Add ->
        let key = model.Model.count in
        { data = Map.add_exn model.data ~key ~data:default; count = key + 1 }
      | Action.Remove key -> { model with data = Map.remove model.Model.data key })
;;

let add_remove component ~wrap_remove ~default_model =
  let%map.Bonsai inject_modifications = state_component default_model
  and data = Bonsai.Map.assoc_model component |> Bonsai.Model.field Model.Fields.data in
  let data =
    Map.to_alist data
    |> List.map ~f:(fun (key, view) ->
      wrap_remove view (inject_modifications (Action.Remove key)))
  in
  let inject_add = inject_modifications Action.Add in
  data, inject_add
;;

let add_remove_vdom component ~default_model =
  let open Vdom in
  let%map.Bonsai views, add_event =
    add_remove component ~default_model ~wrap_remove:(fun view remove_event ->
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
    (add_remove_vdom counter_component ~default_model:0)
    ~initial_input:()
    ~initial_model:(Model.init ~initial:[ 1; 2; 3; 4 ] ())
    ~bind_to_element_with_id:"app"
;;

