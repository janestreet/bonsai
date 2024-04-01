open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
open! Vdom
module Style = Todomvc

(* Module for getting a Value.t tracking the global URL fragment. Best practice is usually
   to use Bonsai_web_ui_url_var. As of this writing, that module does not support tracking
   URL Fragments. As a lightweight workaround, I created the Url_hash module. *)
module Url_hash : sig
  val get : unit -> string Bonsai.t
end = struct
  (* is_watching ensures we only add one event listener over the lifetime of the app. *)
  let is_watching = ref false
  let hash_var = Bonsai.Expert.Var.create "/"
  let hash_val = Bonsai.Expert.Var.value hash_var

  let on_hash_change (f : string -> unit) =
    let open Js_of_ocaml in
    let handler =
      Dom.handler (fun _ ->
        let hash = Dom_html.window##.location##.hash |> Js.to_string in
        f hash;
        Js.bool false)
    in
    let capture = Js.bool false in
    Dom.addEventListener Dom_html.window Dom_html.Event.hashchange handler capture
  ;;

  let get () =
    if not !is_watching
    then (
      let _discarded_id = on_hash_change (Bonsai.Expert.Var.set hash_var) in
      is_watching := true);
    hash_val
  ;;
end

module Dom_helpers = struct
  let get_target_value evt : string =
    let open Js_of_ocaml.Js in
    Unsafe.get (Unsafe.get evt "target") "value" |> to_string
  ;;

  let filtered_attrs (cx : (Attr.t * bool) list) =
    List.filter_map cx ~f:(fun (str, b) -> Option.some_if b str) |> Attr.many
  ;;
end

module Model = struct
  type todo =
    { id : int
    ; title : string
    ; completed : bool
    }
  [@@deriving sexp, equal]

  type t = todo Int.Map.t [@@deriving sexp, equal]
end

(* Persist the model to local storage. Used for hydration. *)
let persisted_model =
  Bonsai_web.Persistent_var.create
    (module Model)
    `Local_storage
    ~unique_id:"todos-bonsai"
    ~default:Int.Map.empty
;;

module Action = struct
  type t =
    | New_todo of string
    | Delete_todo of int
    | Edit_todo_title of (int * string)
    | Complete_todo of int
    | Uncomplete_todo of int
    | Clear_completed
    | Toggle_all of { active_todo_ids : Int.Set.t }
  [@@deriving sexp]
end

(* Apply state transitions. Also persist state to localStorage on each modification. *)
let apply_action context (model : Model.t) (action : Action.t) =
  let warn ~id message = eprint_s [%message message "for item" ~_:(id : int)] in
  let new_model : Model.t =
    match action with
    | New_todo title ->
      let id =
        Map.max_elt model |> Option.value_map ~f:(fun (id, _) -> id + 1) ~default:0
      in
      Map.set model ~key:id ~data:{ title; id; completed = false }
    | Delete_todo id -> Map.remove model id
    | Complete_todo id ->
      Map.change model id ~f:(function
        | None ->
          warn ~id "Attempt to complete nonexistent todo";
          None
        | Some todo -> Some { todo with completed = true })
    | Uncomplete_todo id ->
      Map.change model id ~f:(function
        | None ->
          warn ~id "Attempt to uncomplete nonexistent todo";
          None
        | Some todo -> Some { todo with completed = false })
    | Edit_todo_title (id, title) ->
      Map.change model id ~f:(function
        | None ->
          warn ~id "Attempt to edit nonexistent todo";
          None
        | Some todo -> Some { todo with title })
    | Clear_completed -> Map.filter model ~f:(fun v -> not v.completed)
    | Toggle_all { active_todo_ids } ->
      let exists_incompleted_todos =
        Map.existsi model ~f:(fun ~key ~data:{ completed; _ } ->
          Set.mem active_todo_ids key && not completed)
      in
      let set_subset_completed_to value =
        Map.mapi model ~f:(fun ~key ~data:item ->
          if Set.mem active_todo_ids key then { item with completed = value } else item)
      in
      if exists_incompleted_todos
      then set_subset_completed_to true
      else set_subset_completed_to false
  in
  let set_persisted_model = Bonsai_web.Persistent_var.effect persisted_model in
  Bonsai.Apply_action_context.schedule_event context (set_persisted_model new_model);
  new_model
;;

let header_component ~inject graph =
  let state, set_state =
    Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t] graph
  in
  let%arr state = state
  and set_state = set_state
  and inject = inject in
  let custom_input =
    let handle_enter evt =
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event evt with
      | Enter -> Effect.Many [ set_state ""; inject (Action.New_todo state) ]
      | _ -> Effect.Ignore
    in
    let attr =
      Attr.many
        [ Style.new_todo
        ; Attr.value_prop state
        ; Attr.on_input (fun _ input -> set_state input)
        ; Attr.placeholder "What needs to be done?"
        ; Attr.autofocus true
        ; Attr.on_keydown handle_enter
        ]
    in
    Node.input ~attrs:[ attr ] ()
  in
  Node.header [ Node.h1 [ Node.text "todos" ]; custom_input ]
;;

let todo_item_component
  (todo : Model.todo Bonsai.t)
  ~(inject : (Action.t -> unit Effect.t) Bonsai.t)
  graph
  =
  let editing, set_editing =
    Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t] graph
  in
  let%arr inject = inject
  and todo = todo
  and editing = editing
  and set_editing = set_editing in
  let is_complete_checkbox =
    let handle_clicked _ _ =
      if todo.completed
      then inject (Uncomplete_todo todo.id)
      else inject (Complete_todo todo.id)
    in
    let attr =
      Attr.many
        [ Dom_helpers.filtered_attrs [ Style.toggle, true; Style.checked, todo.completed ]
        ; Attr.type_ "checkbox"
        ; Attr.on_change handle_clicked
        ; Attr.bool_property "checked" todo.completed
        ]
    in
    Node.input ~attrs:[ attr ] ()
  in
  let task_name_input =
    let handle_focus_leave evt =
      let value = Dom_helpers.get_target_value evt in
      Effect.Many [ inject (Edit_todo_title (todo.id, value)); set_editing false ]
    in
    let handle_enter evt =
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event evt with
      | Enter ->
        let value = Dom_helpers.get_target_value evt in
        Effect.Many [ inject (Edit_todo_title (todo.id, value)); set_editing false ]
      | _ -> Effect.Ignore
    in
    let attr =
      Attr.many
        [ Attr.value todo.title
        ; Style.edit
        ; Attr.on_blur handle_focus_leave
        ; Attr.on_keydown handle_enter
        ]
    in
    Node.input ~attrs:[ attr ] ()
  in
  let view =
    let remove_task_button =
      let on_click _ = inject (Delete_todo todo.id) in
      let attr = Attr.many [ Style.destroy; Attr.on_click on_click ] in
      Node.button ~attrs:[ attr ] []
    in
    let on_double_click _ = set_editing (not editing) in
    Node.div
      ~attrs:[ Style.view ]
      [ is_complete_checkbox
      ; Node.label
          ~attrs:[ Attr.on_double_click on_double_click ]
          [ Node.text todo.title ]
      ; remove_task_button
      ]
  in
  Node.li
    ~key:(Int.to_string todo.id)
    ~attrs:
      [ Dom_helpers.filtered_attrs
          [ Style.editing, editing; Style.completed, todo.completed ]
      ]
    [ view; task_name_input ]
;;

let todo_list (model : Model.t Bonsai.t) ~inject graph =
  let filtered_model =
    let%map model = model
    and hash = Url_hash.get () in
    Map.filter model ~f:(fun todo ->
      match hash with
      | "#/completed" -> todo.completed
      | "#/active" -> not todo.completed
      | _ -> true)
  in
  let active_count =
    let%map model = model in
    Map.count model ~f:(fun todo -> not todo.completed)
  in
  let todo_items =
    Bonsai.assoc
      (module Int)
      filtered_model
      ~f:(fun _id todo -> todo_item_component ~inject todo)
      graph
  in
  let%arr todo_items = todo_items
  and active_count = active_count
  and inject = inject in
  let toggle_all =
    let handle_clicked _ _ =
      inject (Toggle_all { active_todo_ids = Map.key_set todo_items })
    in
    let attr =
      let is_checked = active_count = 0 in
      Attr.many
        [ Dom_helpers.filtered_attrs [ Style.checked, is_checked ]
        ; Vdom.Attr.bool_property "checked" is_checked
          (* weirdly, todomvc expects this checkbox to have both a class and an id named toggle-all. *)
        ; Vdom.Attr.id "toggle-all"
        ; Style.toggle_all
        ; Attr.type_ "checkbox"
        ; Attr.on_change handle_clicked
        ]
    in
    Node.input ~attrs:[ attr ] ()
  in
  Node.section
    ~attrs:[ Style.main ]
    [ toggle_all
    ; Node.label ~attrs:[ Attr.for_ "toggle-all" ] [ Node.text "Mark all as complete" ]
    ; Node.ul ~attrs:[ Style.todo_list ] (Map.data todo_items)
    ]
;;

let pluralize count word = if count > 1 then word ^ "s" else word

let footer_component
  (state : Model.t Bonsai.t)
  ~(inject : (Action.t -> unit Effect.t) Bonsai.t)
  _graph
  =
  let%arr inject = inject
  and active, completed =
    let%map state = state in
    let completed = Map.filter state ~f:(fun e -> e.completed) in
    let active = Map.filter state ~f:(fun e -> not e.completed) in
    active, completed
  in
  let active_count = Map.length active in
  let completed_count = Map.length completed in
  let todo_count =
    let text = Node.textf "%d %s left" active_count (pluralize active_count "item") in
    Node.span ~attrs:[ Style.todo_count ] [ Node.strong [ text ] ]
  in
  let filters =
    Node.ul
      ~attrs:[ Style.filters ]
      [ Node.li [ Node.a ~attrs:[ Attr.href "#/" ] [ Node.text "All" ] ]
      ; Node.li [ Node.a ~attrs:[ Attr.href "#/active" ] [ Node.text "Active" ] ]
      ; Node.li [ Node.a ~attrs:[ Attr.href "#/completed" ] [ Node.text "Completed" ] ]
      ]
  in
  let maybe_clear_completed_button =
    match completed_count with
    | 0 -> Node.none
    | _ ->
      let on_click _ = inject Clear_completed in
      let attr = Attr.many [ Style.clear_completed; Attr.on_click on_click ] in
      Node.button ~attrs:[ attr ] [ Node.text "Clear completed" ]
  in
  Node.footer
    ~attrs:[ Style.footer ]
    [ todo_count; filters; maybe_clear_completed_button ]
;;

let info =
  Vdom.Node.footer
    ~attrs:[ Style.info ]
    [ Vdom.Node.p [ Vdom.Node.text "Double-click to edit a todo" ]
    ; Vdom.Node.p
        [ Vdom.Node.text "Written by "
        ; Vdom.Node.a
            ~attrs:[ Vdom.Attr.href "http://github.com/samouri/" ]
            [ Vdom.Node.text "@samouri" ]
        ]
    ; Vdom.Node.p
        [ Vdom.Node.text "Part of "
        ; Vdom.Node.a
            ~attrs:[ Vdom.Attr.href "http://todomvc.com" ]
            [ Vdom.Node.text "TodoMVC" ]
        ]
    ]
;;

let root_component graph =
  let default_model = Bonsai_web.Persistent_var.get persisted_model in
  let state, inject =
    Bonsai.state_machine0
      graph
      ~sexp_of_model:[%sexp_of: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model
      ~apply_action
      ~equal:[%equal: Model.t]
  in
  let header_component = header_component ~inject graph in
  let todo_list = todo_list ~inject state graph in
  let footer_component = footer_component state ~inject graph in
  let%arr header_component = header_component
  and todo_list = todo_list
  and footer_component = footer_component in
  let app =
    Node.section
      ~attrs:[ Style.todoapp ]
      [ header_component; todo_list; footer_component ]
  in
  Vdom.Node.div [ app; info ]
;;

let () = Bonsai_web.Start.start root_component
