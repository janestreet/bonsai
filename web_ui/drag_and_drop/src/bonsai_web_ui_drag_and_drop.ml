open! Core
open Virtual_dom
open Bonsai
open Bonsai.Let_syntax
open Js_of_ocaml
open Vdom

module Position = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, equal]
end

module Size = struct
  type t =
    { width : int
    ; height : int
    }
  [@@deriving sexp, equal]
end

module State_machine_model = struct
  type ('source_id, 'target_id) dragging =
    { source : 'source_id
    ; target : 'target_id option
    (* If [has_moved] is false, then the mouse has been clicked, but we do
       not yet consider the dragging to have started, so for all visual
       purposes we are [Not_dragging]. *)
    ; has_moved : bool
    ; offset : Position.t
    ; position : Position.t
    ; size : Size.t
    }
  [@@deriving sexp, equal]

  type ('source_id, 'target_id) t =
    | Not_dragging
    | Dragging of ('source_id, 'target_id) dragging
  [@@deriving sexp, equal]
end

module Action = struct
  type ('source_id, 'target_id) t =
    | Started_drag of
        { source : 'source_id
        ; offset : Position.t
        ; position : Position.t
        ; size : Size.t
        }
    | Set_target of 'target_id option
    | Finished_drag
    | Mouse_moved of Position.t
  [@@deriving sexp, equal]
end

type ('source_id, 'target_id) t =
  { source : id:'source_id -> Attr.t
  ; drop_target : id:'target_id -> Attr.t
  ; sentinel : name:string -> Attr.t
  ; model : ('source_id, 'target_id) State_machine_model.t
  ; inject : ('source_id, 'target_id) Action.t -> Ui_event.t
  }
[@@deriving fields]

let bug message =
  let message = sprintf "BUG: %s. Report to Bonsai developers" message in
  eprint_s [%message message]
;;

module For_testing = struct
  module Action = struct
    type t =
      | Start_drag of string
      | Set_target of string option
      | Finish_drag
    [@@deriving sexp, equal]

    let to_internal_actions
          (type source target)
          (module Source : Model with type t = source)
          (module Target : Model with type t = target)
      = function
        | Start_drag source ->
          [ Action.Started_drag
              { source = Sexp.of_string source |> Source.t_of_sexp
              ; offset = { x = 0; y = 0 }
              ; position = { x = 0; y = 0 }
              ; size = { width = 0; height = 0 }
              }
          ; Action.Mouse_moved { x = 0; y = 0 }
          ]
        | Set_target (Some target) ->
          [ Set_target (Some (Sexp.of_string target |> Target.t_of_sexp)) ]
        | Set_target None -> [ Set_target None ]
        | Finish_drag -> [ Finished_drag ]
    ;;
  end

  module Inject_hook = Attr.Hooks.Make (struct
      module State = Unit

      module Input = struct
        type t = Action.t -> Ui_event.t [@@deriving sexp]

        let combine _ second = second
      end

      let init _ _ = ()
      let on_mount _ () _ = ()
      let update ~old_input:_ ~new_input:_ () _ = ()
      let destroy _ () _ = ()
    end)

  let type_id = Inject_hook.For_testing.type_id
end

let add_event_listener, remove_event_listener =
  let active : Dom_html.event_listener_id Bonsai.Private.Path.Map.t ref =
    ref Bonsai.Private.Path.Map.empty
  in
  let install =
    Effect.of_sync_fun (fun (typ, path, handler) ->
      if am_running_test
      then print_endline "adding window event listener"
      else (
        let listener =
          Dom_html.addEventListener Dom_html.window typ (Dom.handler handler) Js._true
        in
        active := Map.update !active path ~f:(function _ -> listener);
        ()))
    |> unstage
  in
  let uninstall =
    Effect.of_sync_fun (fun path ->
      if am_running_test
      then print_endline "removing window event listener"
      else Map.find !active path |> Option.iter ~f:Dom_html.removeEventListener)
    |> unstage
  in
  (fun typ path handler -> install (typ, path, handler)), uninstall
;;

let create
      (type source target)
      here
      ~source_id:(module Source : Model with type t = source)
      ~target_id:(module Target : Model with type t = target)
      ~on_drop
  =
  let%sub model, inject =
    Bonsai.state_machine1
      here
      (module struct
        type t = (Source.t, Target.t) State_machine_model.t [@@deriving sexp, equal]
      end)
      (module struct
        type t = (Source.t, Target.t) Action.t [@@deriving sexp, equal]
      end)
      on_drop
      ~default_model:Not_dragging
      ~apply_action:(fun ~inject:_ ~schedule_event on_drop model action ->
        match action with
        | Started_drag { source; offset; position; size } ->
          (match model with
           | State_machine_model.Not_dragging -> ()
           | Dragging _ -> bug "Started dragging before dragging finished");
          Dragging { source; offset; position; size; target = None; has_moved = false }
        | Set_target target ->
          (match model with
           | State_machine_model.Not_dragging -> Not_dragging
           | Dragging t -> Dragging { t with target })
        | Finished_drag ->
          (match model with
           | State_machine_model.Not_dragging | Dragging { target = None; _ } ->
             Not_dragging
           | Dragging { source; target = Some target; _ } ->
             schedule_event (on_drop source target);
             Not_dragging)
        | Mouse_moved position ->
          (match model with
           | State_machine_model.Not_dragging -> Not_dragging
           | Dragging t -> Dragging { t with position; has_moved = true }))
  in
  let%sub source =
    return
      (let%map inject = inject in
       fun ~id ->
         Attr.many
           [ Attr.on_mousedown (fun event ->
               let position = { Position.x = event##.clientX; y = event##.clientY } in
               let bounding_rect =
                 (Js.Opt.to_option event##.currentTarget |> Option.value_exn)##getBoundingClientRect
               in
               let optdef_float x =
                 x |> Js.Optdef.to_option |> Option.value_exn |> Int.of_float
               in
               let width = optdef_float bounding_rect##.width in
               let height = optdef_float bounding_rect##.height in
               let top = Int.of_float bounding_rect##.top in
               let left = Int.of_float bounding_rect##.left in
               let size = { Size.width; height } in
               let offset = { Position.x = position.x - left; y = position.y - top } in
               inject (Started_drag { source = id; offset; position; size }))
           ; Attr.style (Css_gen.user_select `None)
           ])
  in
  let%sub path = Bonsai.Private.path in
  let%sub path_for_mousemove = Bonsai.Private.path in
  let%sub path_for_mouseup = Bonsai.Private.path in
  let%sub universe_suffix =
    Bonsai.pure Bonsai.Private.Path.to_unique_identifier_string path
  in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_deactivate:
        (let%map path_for_mousemove = path_for_mousemove
         and path_for_mouseup = path_for_mouseup in
         Effect.inject_ignoring_response
         @@ let%bind.Effect () = remove_event_listener path_for_mousemove in
         let%bind.Effect () = remove_event_listener path_for_mouseup in
         Effect.return ())
      ~on_activate:
        (let%map inject = inject
         and path_for_mousemove = path_for_mousemove
         and path_for_mouseup = path_for_mouseup
         and universe_suffix = universe_suffix in
         (let%bind.Effect () =
            add_event_listener
              Dom_html.Event.mousemove
              path_for_mousemove
              (fun (event : Dom_html.mouseEvent Js.t) ->
                 let (event
                      : < composedPath : 'a Js.js_array Js.t Js.meth
                     ; Dom_html.mouseEvent >
                       Js.t)
                   =
                   Js.Unsafe.coerce event
                 in
                 (* Why client coordinates and not page or screen coordinates. I've
                    tested with all three and client coordinates is clearly the
                    correct choice.

                    - page: If you scroll while dragging, the dragged element moves
                      away from your mouse because the diff between start and end
                      positions gets larger even though the mouse is stationary on
                      the screen.
                    - screen: If you move the mouse while dragging (which can
                      happen if you use window management keyboard shortcuts), the
                      dragged element stays in the same position relative to the
                      browser window, since the mouse didn't move, but this is not
                      good because the mouse window has moved away from the mouse.
                    - client: Scrolling or moving the window does not pull the
                      dragged element away from the mouse.

                    It makes sense that client coordinates is correct because the
                    dragged element itself uses fixed positioning, which is roughly
                    equivalent to client coordinates.  *)
                 let position = { Position.x = event##.clientX; y = event##.clientY } in
                 let path = Js.to_array event##composedPath |> Array.to_list in
                 let target =
                   List.find_map path ~f:(fun element ->
                     let%bind.Option dataset = Js.Opt.to_option element##.dataset in
                     let%map.Option drag_target =
                       Js.Opt.to_option
                         (Js.Unsafe.get dataset ("dragTarget" ^ universe_suffix))
                     in
                     let drag_target = Js.to_string drag_target in
                     Target.t_of_sexp (Sexp.of_string drag_target))
                 in
                 Event.Expert.handle_non_dom_event_exn
                   (Event.Many
                      [ inject (Set_target target); inject (Mouse_moved position) ]);
                 Js._true)
          in
          add_event_listener Dom_html.Event.mouseup path_for_mouseup (fun _ ->
            Event.Expert.handle_non_dom_event_exn (inject Finished_drag);
            Js._true))
         |> Effect.inject_ignoring_response)
      ()
  in
  let%sub sentinel =
    return
      (let%map inject = inject in
       fun ~name ->
         Attr.many
           [ Attr.create_hook
               "dnd-test-hook"
               (For_testing.Inject_hook.create (fun action ->
                  Event.Many
                    (action
                     |> For_testing.Action.to_internal_actions
                          (module Source)
                          (module Target)
                     |> List.map ~f:inject)))
           ; Attr.create "data-dnd-name" name
           ])
  in
  return
    (let%map model = model
     and universe_suffix = universe_suffix
     and inject = inject
     and source = source
     and sentinel = sentinel in
     let drop_target ~id =
       Attr.many
         [ Attr.on_mouseup (fun _ -> inject Finished_drag)
         ; Attr.create
             ("data-drag-target" ^ universe_suffix)
             (Sexp.to_string_mach (Target.sexp_of_t id))
         ]
     in
     { model; inject; source; drop_target; sentinel })
;;

let dragged_element t ~f =
  match%sub t >>| model with
  | Not_dragging | Dragging { has_moved = false; _ } -> Bonsai.const Node.None
  | Dragging ({ source; _ } as dragging) ->
    let%sub item = f source in
    return
      (let%map { position; offset; size; _ } = dragging
       and item = item in
       let x = position.x - offset.x in
       let y = position.y - offset.y in
       Node.div
         ~attr:
           (Attr.style
              Css_gen.(
                position `Fixed ~top:(`Px 0) ~left:(`Px 0)
                @> create ~field:"pointer-events" ~value:"none"
                @> width (`Px size.width)
                @> height (`Px size.height)
                @> create
                     ~field:"transform"
                     ~value:[%string "translateY(%{y#Int}px) translateX(%{x#Int}px)"]))
         [ item ])
;;

(* A cut-down version of [State_machine_model] for users of the library *)
module Model = struct
  type ('source_id, 'target_id) t =
    | Not_dragging
    | Dragging of
        { source : 'source_id
        ; target : 'target_id option
        }
  [@@deriving sexp, equal]
end

let model t =
  match t.model with
  | Not_dragging | Dragging { has_moved = false; _ } -> Model.Not_dragging
  | Dragging { source; target; _ } -> Dragging { source; target }
;;
