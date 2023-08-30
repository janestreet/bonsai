open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml

module Style =
[%css
stylesheet
  {|
  .no_select {
    user-select: none;
  }

  .dragged_element {
    position: fixed;
    top: 0px;
    left: 0px;
    pointer-events: none;
  }
  |}]

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

  let map_target : ('source, 'a) t -> f:('a -> 'b) -> ('source, 'b) t =
    fun t ~f ->
    match t with
    | Not_dragging -> Not_dragging
    | Dragging t -> Dragging { t with target = Option.map t.target ~f }
  ;;
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

  let map_target : ('source, 'a) t -> f:('a -> 'b) -> ('source, 'b) t =
    fun t ~f ->
    match t with
    | Finished_drag -> Finished_drag
    | Mouse_moved position -> Mouse_moved position
    | Set_target x -> Set_target (Option.map x ~f)
    | Started_drag { source; offset; position; size } ->
      Started_drag { source; offset; position; size }
  ;;
end

type ('source_id, 'target_id) t =
  { source : id:'source_id -> Vdom.Attr.t
  ; drop_target : id:'target_id -> Vdom.Attr.t
  ; sentinel : name:string -> Vdom.Attr.t
  ; model : ('source_id, 'target_id) State_machine_model.t
  ; inject : ('source_id, 'target_id) Action.t list -> unit Effect.t
  }
[@@deriving fields ~getters]

let project_target
  :  ('source, 'target_a) t -> map:('target_a -> 'target_b)
  -> unmap:('target_b -> 'target_a) -> ('source, 'target_b) t
  =
  fun t ~map ~unmap ->
  let source ~id = t.source ~id in
  let drop_target ~id = t.drop_target ~id:(unmap id) in
  let sentinel ~name = t.sentinel ~name in
  let model = State_machine_model.map_target t.model ~f:map in
  let inject : ('source, 'target_b) Action.t list -> unit Effect.t =
    fun actions ->
    t.inject (List.map actions ~f:(fun action -> Action.map_target action ~f:unmap))
  in
  { source; drop_target; sentinel; model; inject }
;;

let bug message =
  let message = sprintf "BUG: %s. Report to Bonsai developers" message in
  eprint_s [%message message]
;;

module type S = sig
  type t [@@deriving sexp, equal]
end

module For_testing = struct
  module Action = struct
    type t =
      | Start_drag of string
      | Set_target of string option
      | Finish_drag
    [@@deriving sexp, equal]

    let to_internal_actions
      (type source target)
      (module Source : S with type t = source)
      (module Target : S with type t = target)
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

  module Inject_hook = Vdom.Attr.No_op_hook (struct
    module Input = struct
      type t = Action.t -> unit Effect.t [@@deriving sexp]

      let combine _ second = second
    end

    let name = "dnd-test-hook"
  end)

  let type_id = Inject_hook.type_id
end

let add_event_listener, remove_event_listener =
  let active : Dom_html.event_listener_id Bonsai.Private.Path.Map.t ref =
    ref Bonsai.Private.Path.Map.empty
  in
  let install =
    Bonsai.Effect.of_sync_fun (fun (typ, path, handler) ->
      match Bonsai_web.am_running_how with
      | `Node_test -> print_endline "adding window event listener"
      | `Browser | `Browser_benchmark ->
        let listener =
          Dom_html.addEventListener Dom_html.window typ (Dom.handler handler) Js._true
        in
        active := Map.update !active path ~f:(function _ -> listener);
        ()
      | `Node | `Node_benchmark -> ())
  in
  let uninstall =
    Bonsai.Effect.of_sync_fun (fun path ->
      match Bonsai_web.am_running_how with
      | `Node_test -> print_endline "removing window event listener"
      | `Browser | `Browser_benchmark ->
        Map.find !active path |> Option.iter ~f:Dom_html.removeEventListener
      | `Node | `Node_benchmark -> ())
  in
  (fun typ path handler -> install (typ, path, handler)), uninstall
;;

let create
  (type source target)
  ~source_id:(module Source : S with type t = source)
  ~target_id:(module Target : S with type t = target)
  ~on_drop
  =
  let%sub model, inject =
    Bonsai.state_machine1
      ~sexp_of_model:[%sexp_of: (Source.t, Target.t) State_machine_model.t]
      ~equal:[%equal: (Source.t, Target.t) State_machine_model.t]
      ~sexp_of_action:[%sexp_of: (Source.t, Target.t) Action.t list]
      on_drop
      ~default_model:Not_dragging
      ~apply_action:(fun context on_drop model actions ->
      match on_drop with
      | Active on_drop ->
        List.fold actions ~init:model ~f:(fun model action ->
          match action with
          | Action.Started_drag { source; offset; position; size } ->
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
               Bonsai.Apply_action_context.schedule_event context (on_drop source target);
               Not_dragging)
          | Mouse_moved position ->
            (match model with
             | State_machine_model.Not_dragging -> Not_dragging
             | Dragging t -> Dragging { t with position; has_moved = true }))
      | Inactive ->
        eprint_s
          [%message
            [%here]
              "An action sent to a [state_machine1] has been dropped because its input \
               was not present. This happens when the [state_machine1] is inactive when \
               it receives a message."
              (actions : (Source.t, Target.t) Action.t list)];
        model)
  in
  let%sub source =
    let%arr inject = inject in
    fun ~id ->
      Vdom.Attr.many
        [ Vdom.Attr.on_pointerdown (fun event ->
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
            match Bonsai_web.am_within_disabled_fieldset event with
            | true -> Effect.Ignore
            | false -> inject [ Started_drag { source = id; offset; position; size } ])
        ; Style.no_select
        ]
  in
  let%sub path = Bonsai.Private.path in
  let%sub path_for_pointermove = Bonsai.Private.path in
  let%sub path_for_pointerup = Bonsai.Private.path in
  let%sub universe_suffix =
    let%arr path = path in
    Bonsai.Private.Path.to_unique_identifier_string path
  in
  let%sub on_deactivate =
    let%arr path_for_pointermove = path_for_pointermove
    and path_for_pointerup = path_for_pointerup in
    Effect.all_unit
      [ remove_event_listener path_for_pointermove
      ; remove_event_listener path_for_pointerup
      ]
  in
  let%sub on_activate =
    let%arr inject = inject
    and path_for_pointermove = path_for_pointermove
    and path_for_pointerup = path_for_pointerup
    and universe_suffix = universe_suffix in
    let%bind.Effect () =
      add_event_listener
        Dom_html.Event.pointermove
        path_for_pointermove
        (fun (event : Dom_html.pointerEvent Js.t) ->
        let (event
              : < composedPath : 'a Js.js_array Js.t Js.meth ; Dom_html.pointerEvent >
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
              Js.Opt.to_option (Js.Unsafe.get dataset ("dragTarget" ^ universe_suffix))
            in
            let drag_target = Js.to_string drag_target in
            Target.t_of_sexp (Sexp.of_string drag_target))
        in
        Effect.Expert.handle_non_dom_event_exn
          (match Bonsai_web.am_within_disabled_fieldset event with
           | true -> inject [ Set_target None; Mouse_moved position ]
           | false -> inject [ Set_target target; Mouse_moved position ]);
        Js._true)
    in
    add_event_listener Dom_html.Event.pointerup path_for_pointerup (fun event ->
      Effect.Expert.handle_non_dom_event_exn
        (match Bonsai_web.am_within_disabled_fieldset event with
         | true -> inject [ Set_target None; Finished_drag ]
         | false -> inject [ Finished_drag ]);
      Js._true)
  in
  let%sub () = Bonsai.Edge.lifecycle ~on_deactivate ~on_activate () in
  let%sub sentinel =
    let%arr inject = inject in
    fun ~name ->
      Vdom.Attr.many
        [ For_testing.Inject_hook.attr (fun action ->
            inject
              (action
               |> For_testing.Action.to_internal_actions (module Source) (module Target)))
        ; Vdom.Attr.create "data-dnd-name" name
        ]
  in
  let%sub drop_target =
    let%arr inject = inject
    and universe_suffix = universe_suffix in
    fun ~id ->
      Vdom.Attr.many
        [ Vdom.Attr.on_pointerup (fun event ->
            match Bonsai_web.am_within_disabled_fieldset event with
            | true -> inject [ Set_target None; Finished_drag ]
            | false -> inject [ Finished_drag ])
        ; Vdom.Attr.create
            ("data-drag-target" ^ universe_suffix)
            (Sexp.to_string_mach (Target.sexp_of_t id))
        ]
  in
  let%arr model = model
  and inject = inject
  and source = source
  and sentinel = sentinel
  and drop_target = drop_target in
  { model; inject; source; drop_target; sentinel }
;;

let dragged_element t ~f =
  match%sub t >>| model with
  | Not_dragging | Dragging { has_moved = false; _ } -> Bonsai.const Vdom.Node.None
  | Dragging ({ source; _ } as dragging) ->
    let%sub item = f source in
    let%arr { position; offset; size; _ } = dragging
    and item = item in
    let x = position.x - offset.x in
    let y = position.y - offset.y in
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.(
            Style.dragged_element
            @ style
                Css_gen.(
                  width (`Px size.width)
                  @> height (`Px size.height)
                  @> create
                       ~field:"transform"
                       ~value:[%string "translateY(%{y#Int}px) translateX(%{x#Int}px)"]))
        ]
      [ item ]
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
