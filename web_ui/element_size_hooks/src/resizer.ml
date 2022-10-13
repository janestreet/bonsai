open! Core
open! Import
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml

let set_cursor s = Dom_html.window##.document##.body##.style##.cursor := Js.string s

module Mouse_event = struct
  module T = struct
    type t =
      | Down
      | Up
      | Move
    [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)

  let to_dom_html = function
    | Down -> Dom_html.Event.mousedown
    | Up -> Dom_html.Event.mouseup
    | Move -> Dom_html.Event.mousemove
  ;;
end

module State = struct
  type t =
    { mutable listeners : Dom.event_listener_id Mouse_event.Map.t
    ; mutable animation_id : Dom_html.animation_frame_request_id
    ; mutable mouse_x : int option
    }
  [@@deriving fields]

  let create () =
    let animation_id = request_animation_frame (Fn.const ()) in
    Fields.create ~listeners:Mouse_event.Map.empty ~animation_id ~mouse_x:None
  ;;

  let remove_event_listener = Option.iter ~f:Dom_html.removeEventListener

  let destroy { listeners; animation_id; mouse_x = _ } =
    Map.iter listeners ~f:Dom_html.removeEventListener;
    cancel_animation_frame animation_id
  ;;

  let on_mouse_event state element ~f ~event =
    remove_event_listener (Map.find state.listeners event);
    let id = add_event_listener element (Mouse_event.to_dom_html event) ~f in
    state.listeners <- Map.set state.listeners ~key:event ~data:id
  ;;

  let remove_mouse_event state ~event =
    remove_event_listener (Map.find state.listeners event)
  ;;

  let cancel_schedule state = cancel_animation_frame state.animation_id

  let schedule state ~f =
    cancel_schedule state;
    state.animation_id <- request_animation_frame f
  ;;

  let set_mouse_x state x = state.mouse_x <- Some x
  let clear_mouse_x state = state.mouse_x <- None
end

let get_parent element =
  let%bind.Option parent = Js.Opt.to_option element##.parentNode in
  Js.Opt.to_option (Dom_html.CoerceTo.element parent)
;;

let rec do_update_width target state =
  let (_ : unit option) =
    let open Option.Let_syntax in
    let%bind mouse_x = State.mouse_x state in
    let%bind target = Js.Opt.to_option target in
    let%bind parent = get_parent target in
    let rect = parent##getBoundingClientRect in
    let left = rect##.left in
    let new_width = Int.to_float mouse_x -. left in
    set_width parent new_width;
    return ()
  in
  State.clear_mouse_x state;
  State.schedule state ~f:(fun _ -> do_update_width target state)
;;

module T = struct
  module Input = struct
    include Unit

    let combine () () = ()
  end

  module State = State

  let init () element =
    let state = State.create () in
    let on_mouse_move _ event = State.set_mouse_x state event##.clientX in
    let on_mouse_up _ _ =
      set_cursor "initial";
      State.remove_mouse_event state ~event:Move;
      State.remove_mouse_event state ~event:Up;
      State.cancel_schedule state
    in
    let on_mouse_down _ event =
      let target = event##.target in
      State.clear_mouse_x state;
      do_update_width target state;
      State.on_mouse_event ~event:Move state Dom_html.document ~f:on_mouse_move;
      State.on_mouse_event ~event:Up state Dom_html.document ~f:on_mouse_up;
      set_cursor "col-resize"
    in
    State.on_mouse_event state element ~f:on_mouse_down ~event:Down;
    state
  ;;

  let on_mount () _state element =
    Option.iter (get_parent element) ~f:Freeze.Expert.set_width
  ;;

  let update ~old_input:() ~new_input:() _state _element = ()

  let destroy () state (element : Dom_html.element Js.t) =
    set_cursor "initial";
    Option.iter (get_parent element) ~f:Freeze.Expert.reset_width;
    State.destroy state
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (T)

let attr = Vdom.Attr.create_hook "resizer" (Hook.create ())
