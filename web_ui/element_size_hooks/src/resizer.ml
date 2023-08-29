open! Core
open! Import
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml

let set_cursor s =
  (* The `!important` reduces the browser's need to perform an expensive style recalc. In
     profiling, it reduces the work on pointer-up and pointer-down events by ~10x *)
  let s = [%string "%{s} !important"] in
  Dom_html.window##.document##.body##.style##.cursor := Js.string s
;;

module Pointer_event = struct
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
    | Down -> Dom_html.Event.pointerdown
    | Up -> Dom_html.Event.pointerup
    | Move -> Dom_html.Event.pointermove
  ;;
end

module Side = struct
  type t =
    | Left
    | Right
  [@@deriving sexp_of]
end

module State = struct
  type t =
    { mutable listeners : Dom.event_listener_id Pointer_event.Map.t
    ; mutable animation_id : Dom_html.animation_frame_request_id
    ; mutable pointer_x : float option
    ; mutable last_pointer_x : float option
    ; mutable side : Side.t
    }
  [@@deriving fields ~getters ~setters ~iterators:create]

  let create ~side () =
    let animation_id = request_animation_frame (Fn.const ()) in
    Fields.create
      ~listeners:Pointer_event.Map.empty
      ~animation_id
      ~pointer_x:None
      ~last_pointer_x:None
      ~side
  ;;

  let remove_event_listener = Option.iter ~f:Dom_html.removeEventListener

  let destroy { listeners; animation_id; _ } =
    Map.iter listeners ~f:Dom_html.removeEventListener;
    cancel_animation_frame animation_id
  ;;

  let on_pointer_event state element ~f ~event =
    remove_event_listener (Map.find state.listeners event);
    let id = add_event_listener element (Pointer_event.to_dom_html event) ~f in
    state.listeners <- Map.set state.listeners ~key:event ~data:id
  ;;

  let remove_pointer_event state ~event =
    remove_event_listener (Map.find state.listeners event)
  ;;

  let cancel_schedule state = cancel_animation_frame state.animation_id

  let schedule state ~f =
    cancel_schedule state;
    state.animation_id <- request_animation_frame f
  ;;

  let set_pointer_x state x = state.pointer_x <- Some x
  let clear_pointer_x state = state.pointer_x <- None
  let clear_pointer_start state = state.last_pointer_x <- None
end

let get_parent element =
  let%bind.Option parent = Js.Opt.to_option element##.parentNode in
  Js.Opt.to_option (Dom_html.CoerceTo.element parent)
;;

let rec do_update_width target state =
  let (_ : unit option) =
    let open Option.Let_syntax in
    let%bind pointer_x = State.pointer_x state in
    let%bind last_pointer_x =
      let temp = State.last_pointer_x state in
      State.set_last_pointer_x state (Some pointer_x);
      temp
    in
    let%bind target = Js.Opt.to_option target in
    let%bind parent = get_parent target in
    let parent_rect = parent##getBoundingClientRect in
    let%bind parent_width = Js.Optdef.to_option parent_rect##.width in
    let%bind new_width =
      let operation =
        match state.side with
        | Left -> Float.sub
        | Right -> Float.add
      in
      let diff_x = pointer_x -. last_pointer_x in
      let proposed_width = operation parent_width diff_x in
      let should_accept =
        let is_growing = Float.(proposed_width > parent_width) in
        let is_shrinking = Float.(proposed_width < parent_width) in
        match state.side with
        | Left ->
          let parent_left = parent_rect##.left in
          let pointer_on_left = Float.(pointer_x < parent_left) in
          let pointer_on_right = Float.(pointer_x > parent_left) in
          (is_growing && pointer_on_left) || (is_shrinking && pointer_on_right)
        | Right ->
          let parent_right = parent_rect##.right in
          let pointer_on_left = Float.(pointer_x < parent_right) in
          let pointer_on_right = Float.(pointer_x > parent_right) in
          (is_growing && pointer_on_right) || (is_shrinking && pointer_on_left)
      in
      Option.some_if should_accept proposed_width
    in
    set_width parent new_width;
    return ()
  in
  State.clear_pointer_x state;
  State.schedule state ~f:(fun _ -> do_update_width target state)
;;

module T = struct
  module Input = struct
    type t = Side.t [@@deriving sexp_of]

    (* Randomly pick the first, since it makes no sense to include two resizer
       hooks on the same node *)
    let combine first _second = first
  end

  module State = State

  let init side element =
    let state = State.create ~side () in
    let on_pointer_move _ event =
      State.set_pointer_x state (Float.of_int event##.clientX)
    in
    let on_pointer_up _ _ =
      set_cursor "initial";
      State.clear_pointer_start state;
      State.clear_pointer_x state;
      State.remove_pointer_event state ~event:Move;
      State.remove_pointer_event state ~event:Up;
      State.cancel_schedule state
    in
    let on_pointer_down _ event =
      (* We use currentTarget to ensure it is the node we attached the event
         listener to instead of a child node *)
      let target = event##.currentTarget in
      let clientX : int = event##.clientX in
      State.set_last_pointer_x state (Some (Float.of_int clientX));
      State.on_pointer_event ~event:Move state Dom_html.document ~f:on_pointer_move;
      State.on_pointer_event ~event:Up state Dom_html.document ~f:on_pointer_up;
      State.schedule state ~f:(fun _ -> do_update_width target state);
      set_cursor "col-resize"
    in
    State.on_pointer_event state element ~f:on_pointer_down ~event:Down;
    state
  ;;

  let on_mount _init _state element =
    Option.iter (get_parent element) ~f:Freeze.Expert.set_width
  ;;

  let update ~old_input:_ ~new_input (state : State.t) _element = state.side <- new_input

  let destroy _input state (element : Dom_html.element Js.t) =
    set_cursor "initial";
    Option.iter (get_parent element) ~f:Freeze.Expert.reset_width;
    State.destroy state
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (T)

let attr ~side = Vdom.Attr.create_hook "resizer" (Hook.create side)
