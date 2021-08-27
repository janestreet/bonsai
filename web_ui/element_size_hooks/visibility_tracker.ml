open! Core
open! Import
open! Bonsai_web
open Js_of_ocaml

module Bounds = struct
  type t =
    { min_x : int
    ; min_y : int
    ; max_x : int
    ; max_y : int
    }
  [@@deriving sexp, equal]
end

let get_conservative_vis_bounds (element : Dom_html.element Js.t) : Bounds.t option =
  let client_bounds = element##getBoundingClientRect in
  let client_x = client_bounds##.left
  and client_y = client_bounds##.top
  and client_bottom = client_bounds##.bottom
  and client_right = client_bounds##.right
  and client_width = client_bounds##.width
  and client_height = client_bounds##.height
  and window_height = Dom_html.window##.innerHeight
  and window_width = Dom_html.window##.innerWidth in
  let%bind.Option window_height = Js.Optdef.to_option window_height in
  let%bind.Option window_width = Js.Optdef.to_option window_width in
  let window_height = Float.of_int window_height
  and window_width = Float.of_int window_width
  and client_width = Js.Optdef.get client_width (Fn.const 0.0)
  and client_height = Js.Optdef.get client_height (Fn.const 0.0) in
  if Float.O.(
    client_y > window_height
    || client_x > window_width
    || client_bottom < 0.0
    || client_right < 0.0)
  then (* The element is not visible *)
    None
  else
    let open Float in
    (* If client_x, client_y, which are relative to viewport, are negative, this means
       we don't see the top/left part of the node, but only the part starting from
       -client_x, -client_y (relative to node's top left corner) *)
    let min_y = max 0.0 (-client_y)
    and min_x = max 0.0 (-client_x)
    (* If client_bottom/right (relative to viewport) are smaller than window width/height,
       this means bottom/right edge is visible. Otherwise, right/bottom part is cropped *)
    and max_y =
      if client_bottom < window_height then client_height else window_height - client_y
    and max_x =
      if client_right < window_width then client_width else window_width - client_x
    in
    Some
      { Bounds.min_x = iround_down_exn min_x
      ; min_y = iround_down_exn min_y
      ; max_x = iround_up_exn max_x
      ; max_y = iround_up_exn max_y
      }
;;

module T = struct
  module Input = struct
    type t = Bounds.t -> unit Vdom.Effect.t [@@deriving sexp_of]

    let combine left right bounds =
      Vdom.Effect.sequence_as_sibling (left bounds) ~unless_stopped:(fun () ->
        right bounds)
    ;;
  end

  module State = struct
    type t =
      { mutable callback : Bounds.t -> unit
      ; mutable prev : Bounds.t
      ; mutable animation_id : (Dom_html.animation_frame_request_id[@sexp.opaque])
      ; mutable dirty : bool
      }
    [@@deriving sexp_of]

    let schedule state ~f =
      cancel_animation_frame state.animation_id;
      state.animation_id <- request_animation_frame f
    ;;
  end

  let wrap_with_handle f t = Vdom.Effect.Expert.handle_non_dom_event_exn (f t)

  let init callback element =
    let state =
      { State.callback = wrap_with_handle callback
      ; prev = { min_x = 0; min_y = 0; max_x = 0; max_y = 0 }
      ; animation_id = request_animation_frame (Fn.const ())
      ; dirty = true
      }
    in
    let rec every_frame _frame_time =
      (match get_conservative_vis_bounds element with
       | Some bounds when state.dirty || not (Bounds.equal bounds state.prev) ->
         state.callback bounds;
         state.prev <- bounds;
         state.dirty <- false
       | _ -> ());
      State.schedule state ~f:every_frame
    in
    State.schedule state ~f:every_frame;
    state
  ;;

  let on_mount _input _state _element = ()

  let update ~old_input:_ ~new_input:callback (state : State.t) _element =
    state.dirty <- true;
    state.callback <- wrap_with_handle callback
  ;;

  let destroy _input (state : State.t) _element =
    cancel_animation_frame state.animation_id
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (T)

let on_change f = Vdom.Attr.create_hook "bounds-change" (Hook.create f)

module For_testing = Hook.For_testing
