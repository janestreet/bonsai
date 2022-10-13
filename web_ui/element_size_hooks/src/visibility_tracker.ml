open! Core
open! Import
open! Bonsai_web
open Js_of_ocaml

module Bbox = struct
  type 'a t =
    { min_x : 'a
    ; max_x : 'a
    ; min_y : 'a
    ; max_y : 'a
    }
  [@@deriving sexp, equal]

  module Int = struct
    type nonrec t = int t [@@deriving sexp, equal]

    let width { min_x; max_x; _ } = max_x - min_x
    let height { min_y; max_y; _ } = max_y - min_y
  end

  module Float = struct
    type nonrec t = float t [@@deriving sexp, equal]

    let width { min_x; max_x; _ } = max_x -. min_x
    let height { min_y; max_y; _ } = max_y -. min_y
  end

  let intersection a b =
    let open Core.Float in
    let r =
      { min_x = max a.min_x b.min_x
      ; min_y = max a.min_y b.min_y
      ; max_x = min a.max_x b.max_x
      ; max_y = min a.max_y b.max_y
      }
    in
    if Float.height r <= 0.0 || Float.width r <= 0.0 then None else Some r
  ;;

  let round_bigger { min_x; min_y; max_x; max_y } =
    let open Core.Float in
    { min_x = iround_down_exn min_x
    ; min_y = iround_down_exn min_y
    ; max_x = iround_up_exn max_x
    ; max_y = iround_up_exn max_y
    }
  ;;
end

let intersect_and_reproject client_bbox window_bbox =
  Option.map (Bbox.intersection client_bbox window_bbox) ~f:(fun inter ->
    let min_x = inter.min_x -. client_bbox.min_x in
    let min_y = inter.min_y -. client_bbox.min_y in
    let max_x = min_x +. Bbox.Float.width inter in
    let max_y = min_y +. Bbox.Float.height inter in
    Bbox.round_bigger { min_x; min_y; max_x; max_y })
;;

let compute_visibility ~client_bbox ~window_bbox ~parents =
  let window_bbox =
    List.fold parents ~init:(Some window_bbox) ~f:(fun acc parent ->
      match acc with
      | None -> None
      | Some acc -> Bbox.intersection acc parent)
  in
  let visible_section =
    Option.bind window_bbox ~f:(intersect_and_reproject client_bbox)
  in
  Bbox.round_bigger client_bbox, visible_section
;;

let rec get_parent_rects element =
  let open Option.Let_syntax in
  let o =
    let%bind parent = Js.Opt.to_option element##.parentNode in
    let%bind parent = Js.Opt.to_option (Dom_html.CoerceTo.element parent) in
    let parent_bb = element##getBoundingClientRect in
    let parent_bb =
      { Bbox.min_x = parent_bb##.left
      ; min_y = parent_bb##.top
      ; max_y = parent_bb##.bottom
      ; max_x = parent_bb##.right
      }
    in
    Some (parent_bb :: get_parent_rects parent)
  in
  Option.value ~default:[] o
;;

let get_accurate_vis_bounds (element : Dom_html.element Js.t) =
  let client_bbox =
    let client_bounds = element##getBoundingClientRect in
    { Bbox.min_x = client_bounds##.left
    ; min_y = client_bounds##.top
    ; max_y = client_bounds##.bottom
    ; max_x = client_bounds##.right
    }
  in
  let window_bbox =
    { Bbox.min_x = 0.0
    ; min_y = 0.0
    ; max_x = Int.to_float Dom_html.window##.innerWidth
    ; max_y = Int.to_float Dom_html.window##.innerHeight
    }
  in
  let parents = get_parent_rects element in
  compute_visibility ~client_bbox ~window_bbox ~parents
;;

module T = struct
  module Input = struct
    type t =
      { client_rect_changed : int Bbox.t -> unit Vdom.Effect.t
      ; visible_rect_changed : int Bbox.t -> unit Vdom.Effect.t
      }

    let sexp_of_t = sexp_of_opaque

    let combine
          { client_rect_changed = client_rect_changed_left
          ; visible_rect_changed = visible_rect_changed_left
          }
          { client_rect_changed = client_rect_changed_right
          ; visible_rect_changed = visible_rect_changed_right
          }
      =
      let client_rect_changed bbox =
        Vdom.Effect.sequence_as_sibling
          (client_rect_changed_left bbox)
          ~unless_stopped:(fun () -> client_rect_changed_right bbox)
      in
      let visible_rect_changed bbox =
        Vdom.Effect.sequence_as_sibling
          (visible_rect_changed_left bbox)
          ~unless_stopped:(fun () -> visible_rect_changed_right bbox)
      in
      { client_rect_changed; visible_rect_changed }
    ;;
  end

  module State = struct
    type prev =
      { mutable whole_element : int Bbox.t
      ; mutable visible_section : int Bbox.t
      }
    [@@deriving sexp, equal]

    type nonrec t =
      { mutable callback : Input.t
      ; mutable prev : prev
      ; mutable animation_id : (Dom_html.animation_frame_request_id[@sexp.opaque])
      ; mutable dirty : bool
      }
    [@@deriving sexp_of]

    let schedule state ~f =
      cancel_animation_frame state.animation_id;
      state.animation_id <- request_animation_frame f
    ;;
  end

  let init (callback : Input.t) element =
    let state =
      { State.callback
      ; prev =
          { whole_element = { min_x = 0; min_y = 0; max_x = 0; max_y = 0 }
          ; visible_section = { min_x = 0; min_y = 0; max_x = 0; max_y = 0 }
          }
      ; animation_id = request_animation_frame (Fn.const ())
      ; dirty = true
      }
    in
    let rec every_frame _frame_time =
      let whole_element, visibility = get_accurate_vis_bounds element in
      (match visibility with
       | Some visible_section
         when state.dirty
           || not ([%equal: int Bbox.t] visible_section state.prev.visible_section) ->
         Effect.Expert.handle_non_dom_event_exn
           (state.callback.visible_rect_changed visible_section);
         state.prev.visible_section <- visible_section
       | _ -> ());
      if state.dirty || not ([%equal: int Bbox.t] whole_element state.prev.whole_element)
      then (
        Effect.Expert.handle_non_dom_event_exn
          (state.callback.client_rect_changed whole_element);
        state.prev.whole_element <- whole_element);
      state.dirty <- false;
      State.schedule state ~f:every_frame
    in
    State.schedule state ~f:every_frame;
    state
  ;;

  let on_mount _input _state _element = ()

  let update ~old_input:_ ~new_input:callback (state : State.t) _element =
    state.dirty <- true;
    state.callback <- callback
  ;;

  let destroy _input (state : State.t) _element =
    cancel_animation_frame state.animation_id
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (T)

let detect
      ?(client_rect_changed = fun _ -> Effect.Ignore)
      ?(visible_rect_changed = fun _ -> Effect.Ignore)
      ()
  =
  Vdom.Attr.create_hook
    "bounds-change"
    (Hook.create { T.Input.client_rect_changed; visible_rect_changed })
;;

module For_testing = struct
  include Hook.For_testing
  include T.Input

  let intersect_and_reproject = intersect_and_reproject

  let compute_visibility ~client_bbox ~window_bbox ~parents =
    compute_visibility ~client_bbox ~window_bbox ~parents |> Tuple2.get2
  ;;
end
