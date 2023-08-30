open! Core
open! Import
open! Bonsai_web
open Js_of_ocaml

module Bbox = struct
  type t =
    { min_x : float
    ; max_x : float
    ; min_y : float
    ; max_y : float
    }
  [@@deriving sexp, equal]

  let of_client_rect (box : Dom_html.clientRect Js.t) =
    { min_x = box##.left; min_y = box##.top; max_x = box##.right; max_y = box##.bottom }
  ;;

  let width t = t.max_x -. t.min_x
  let height t = t.max_y -. t.min_y
end

module T = struct
  module Input = struct
    type t =
      { client_rect_changed : Bbox.t -> unit Vdom.Effect.t
      ; visible_rect_changed : Bbox.t option -> unit Vdom.Effect.t
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
      { mutable whole_element : Bbox.t
      ; mutable visible_section : Bbox.t option
      }
    [@@deriving sexp, equal]

    type nonrec t =
      { mutable callback : Input.t
      ; mutable prev : prev
      ; mutable dirty : bool
      ; observer :
          (Js_of_ocaml.IntersectionObserver.intersectionObserver Js.t[@sexp.opaque])
      }
    [@@deriving sexp_of]
  end

  let init (callback : Input.t) element =
    let rec state =
      lazy
        { State.callback
        ; prev =
            { whole_element = { min_x = 0.0; min_y = 0.0; max_x = 0.0; max_y = 0.0 }
            ; visible_section = None
            }
        ; dirty = true
        ; observer = Lazy.force observer
        }
    and observer =
      lazy
        (new%js IntersectionObserver.intersectionObserver
           (Js.wrap_callback
              (fun entries (observer : IntersectionObserver.intersectionObserver Js.t) ->
              for i = 0 to entries##.length - 1 do
                let entry = Js.array_get entries i in
                Js.Optdef.iter entry process
              done;
              (* unobserve and immediately re-observe in order to trigger a
                   recomputation of the intersection rect. *)
              observer##unobserve (element :> #Dom.node Js.t);
              observer##observe (element :> #Dom.node Js.t)))
           (IntersectionObserver.empty_intersection_observer_options ()))
    and process entry =
      let client_rect = Bbox.of_client_rect entry##.boundingClientRect in
      let intersection_rect =
        if Js.to_bool entry##.isIntersecting
        then (
          let bbox = Bbox.of_client_rect entry##.intersectionRect in
          Some
            { Bbox.min_x = bbox.min_x -. client_rect.min_x
            ; min_y = bbox.min_y -. client_rect.min_y
            ; max_x = bbox.max_x -. client_rect.min_x
            ; max_y = bbox.max_y -. client_rect.min_y
            })
        else None
      in
      let state = Lazy.force state in
      if state.dirty
         || not ([%equal: Bbox.t option] intersection_rect state.prev.visible_section)
      then (
        intersection_rect
        |> state.callback.visible_rect_changed
        |> Effect.Expert.handle_non_dom_event_exn;
        state.prev.visible_section <- intersection_rect);
      if state.dirty || not ([%equal: Bbox.t] client_rect state.prev.whole_element)
      then (
        client_rect
        |> state.callback.client_rect_changed
        |> Effect.Expert.handle_non_dom_event_exn;
        state.prev.whole_element <- client_rect);
      state.dirty <- false
    in
    let state = Lazy.force state in
    state.observer##observe element;
    state
  ;;

  let on_mount _input _state _element = ()

  let update ~old_input:_ ~new_input:callback (state : State.t) _element =
    state.dirty <- true;
    state.callback <- callback
  ;;

  let destroy _input (state : State.t) _element = state.observer##disconnect
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
end
