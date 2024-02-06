open! Core
open! Import
open! Bonsai_web
open! Js_of_ocaml

module Dimensions = struct
  type t =
    { width : float
    ; height : float
    }
end

module T = struct
  module State = struct
    type t =
      { mutable callback : width:float -> height:float -> unit
      ; mutable observer : ResizeObserver.resizeObserver Js.t option
      ; mutable last_width : float
      ; mutable last_height : float
      }
  end

  let observe node ~(state : State.t) =
    (* We take the whole state here so that we can mutate the callback in it and
       witness the change in the observer *)
    let on_resize_observed entries _observer =
      let open Option.Let_syntax in
      let size =
        let%bind first_entry = Js.array_get entries 0 |> Js.Optdef.to_option in
        let border_box_arr = first_entry##.borderBoxSize in
        let%map border_box = Js.array_get border_box_arr 0 |> Js.Optdef.to_option in
        (* This assumes writing-mode:horizontal.
           https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserverEntry/borderBoxSize *)
        border_box##.inlineSize, border_box##.blockSize
      in
      Option.iter size ~f:(fun (width, height) ->
        state.last_width <- width;
        state.last_height <- height;
        state.callback ~width ~height)
    in
    ResizeObserver.observe () ~node ~f:on_resize_observed
  ;;

  open State

  module Input = struct
    type t = Dimensions.t -> unit Vdom.Effect.t [@@deriving sexp_of]

    let combine left right dimensions =
      Vdom.Effect.sequence_as_sibling (left dimensions) ~unless_stopped:(fun () ->
        right dimensions)
    ;;
  end

  let wrap_with_handle ~width ~height ~f =
    Vdom.Effect.Expert.handle_non_dom_event_exn (f { Dimensions.width; height })
  ;;

  let init input _ =
    { State.observer = None
    ; callback = wrap_with_handle ~f:input
    ; last_width = 0.0
    ; last_height = 0.0
    }
  ;;

  let on_mount _ state element = state.observer <- Some (observe ~state element)

  let update ~old_input ~new_input state _ =
    if phys_equal old_input new_input
    then ()
    else (
      state.callback <- wrap_with_handle ~f:new_input;
      (* if the "size change" callback function changes, we should send it what we
         currently think the size is, otherwise if the element never changes size, 
         the function would never get called, so whatever is ttracking the size would 
         always remain clueless... *)
      state.callback ~width:state.last_width ~height:state.last_height)
  ;;

  let destroy _ state _ =
    Option.iter state.observer ~f:(fun observer -> observer##disconnect)
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (T)

let on_change f =
  let f { Dimensions.width; height } = f ~width ~height in
  Vdom.Attr.create_hook "size_tracker" (Hook.create f)
;;

module For_testing = struct
  module Dimensions = Dimensions
  include Hook.For_testing
end
