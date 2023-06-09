open! Core
open Virtual_dom
open Js_of_ocaml

include Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t =
        { mutable current_transform : int
        ; mutable anim_frame_id : Dom_html.animation_frame_request_id option
        }
    end

    module Input = struct
      type t = unit [@@deriving sexp_of]

      let combine _ _ = ()
    end

    let safe_screen_margin = 12

    let step (state : State.t) element =
      let frame_width, frame_offset =
        Dom_html.window##.innerWidth, 0
      in
      let element_width = element##.clientWidth in
      (* What if it's off screen to the top, bottom, or right?

         These are more tricky I think, especially if you fall out at the
         bottom, generally the document will just become scrollable. But I'm thinking
         we may want to do something like store the [direction] and then only shift
         along the orthognal axis (i.e. if you're [Top]/[Bottom] we reflow along left to
         right, and vice versa for [Left]/[Right].)

         Or maybe, we only push the element down if it overflows at the top, but not up? *)
      let offset =
        (* Remove the transform from the offset, since we're setting it here. *)
        let total_offset = element##getBoundingClientRect##.left |> Float.to_int in
        total_offset - state.current_transform - frame_offset
      in
      let total_width = offset + element_width + safe_screen_margin in
      let delta =
        if frame_width < total_width
        then frame_width - total_width
        else if offset - safe_screen_margin < 0
        then -(offset - safe_screen_margin)
        else 0
      in
      if state.current_transform <> delta
      then (
        (* only update the style if the delta is different. *)
        state.current_transform <- delta;
        element##.style##.transform := Js.string [%string "translate(%{delta#Int}px, 0px)"])
    ;;

    let reposition_until_stopped state ~element =
      let rec loop () =
        step state element;
        (* Repeat this for every frame. *)
        let next_frame_anim_id =
          Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> loop ()))
        in
        state.anim_frame_id <- Some next_frame_anim_id
      in
      (* Unset the elements inline opacity value set before; we don't set it to
         [100%] since we want it to respect values set e.g. via a CSS stylesheet. *)
      element##.style##.opacity := Js.Optdef.return (Js.string "");
      loop ()
    ;;

    let init () element =
      (* Hide the element before we fully re-flow it, to prevent it from jumping around
         on the first paint. *)
      element##.style##.opacity := Js.Optdef.return (Js.string "0");
      { State.anim_frame_id = None; current_transform = 0 }
    ;;

    let on_mount () state element = reposition_until_stopped state ~element
    let update ~old_input:() ~new_input:() _state _element = ()

    let destroy () (state : State.t) element =
      Option.iter state.anim_frame_id ~f:(fun anim_frame_id ->
        Dom_html.window##cancelAnimationFrame anim_frame_id);
      element##.style##.transform := Js.string ""
    ;;
  end)
