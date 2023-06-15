open! Core
open! Import
open! Bonsai_web
open! Js_of_ocaml
module Dims = Size_tracker.For_testing.Dimensions

module Behavior = struct
  type t =
    | Grow_to_fill
    | Shrink_to_avoid_overflow
    | Grow_or_shrink_to_match_parent_size
  [@@deriving sexp, equal]
end

module T = struct
  module Input = struct
    include Behavior

    let combine (a : Behavior.t) (b : Behavior.t) : Behavior.t =
      match a, b with
      | Grow_or_shrink_to_match_parent_size, _
      | _, Grow_or_shrink_to_match_parent_size
      | Grow_to_fill, Shrink_to_avoid_overflow
      | Shrink_to_avoid_overflow, Grow_to_fill -> Grow_or_shrink_to_match_parent_size
      | Grow_to_fill, Grow_to_fill -> Grow_to_fill
      | Shrink_to_avoid_overflow, Shrink_to_avoid_overflow -> Shrink_to_avoid_overflow
    ;;
  end

  module State = struct
    type t =
      { mutable observer : ResizeObserver.resizeObserver Js.t option
      (* [my_dims] and [parent_dims] are in their local dimensionality and are
         calculated prior to the transformation taking effect. *)
      ; mutable my_dims : Dims.t
      ; mutable parent_dims : Dims.t
      ; mutable behavior : Behavior.t
      ; me : Dom_html.element Js.t
      }
  end

  let maybe_clamp_according_to_behavior v = function
    | Behavior.Grow_or_shrink_to_match_parent_size -> v
    | Grow_to_fill -> Float.max v 1.0
    | Shrink_to_avoid_overflow -> Float.min v 1.0
  ;;

  (* [width: max-content] is useful for behaviors that can shrink because
     they won't attempt to collapse the content in other ways (like by folding text
     across multiple lines).  If you've got shrinking, you probably don't want two
     compression options fighting each other.

     [width: fit-content] is useful in the "only grows" scenario because when growing,
     it has no effect, but when it would otherwise be shrunk, it'll try to compress
     itself. *)
  let set_width_property ~(state : State.t) =
    match state.behavior with
    | Grow_to_fill -> state.me##.style##.width := Js.string "fit-content"
    | Shrink_to_avoid_overflow | Grow_or_shrink_to_match_parent_size ->
      state.me##.style##.width := Js.string "max-content"
  ;;

  (* [adjust] actually changes the transform on the target element.
     If any of the dimensions have a component that is at-or-below 0,
     we set the scale to "1.0", which functionally resets the scaling. *)
  let adjust ~(state : State.t) =
    let set_scale scale =
      state.me##.style##.transform
      := Js.string [%string "scale(%{Virtual_dom.Dom_float.to_string scale})"]
    in
    let lte_zero f = Float.(f <= 0.0) in
    if lte_zero state.my_dims.width
    || lte_zero state.my_dims.height
    || lte_zero state.parent_dims.width
    || lte_zero state.parent_dims.height
    then set_scale 1.0
    else (
      let width_rat = state.parent_dims.width /. state.my_dims.width in
      let height_rat = state.parent_dims.height /. state.my_dims.height in
      let scale =
        maybe_clamp_according_to_behavior (Float.min width_rat height_rat) state.behavior
      in
      set_scale scale)
  ;;

  let with_size box_array ~f =
    Js.Optdef.case
      (Js.array_get box_array 0)
      (fun () -> ())
      (fun box -> f { Dims.width = box##.inlineSize; height = box##.blockSize })
  ;;

  let observe ~parent ~(state : State.t) =
    let on_resize_observed entries _observer =
      for i = 0 to entries##.length - 1 do
        let entry = Js.Optdef.get (Js.array_get entries i) (fun () -> assert false) in
        if phys_equal (state.me :> Dom.node Js.t) entry##.target
        then
          (* use border-box for me *)
          with_size entry##.borderBoxSize ~f:(fun dims -> state.my_dims <- dims)
        else if phys_equal parent entry##.target
        then
          (* use content-box for parent *)
          with_size entry##.contentBoxSize ~f:(fun dims -> state.parent_dims <- dims)
      done;
      (* After processing all entries, attempt to resize *)
      adjust ~state
    in
    let obs =
      new%js ResizeObserver.resizeObserver (Js.wrap_callback on_resize_observed)
    in
    obs##observe state.me;
    obs##observe parent;
    obs
  ;;

  open State

  let init input element =
    let state =
      { State.observer = None
      ; my_dims = { width = 0.0; height = 0.0 }
      ; parent_dims = { width = 0.0; height = 0.0 }
      ; me = element
      ; behavior = input
      }
    in
    (* setting the [width] property on init (before it has been added to the
       dom) is important to get an accurate first read. *)
    set_width_property ~state;
    state
  ;;

  let on_mount _input state _element =
    Js.Opt.case
      state.me##.parentNode
      (fun () ->
         eprint_s [%message "BUG" [%here] "parent should always be set in [on_mount]"])
      (fun parent -> state.observer <- Some (observe ~parent ~state))
  ;;

  let update ~old_input ~new_input state _element =
    if [%equal: Input.t] old_input new_input then ();
    state.behavior <- new_input;
    set_width_property ~state;
    adjust ~state
  ;;

  let destroy _old_input state =
    let { State.observer; _ } = state in
    fun _element -> Option.iter observer ~f:(fun observer -> observer##disconnect)
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (T)

let attr ?(behavior = Behavior.Shrink_to_avoid_overflow) () =
  Vdom.Attr.many
    [ Vdom.Attr.create_hook "grow_to_fill" (Hook.create behavior)
    ; Vdom.Attr.style
        Css_gen.(
          (* scale from the top left instead of the center *)
          create ~field:"transform-origin" ~value:"left top"
          (* [display:inline] and [position:relative] is funky, avoid it by
             using inline-block, which should also not negatively impact block
             layout. *)
          @> display `Inline_block
          @> margin ~top:(`Px 0) ~bottom:(`Px 0) ~left:(`Px 0) ~right:(`Px 0) ()
          (* use [position: relative] to keep it from impacting flow. *)
          @> position `Relative)
    ]
;;

let attr_for_parent__recommended = Vdom.Attr.style (Css_gen.overflow `Hidden)
