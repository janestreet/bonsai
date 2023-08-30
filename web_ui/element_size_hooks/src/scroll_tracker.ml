open! Core
open! Import
open! Bonsai_web
open! Js_of_ocaml

module Scrollable = struct
  type t =
    | Neither
    | Vertical
    | Horizontal
    | Both
  [@@deriving sexp, equal]
end

(* ResizeObserver is an unintuitive choice, but intersection oberver has issues managing
   changes off screen and has more extreme pitfalls around multiple children and where you
   attach it. *)
module Scroll_tracker = struct
  module State = struct
    type t =
      { mutable observer : ResizeObserver.resizeObserver Js.t option
      ; mutable parent : Dom_html.element Js.t option
      ; mutable scrollable : Scrollable.t
      ; mutable callback : scrollable:Scrollable.t -> unit
      }
  end

  let observe child parent ~(state : State.t) =
    (* We take the whole state here so that we can mutate the callback in it and
       witness the change in the observer *)
    let on_resize_observed _entries _observer =
      (* You might be wondering why size-changes would trigger when a scrollbar 
         is added; after all, aren't scrollbars added to an element in order to 
         keep its size the same?

         However, the demo of this hook shows that the size does in fact change, 
         and if you add a log statement here 
         {[
           Js_of_ocaml.Firebug.console##log entries;
         ]} *)
      let open Option.Let_syntax in
      let (_ : unit option) =
        let%map target = state.parent in
        let vertical = target##.scrollHeight > target##.clientHeight in
        let horizontal = target##.scrollWidth > target##.clientWidth in
        state.scrollable
          <- (match vertical, horizontal with
              | true, true -> Both
              | true, false -> Vertical
              | false, true -> Horizontal
              | false, false -> Neither);
        state.callback ~scrollable:state.scrollable
      in
      ()
    in
    let observer = ResizeObserver.observe () ~node:child ~f:on_resize_observed in
    observer##observe parent;
    observer
  ;;

  open State

  module Input = struct
    type t = Scrollable.t -> unit Vdom.Effect.t [@@deriving sexp_of]

    let combine f g event =
      Vdom.Effect.sequence_as_sibling (f event) ~unless_stopped:(fun () -> g event)
    ;;
  end

  let wrap_with_handle ~scrollable ~f =
    Vdom.Effect.Expert.handle_non_dom_event_exn (f scrollable)
  ;;

  let init input _ =
    { State.observer = None
    ; scrollable = Neither
    ; callback = wrap_with_handle ~f:input
    ; parent = None
    }
  ;;

  let on_mount _ state element =
    let open Option.Let_syntax in
    let (_ : unit option) =
      let%bind parentNode = element##.parentNode |> Js.Opt.to_option in
      let%map parentElement = Dom_html.CoerceTo.element parentNode |> Js.Opt.to_option in
      state.parent <- Some parentElement;
      (* There are two separate observers on parent and child here. This is because resize
         events could be triggered by either the child or the parent resizing. We could
         only listed to the parent except in cases where the scrollbar itself doesn't
         change the size of the parent - specific known weaknesses of this approach are
         OSX with scrollbars only on scroll, and [scrollbar-gutter: stable].*)
      state.observer <- Some (observe ~state element parentNode)
    in
    ()
  ;;

  let update ~old_input:_ ~new_input state _ =
    state.callback <- wrap_with_handle ~f:new_input;
    state.callback ~scrollable:state.scrollable
  ;;

  let destroy _ state _ =
    Option.iter state.observer ~f:(fun observer -> observer##disconnect)
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (Scroll_tracker)

let on_change f = Vdom.Attr.create_hook "scroll_tracker" (Hook.create f)

module For_testing = struct
  include Hook.For_testing
end
