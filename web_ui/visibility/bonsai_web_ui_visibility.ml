open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Js_of_ocaml

(* This Id is used to identify instances of hooks.  This is so that a state-machine can
   keep all the visibility information for instances separate from one another. *)
module Id = Unique_id.Int63 ()

module Action = struct
  type action =
    | Set_visible
    | Set_hidden
    | Install
    | Remove
  [@@deriving sexp_of]

  type t = Id.t * action [@@deriving sexp_of]
end

module T = struct
  module Input = struct
    type t = Action.t -> unit Effect.t [@@deriving sexp_of]

    (* Always schedule both because we don't want one consumer to be able to
       block another from receiving an action. *)
    let combine left right action = Effect.Many [ left action; right action ]
  end

  module State = struct
    type t =
      { mutable callback : Action.action -> unit Effect.t
      ; mutable last_state : [ `Visible | `Hidden ] option
      ; observer : IntersectionObserver.intersectionObserver Js.t
      ; id : Id.t
      }
  end

  let process_entries
        (state : State.t Lazy.t)
        (entries : IntersectionObserver.intersectionObserverEntry Js.t Js.js_array Js.t)
        _observer
    =
    let state = Lazy.force state in
    Array.iter (Js.to_array entries) ~f:(fun entry ->
      let new_state = if Js.to_bool entry##.isIntersecting then `Visible else `Hidden in
      (match state.last_state, new_state with
       | Some `Visible, `Visible -> ()
       | Some `Hidden, `Hidden -> ()
       | _, `Visible ->
         Effect.Expert.handle_non_dom_event_exn (state.callback Set_visible)
       | _, `Hidden -> Effect.Expert.handle_non_dom_event_exn (state.callback Set_hidden));
      state.last_state <- Some new_state);
    ()
  ;;

  let init callback element =
    let id = Id.create () in
    let callback action = callback (id, action) in
    let rec state =
      lazy
        (let options = IntersectionObserver.empty_intersection_observer_options () in
         (* A threshold of [0, 1] means "notify me when it crosses the visible-to-hidden
            boundary" *)
         options##.threshold := Js.array [| 0.0; 1.0 |];
         let observer =
           new%js IntersectionObserver.intersectionObserver
             (Js.wrap_callback (process_entries state))
             options
         in
         observer##observe element;
         { State.callback; observer; last_state = None; id })
    in
    (* start by sending an 'install' message to our consumer *)
    Effect.Expert.handle_non_dom_event_exn (callback Install);
    Lazy.force state
  ;;

  let destroy _input (state : State.t) _element =
    state.observer##disconnect;
    (* send 'remove' message *)
    Effect.Expert.handle_non_dom_event_exn (state.callback Remove)
  ;;

  let update ~old_input ~new_input state _element =
    (* if [old_input] and [new_input] are the same, then we have nothing to do *)
    if not (phys_equal old_input new_input)
    then (
      let callback action = new_input (state.State.id, action) in
      state.callback <- callback;
      (* Our [old_input] may have had an injection function that will be expecting a
         [Remove], so we send one regardless of if it's necessary or not. *)
      Effect.Expert.handle_non_dom_event_exn (old_input (state.id, Remove));
      (* If we got a new callback input (or if we just sent a [Remove] to an existing
         consumer), we should follow up with the last state that we just saw. *)
      Effect.Expert.handle_non_dom_event_exn
        (callback
           (match state.last_state with
            | None -> Install
            | Some `Hidden -> Set_hidden
            | Some `Visible -> Set_visible)));
    ()
  ;;

  let on_mount _input _state _element = ()
end

module Hook = Vdom.Attr.Hooks.Make (T)

let attr inject = Vdom.Attr.create_hook "visibility-tracker" (Hook.create inject)

module Model = struct
  type state =
    | Installed
    | Visible
    | Hidden
  [@@deriving sexp, equal]

  type t = state Map.M(Id).t [@@deriving sexp, equal]
end

module Tracker = struct
  type t =
    | Visible
    | Hidden
    | Unknown

  let component =
    let%sub state, inject =
      Bonsai.state_machine0
        ()
        ~sexp_of_model:[%sexp_of: Model.t]
        ~equal:[%equal: Model.t]
        ~sexp_of_action:[%sexp_of: Action.t]
        ~default_model:(Map.empty (module Id))
        ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) map -> function
          | id, Install -> Map.set map ~key:id ~data:Installed
          | id, Remove -> Map.remove map id
          | id, Set_visible -> Map.set map ~key:id ~data:Visible
          | id, Set_hidden -> Map.set map ~key:id ~data:Hidden)
    in
    let%sub attr = Bonsai.pure attr inject in
    let%arr attr = attr
    and state = state in
    let on_page = not (Map.is_empty state) in
    let visible =
      Map.exists state ~f:(function
        | Visible -> true
        | Installed | Hidden -> false)
    in
    let v =
      match on_page, visible with
      | false, _ -> Unknown
      | true, true -> Visible
      | true, false -> Hidden
    in
    v, attr
  ;;
end

module Vdom_model = struct
  type t = Vdom.Node.t

  let equal, sexp_of_t = phys_equal, sexp_of_opaque
end

let rec with_attr attr (vdom : Vdom.Node.t) =
  match vdom with
  | None ->
    (* Vdom.Node.none would cause the node to never be displayed; so we make a 0px x 0px
       div to put the attribute on instead. *)
    let style =
      Vdom.Attr.style Css_gen.(display `Inline_block @> width (`Px 0) @> height (`Px 0))
    in
    Vdom.Node.div ~attrs:[ style; attr ] []
  | Text _ -> Vdom.Node.span ~attrs:[ attr ] [ vdom ]
  | Element e ->
    Element (Vdom.Node.Element.map_attrs e ~f:(fun xs -> Vdom.Attr.many [ attr; xs ]))
  | Widget _ ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.style (Css_gen.display `Inline_block); attr ]
      [ vdom ]
  | Lazy { key; t } -> Lazy { key; t = Lazy.map t ~f:(with_attr attr) }
  | Fragment children -> Vdom.Node.div ~attrs:[ attr ] children
;;

let only_when_visible' ?visible_attr ?hidden_attr c =
  let with_visible_attr =
    match visible_attr with
    | Some attr -> Value.map attr ~f:with_attr
    | None -> Value.return Fn.id
  in
  let with_hidden_attr =
    match hidden_attr with
    | Some attr -> Value.map attr ~f:with_attr
    | None -> Value.return Fn.id
  in
  let%sub state, attr = Tracker.component in
  let%sub prev_vdom, set_prev_vdom =
    Bonsai.state_opt
      ()
      ~sexp_of_model:[%sexp_of: Vdom_model.t]
      ~equal:[%equal: Vdom_model.t]
  in
  let%sub vdom_and_other =
    match%sub Value.both state prev_vdom with
    (* Always render the component at least once, so even in the None case,
       we render it; this will grab the vdom for the next frame, in which we'll
       probably fall through to the other branch. *)
    | _, None | Visible, _ ->
      let%sub vdom, other = c in
      let%sub () =
        Bonsai.Edge.on_change
          ~sexp_of_model:[%sexp_of: Vdom_model.t]
          ~equal:[%equal: Vdom_model.t]
          vdom
          ~callback:
            (let%map set_prev_vdom = set_prev_vdom in
             fun v -> set_prev_vdom (Some v))
      in
      let%arr vdom = vdom
      and other = other
      and with_visible_attr = with_visible_attr in
      with_visible_attr vdom, Some other
    | (Unknown | Hidden), Some prev_vdom ->
      let%arr prev_vdom = prev_vdom
      and with_hidden_attr = with_hidden_attr in
      with_hidden_attr prev_vdom, None
  in
  let%arr vdom, other = vdom_and_other
  and attr = attr in
  with_attr attr vdom, other
;;

let only_when_visible ?visible_attr ?hidden_attr c =
  let c = Computation.map c ~f:(fun vdom -> vdom, ()) in
  let%sub vdom, _ = only_when_visible' ?visible_attr ?hidden_attr c in
  return vdom
;;
