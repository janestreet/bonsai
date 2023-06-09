open! Core
open! Async_kernel
open! Bonsai_web
open! Bonsai.Let_syntax

(* $MDX part-begin=after_display *)
let frame_counter =
  let%sub frames, set_frames = Bonsai.state 0 in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~after_display:
        (let%map frames = frames
         and set_frames = set_frames in
         set_frames (frames + 1))
      ()
  in
  let%arr frames = frames in
  Vdom.Node.textf "this component has been alive for %d frames" frames
;;

(* $MDX part-end *)

let () = Util.run frame_counter ~id:"after-display"

(* $MDX part-begin=only_when_active *)
let frame_toggler =
  let%sub showing, set_showing = Bonsai.state false in
  let%sub output =
    match%sub showing with
    | true -> frame_counter
    | false -> Bonsai.const Vdom.Node.none
  in
  let%arr showing = showing
  and set_showing = set_showing
  and output = output in
  let toggle_showing = set_showing (not showing) in
  let button_text = if showing then "disable counter" else "enable counter" in
  let toggle_button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> toggle_showing) ]
      [ Vdom.Node.text button_text ]
  in
  Vdom.Node.div [ toggle_button; output ]
;;

(* $MDX part-end *)

let () = Util.run frame_toggler ~id:"only-when-active"

module Extendy = Bonsai_web_ui_extendy

(* $MDX part-begin=extendy-api *)

type 'a t = 'a Extendy.t =
  { contents : 'a Extendy.Id.Map.t
  ; append : unit Ui_effect.t
  ; set_length : int -> unit Ui_effect.t
  ; remove : Extendy.Id.t -> unit Ui_effect.t
  }

type ('a, 'b) extendy_function_signature =
  'a Computation.t -> wrap_remove:('a -> unit Ui_effect.t -> 'b) -> 'b t Computation.t

let extendy : ('a, 'b) extendy_function_signature = Extendy.component'

(* $MDX part-end *)

(* $MDX part-begin=extendy-use *)

let wrap_remove frame_counter remove =
  let x_button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> remove) ]
      [ Vdom.Node.text "x" ]
  in
  Vdom.Node.div [ x_button; frame_counter ]
;;

let many_frame_watches =
  let%sub { contents; append; _ } = extendy frame_counter ~wrap_remove in
  let%arr contents = contents
  and append = append in
  let append_button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> append) ]
      [ Vdom.Node.text "add" ]
  in
  Vdom.Node.div (append_button :: Map.data contents)
;;

(* $MDX part-end *)
let () = Util.run many_frame_watches ~id:"extendy-use"

let logger =
  let%sub logger =
    Bonsai.state_machine0
      ()
      ~sexp_of_action:[%sexp_of: String.t]
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action -> action :: model)
      ~default_model:[]
  in
  let%arr log, inject = logger in
  let view = log |> List.rev |> List.map ~f:Vdom.Node.text |> Vdom.Node.span in
  view, inject
;;

(* $MDX part-begin=activations *)
let frame_counter (log : (string -> unit Ui_effect.t) Value.t) =
  let%sub frames, set_frames = Bonsai.state 0 in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map log = log in
         log "🚀")
      ~on_deactivate:
        (let%map log = log in
         log "🔥")
      ~after_display:
        (let%map frames = frames
         and set_frames = set_frames in
         set_frames (frames + 1))
      ()
  in
  let%arr frames = frames in
  Vdom.Node.textf "this component has been alive for %d frames" frames
;;

(* $MDX part-end *)

(* $MDX part-begin=extendy-use-2 *)
let many_frame_watches =
  let%sub log_view, log = logger in
  let%sub { contents; append; _ } =
    extendy (frame_counter log) ~wrap_remove:(fun frame_counter remove ->
      let x_button =
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> remove) ]
          [ Vdom.Node.text "x" ]
      in
      Vdom.Node.div [ x_button; frame_counter ])
  in
  let%arr contents = contents
  and append = append
  and log_view = log_view in
  let append_button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> append) ]
      [ Vdom.Node.text "add" ]
  in
  Vdom.Node.div (append_button :: log_view :: Map.data contents)
;;

(* $MDX part-end *)
let () = Util.run many_frame_watches ~id:"extendy-use-2"

(* $MDX part-begin=on_change_type *)
type 'a on_change_function_signature =
  ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a Value.t
  -> callback:('a option -> 'a -> unit Ui_effect.t) Value.t
  -> unit Computation.t

let _on_change' : _ on_change_function_signature = Bonsai.Edge.on_change'

(* $MDX part-end *)

(* $MDX part-begin=on_change_prime *)
let on_change'
      (type a)
      (module M : Bonsai.Model with type t = a)
      ~equal
      (current_value : a Value.t)
      ~(callback : (a option -> a -> unit Effect.t) Value.t)
  =
  let%sub previous_value, set_previous_value =
    Bonsai.state_opt () ~sexp_of_model:[%sexp_of: M.t] ~equal
  in
  let%sub after_display =
    match%sub previous_value with
    | None ->
      let%arr set_previous_value = set_previous_value
      and current_value = current_value
      and callback = callback in
      Ui_effect.Many
        [ set_previous_value (Some current_value); callback None current_value ]
    | Some previous_value ->
      let%arr previous_value = previous_value
      and set_previous_value = set_previous_value
      and current_value = current_value
      and callback = callback in
      if equal previous_value current_value
      then Ui_effect.Ignore
      else
        Ui_effect.Many
          [ set_previous_value (Some current_value)
          ; callback (Some previous_value) current_value
          ]
  in
  Bonsai.Edge.lifecycle ~after_display ()
;;

(* $MDX part-end *)

let _ = on_change'

let counter =
  let%sub state, set_state = Bonsai.state 0 in
  let%arr state = state
  and set_state = set_state in
  let decrement =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> set_state (state - 1)) ]
      [ Vdom.Node.text "-1" ]
  in
  let increment =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> set_state (state + 1)) ]
      [ Vdom.Node.text "+1" ]
  in
  Vdom.Node.div [ decrement; Vdom.Node.textf "%d" state; increment ], state
;;

(* $MDX part-begin=logging_counter *)
let logging_counter =
  let%sub log_view, log = logger in
  let%sub counter_view, counter = counter in
  let%sub () =
    let callback =
      let%map log = log in
      fun prev cur ->
        match prev with
        | None -> Ui_effect.Ignore
        | Some prev -> log (if prev < cur then "🚀" else "🔥")
    in
    Bonsai.Edge.on_change'
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      counter
      ~callback
  in
  let%arr log_view = log_view
  and counter_view = counter_view in
  Vdom.Node.div [ counter_view; log_view ]
;;

(* $MDX part-end *)
let () = Util.run logging_counter ~id:"logging-counter"
