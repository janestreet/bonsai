open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Cm_ro = Bonsai_web_ui_codemirror_read_only
module Animation = Bonsai_experimental_animation

let this_source_code = [%embed_file_as_string "./main.ml"]

module _ = [%css stylesheet {| body { background: black } |}]

let back_and_forth graph =
  let%sub { value; animate } =
    Animation.Advanced.make
      ~fallback:(Bonsai.return 0)
      ~interpolate:Animation.Interpolatable.int
      graph
  in
  let forward, set_forward = Bonsai.state true graph in
  let get_forward = Bonsai.peek forward graph in
  let get_things_started =
    let%map animate = animate
    and get_forward = get_forward
    and set_forward = set_forward in
    let rec switch_directions () =
      let%bind.Effect forward =
        match%bind.Effect get_forward with
        | Active forward -> Effect.return forward
        | Inactive -> Effect.return false
      in
      let%bind.Effect () = set_forward (not forward) in
      let target = if forward then String.length this_source_code else 0 in
      let duration = `For (Time_ns.Span.of_sec 10.0) in
      animate
        ~with_:Animation.Interpolator.Linear
        ~after_finished:(switch_directions ())
        duration
        target
    in
    switch_directions ()
  in
  Bonsai.Edge.lifecycle ~on_activate:get_things_started graph;
  let%map value = value in
  String.sub this_source_code ~pos:0 ~len:value
;;

let pausable ~equal ~f graph =
  let is_paused, toggle_pause = Bonsai.toggle ~default_model:false graph in
  let previous_value, set_previous_value = Bonsai.state_opt graph in
  let result =
    match%sub is_paused, previous_value with
    | true, Some previous -> previous
    | _ ->
      let to_return = f graph in
      let callback =
        let%map set_previous_value = set_previous_value in
        fun v -> set_previous_value (Some v)
      in
      Bonsai.Edge.on_change ~equal to_return graph ~callback;
      to_return
  in
  let view =
    let%map is_paused = is_paused
    and toggle_pause = toggle_pause
    and theme = View.Theme.current graph in
    View.button theme ~on_click:toggle_pause (if is_paused then "play" else "pause")
  in
  result, view
;;

let component graph =
  let source_code, pause_controlls =
    pausable ~equal:[%equal: string] graph ~f:back_and_forth
  in
  let%map source_code = source_code
  and pause_controlls = pause_controlls in
  View.vbox
    ~attrs:[ {%css| background: white; width : fit-content; |} ]
    [ pause_controlls; Cm_ro.make ~language:OCaml ~theme:Basic_light source_code ]
;;

let () = Bonsai_web.Start.start component
