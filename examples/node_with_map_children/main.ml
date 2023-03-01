open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component =
  let%sub { tag; reset = reset_tag } = Tag.component in
  let%sub { attr; reset = reset_attr } = Attr.component in
  let%sub { out = before_state; view = before_view; reset = reset_before } =
    Color_list.component "before"
  in
  let%sub { out = after_state; view = after_view; reset = reset_after } =
    Color_list.component "after"
  in
  let%sub { state; view = tweener; is_automating = is_running; is_done; step } =
    Stepper.component ~before_state ~after_state
  in
  let%sub reset_all =
    let%arr reset_before = reset_before
    and reset_after = reset_after
    and reset_tag = reset_tag
    and reset_attr = reset_attr in
    Effect.Many [ reset_before; reset_after; reset_tag; reset_attr ]
  in
  let%sub () = Automator.component ~is_running ~step ~is_done ~reset_all in
  let%sub comparison = Comparison.view ~tag ~attr state in
  let%arr before_view = before_view
  and after_view = after_view
  and tweener = tweener
  and comparison = comparison in
  Vdom.Node.div ~attr:Style.app [ before_view; after_view; tweener; comparison ]
;;

let () = Bonsai_web.Start.start component
