open! Core
open! Bonsai_web
open Bonsai.Let_syntax

type t =
  { tag : string
  ; reset : unit Effect.t
  }

let component =
  let%sub tag, inject =
    Bonsai.state_machine0
      (module String)
      (module Unit)
      ~default_model:"div"
      ~apply_action:(fun ~inject:_ ~schedule_event:_ _model () ->
        match Random.int 3 with
        | 0 -> "div"
        | 1 -> "section"
        | _ -> "header")
  in
  let%arr tag = tag
  and inject = inject in
  { tag; reset = inject () }
;;
