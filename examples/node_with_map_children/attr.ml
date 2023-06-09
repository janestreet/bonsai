open! Core
open! Bonsai_web
open Bonsai.Let_syntax

type t =
  { attr : Vdom.Attr.t
  ; reset : unit Effect.t
  }

let component =
  let%sub attr, inject =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: opaque]
      ~equal:phys_equal
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~default_model:Vdom.Attr.empty
      ~apply_action:(fun ~inject:_ ~schedule_event:_ _model () ->
        match Random.int 4 with
        | 0 -> Vdom.Attr.empty
        | 1 -> Vdom.Attr.create "foo" "5"
        | 2 -> Vdom.Attr.create "foo" "6"
        | _ -> Vdom.Attr.css_var ~name:"test" "foo")
  in
  let%arr attr = attr
  and inject = inject in
  { attr; reset = inject () }
;;
