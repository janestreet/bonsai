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
      (module struct
        type t = Vdom.Attr.t

        let sexp_of_t, t_of_sexp, equal = sexp_of_opaque, opaque_of_sexp, phys_equal
      end)
      (module Unit)
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
