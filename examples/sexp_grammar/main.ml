open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Auto_generated = Bonsai_web_ui_auto_generated
module Form = Bonsai_web_ui_form

let generation_count = 100

let generated_values =
  Quickcheck.random_sequence
    ~sizes:(Sequence.init generation_count ~f:(Fn.const 10))
    [%quickcheck.generator: Type.t]
  |> Fn.flip Sequence.take generation_count
  |> Sequence.to_list
;;

let component =
  let type_definition =
    Vdom.Node.pre [ Vdom.Node.text Embedded_files.type_intf_dot_ml ]
  in
  let%sub form = Type.form in
  let%sub view =
    match%sub form >>| Form.value with
    | Ok value -> Type.view value
    | Error error ->
      let%arr error = error in
      Vdom.Node.sexp_for_debugging [%message "error in form" (error : Error.t)]
  in
  let%sub index, incr =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) index () ->
        (index + 1) mod generation_count)
  in
  let%arr form = form
  and view = view
  and index = index
  and incr = incr in
  let button =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            Effect.Many [ Form.set form (List.nth_exn generated_values index); incr () ])
        ]
      [ Vdom.Node.text "Set form to a random value" ]
  in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Example type" ]
    ; type_definition
    ; Vdom.Node.h3 [ Vdom.Node.text "Auto-generated form for type" ]
    ; button
    ; Auto_generated.view_as_vdom form
    ; Vdom.Node.h3 [ Vdom.Node.text "Auto-generated view for value produced by form" ]
    ; view
    ]
;;

let () = Bonsai_web.Start.start component
