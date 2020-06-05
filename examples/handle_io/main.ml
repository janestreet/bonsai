open! Core_kernel
open! Async_kernel
open! Bonsai_web

let print_effect = Effect.of_sync_fun print_endline |> unstage

let component =
  let on_click = Effect.inject_ignoring_response (print_effect "hello world") in
  Bonsai.Proc.const
    (Vdom.Node.button
       [ Vdom.Attr.on_click (fun _ -> on_click) ]
       [ Vdom.Node.text "click me to print a thing to the console" ])
;;

let (_ : _ Start.Proc.Handle.t) =
  Start.Proc.start
    Start.Proc.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    component
;;
