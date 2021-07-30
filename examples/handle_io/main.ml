open! Core
open! Async_kernel
open! Bonsai_web

let print_effect = Effect.of_sync_fun print_endline

let component =
  let on_click = print_effect "hello world" in
  Bonsai.const
    (Vdom.Node.button
       ~attr:(Vdom.Attr.on_click (fun _ -> on_click))
       [ Vdom.Node.text "click me to print a thing to the console" ])
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
