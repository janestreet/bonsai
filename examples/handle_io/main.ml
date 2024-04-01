open! Core
open! Async_kernel
open! Bonsai_web.Cont

let print_effect = Effect.of_sync_fun print_endline

let component _graph =
  let on_click = print_effect "hello world" in
  Bonsai.return
    (Vdom.Node.button
       ~attrs:[ Vdom.Attr.on_click (fun _ -> on_click) ]
       [ Vdom.Node.text "click me to print a thing to the console" ])
;;

let () = Bonsai_web.Start.start component
