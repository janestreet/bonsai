open! Core
open! Bonsai_web.Cont

let component _graph =
  Bonsai.return
    (Vdom.Node.div
       ~attrs:
         [ Vdom.Attr.Global_listeners.beforeunload (fun _ -> Effect.return `Show_warning)
         ]
       [ Vdom.Node.text "attempting to leave this page will show a warning" ])
;;

let () = Bonsai_web.Start.start component
