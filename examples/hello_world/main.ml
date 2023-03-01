open! Core
open! Bonsai_web

let component = Bonsai.const (Vdom.Node.text "hello world")
let () = Bonsai_web.Start.start component
