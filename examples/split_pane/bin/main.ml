open! Core
open! Bonsai_web
open! Async_kernel

let () =
  Async_js.init ();
  Bonsai_web.Start.start Bonsai_web_ui_split_pane_example.app
;;
