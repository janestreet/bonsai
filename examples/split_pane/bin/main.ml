open! Core
open! Bonsai_web
open! Async_kernel

let (_ : _ Start.Handle.t) =
  Async_js.init ();
  Auto_reload.refresh_on_build ();
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    Bonsai_web_ui_split_pane_example.app
;;
