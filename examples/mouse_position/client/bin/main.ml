open! Core
open! Async_kernel
open Bonsai_web
open Bonsai_examples_mouse_position_lib

let run () =
  Async_js.init ();
  Start.start App.app;
  Deferred.never ()
;;

let () = don't_wait_for (run ())
