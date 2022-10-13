open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml

(* defined in ./validate.js *)

let validate : unit -> bool =
  Js.Unsafe.get Js.Unsafe.global (Js.string "domNodeColorValidator")
;;

let validate = Effect.of_sync_fun validate ()

let driver ~reset_all ~step ~is_done ~set_has_error =
  let%sub get_is_done = Bonsai.yoink is_done in
  Bonsai.Edge.after_display
    (let%map get_is_done = get_is_done
     and reset_all = reset_all
     and set_has_error = set_has_error
     and step = step in
     let%bind.Effect ok = validate in
     if not ok
     then set_has_error true
     else (
       let%bind.Effect () = step in
       let%bind.Effect is_done = get_is_done in
       if is_done then reset_all else Effect.Ignore))
;;

let component ~is_running ~reset_all ~step ~is_done =
  let%sub has_error, set_has_error = Bonsai.state (module Bool) ~default_model:false in
  let%sub active =
    let%arr is_running = is_running
    and has_error = has_error in
    is_running && not has_error
  in
  if%sub active then driver ~set_has_error ~reset_all ~step ~is_done else Bonsai.const ()
;;
