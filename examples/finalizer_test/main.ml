open! Core
open Js_of_ocaml

let count = ref 0

let (_ : _) =
  let f () =
    let x = [ !count ] in
    incr count;
    Gc.Expert.add_finalizer_last_exn x (fun () -> decr count)
  in
  Dom_html.window##setInterval (Js.wrap_callback f) 1.0
;;

let (_ : _) =
  let f () = Dom_html.document##.body##.innerText := Js.string (Int.to_string !count) in
  Dom_html.window##setInterval (Js.wrap_callback f) 16.0
;;
