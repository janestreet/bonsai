open! Core
open! Bonsai_web.Cont
open Js_of_ocaml

let () = Async_js.init ()

let run ?custom_connector ~id computation =
  (* Because we're iframing into this app from docpub, we look up what the
     current url-hash is, and only run the requested example. *)
  let current_hash = Dom_html.window##.location##.hash |> Js.to_string in
  print_s [%message (current_hash : string) (id : string)];
  if String.equal current_hash ("#" ^ id)
  then Start.start ?custom_connector computation
  else ()
;;

let run_vdom_val vdom = run (fun _ -> vdom)
let run_vdom vdom = run (fun _ -> Bonsai.return vdom)
