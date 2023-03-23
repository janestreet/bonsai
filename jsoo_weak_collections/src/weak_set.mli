open! Js_of_ocaml
open! Gen_js_api

(** https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakSet *)

type 'a t

val create : unit -> 'a t [@@js.new "WeakSet"]
val add : 'a t -> 'a -> unit [@@js.call]
val has : 'a t -> 'a -> bool [@@js.call]
val delete : 'a t -> 'a -> unit [@@js.call]
