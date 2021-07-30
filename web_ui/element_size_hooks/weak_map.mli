open! Js_of_ocaml
open! Gen_js_api

(** https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap *)

type ('a, 'b) t

val create : unit -> ('a, 'b) t [@@js.new "WeakMap"]
val set : ('a, 'b) t -> 'a -> 'b -> unit [@@js.call]
val get : ('a, 'b) t -> 'a -> 'b option [@@js.call]
val delete : ('a, 'b) t -> 'a -> unit [@@js.call]
