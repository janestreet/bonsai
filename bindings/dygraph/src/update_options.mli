open! Core
open! Import
open Gen_js_api

type t

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
val create : ?options:Options.t -> ?data:Data.t -> unit -> t
