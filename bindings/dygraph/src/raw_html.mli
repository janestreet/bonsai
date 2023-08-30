open! Core
open Import
open Gen_js_api

(** Dygraphs returns "raw html" as strings in some callbacks (see [Legend_data] and
    [legendFormatter]).  We mint this type so that the types make it more clear which
    fields are just normal strings and which are html strings. *)
type t [@@deriving compare, equal, sexp]

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
val of_string : string -> t
val view : tag:string -> t -> Vdom.Node.t
