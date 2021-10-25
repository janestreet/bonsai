open! Core
open! Bonsai_web

module Components : sig
  type t =
    { path : string
    ; query : string list String.Map.t
    ; fragment : string option
    }
  [@@deriving sexp, equal]

  val create
    :  ?path:string
    -> ?query:string list String.Map.t
    -> ?fragment:string option
    -> unit
    -> t
end

(** Types that implement [S] can be parsed from a url
    and unparsed into a url. *)
module type S = sig
  type t [@@deriving sexp, equal]

  val parse_exn : Components.t -> t
  val unparse : t -> Components.t
end

module type S_via_sexp = sig
  type t [@@deriving sexp, equal]
end

(** Types that are sexpable can be turned a URL by storing the
    value as a sexp in a query parameter. *)
module Literally_just_a_gigantic_sexp (M : S_via_sexp) : S with type t = M.t

type 'a t

(** Creating a url-var involves passing in a module that implements [S], but
    also a fallback value which will be used if [S.parse_exn] throws. *)
val create_exn : (module S with type t = 'a) -> fallback:'a -> 'a t

val update : 'a t -> f:('a -> 'a) -> unit
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a
val value : 'a t -> 'a Value.t

(** By asking for the [Url_var.t]'s effect, you get a function that can be easily threaded
    through your components and triggered inside an action-application or inside of an
    event listener. *)
val set_effect : 'a t -> 'a -> unit Effect.t
