open! Core
open Bonsai_web

module Username : sig
  type t = { username : string } [@@deriving compare, equal, fields, sexp]

  val of_user_info : User_info.t -> t
  val to_string : t -> string
end

module Input : sig
  type t

  val create
    :  choices:Username.t list
    -> on_select:(Username.t -> unit Vdom.Effect.t)
    -> t
end

val component : Input.t Value.t -> Vdom.Node.t Computation.t
