open! Core
open! Import

type t

val empty : t
val add_exn : t -> key:'a Type_equal.Id.t -> data:'a Incr.t -> t
val add_overwriting : t -> key:'a Type_equal.Id.t -> data:'a Incr.t -> t
val find : t -> 'a Type_equal.Id.t -> 'a Incr.t option
