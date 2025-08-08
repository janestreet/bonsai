open! Core
open! Import

type t

val empty : t
val add_exn : t -> key:'a Type_equal.Id.t -> data:'a Incr.t -> t
val add_overwriting : t -> key:'a Type_equal.Id.t -> data:'a Incr.t -> t
val find : t -> 'a Type_equal.Id.t -> 'a Incr.t option

(** A [Recursive.t] is a mapping from [Fix_id.t] to a copy of the environment at the point
    where the [Fix_define] for that [Fix_id.t] was evaluated, as well as the resolved
    [May_contain] for that [Fix_define].

    Why do we have enough information when gathering a [Fix_define] to resolve its
    dependencies? A [Fix_define] can only contain [Fix_recurse]s corresponding to itself
    and any enclosing [Fix_define]s. The outermost [Fix_define] has no enclosing
    [Fix_define]s and therefore we can resolve it: it resolves to the [May_contain] of its
    body ignoring the recursive dependency on itself. Induction on the number of enclosing
    [Fix_defines] proves we can resolve _any_ [Fix_define]'s [May_contain]. *)
module Recursive : sig
  type env := t

  type entry =
    { environment : env
    ; resolved_contain : May_contain.Resolved.t
    }

  type t

  val empty : t
  val add_exn : t -> key:'a Fix_id.t -> data:entry -> t
  val add_overwriting : t -> key:'a Fix_id.t -> data:entry -> t
  val find_exn : t -> 'a Fix_id.t -> entry

  (** [resolve_may_contain] takes a [t] and a [May_contain.Unresolved.t] and looks up any
      required information about the [resolved_contain] of any recursive dependencies to
      resolve it. *)
  val resolve_may_contain : t -> May_contain.Unresolved.t -> May_contain.Resolved.t
end
