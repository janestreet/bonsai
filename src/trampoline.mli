open! Core

type 'a t

val lazy_ : 'a t Lazy.t -> 'a t
val run : 'a t -> 'a
val return : 'a -> 'a t
val all_map : ('k, 'v t, 'cmp) Map.t -> ('k, 'v, 'cmp) Map.t t

(*_ this is a stripped-down let syntax intended to prevent you 
  from using [let%map] and [and] in the syntax.  *)
module Let_syntax : sig
  val return : 'a -> 'a t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end
end
