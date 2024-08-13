open! Base

(* NOTE: Bonsai uses its own [Applicative] module so that it can make use of [%call_pos]. *)
module type S = sig
  type 'a t

  val return : ?here:Stdlib.Lexing.position -> 'a -> 'a t
  val map : ?here:Stdlib.Lexing.position -> 'a t -> f:('a -> 'b) -> 'b t
  val both : ?here:Stdlib.Lexing.position -> 'a t -> 'b t -> ('a * 'b) t
  val ( <*> ) : ?here:Stdlib.Lexing.position -> ('a -> 'b) t -> 'a t -> 'b t
  val ( <* ) : ?here:Stdlib.Lexing.position -> 'a t -> unit t -> 'a t
  val ( *> ) : ?here:Stdlib.Lexing.position -> unit t -> 'a t -> 'a t
  val ( >>| ) : ?here:Stdlib.Lexing.position -> 'a t -> ('a -> 'b) -> 'b t
  val apply : ?here:Stdlib.Lexing.position -> ('a -> 'b) t -> 'a t -> 'b t
  val map2 : ?here:Stdlib.Lexing.position -> 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val map3
    :  ?here:Stdlib.Lexing.position
    -> 'a t
    -> 'b t
    -> 'c t
    -> f:('a -> 'b -> 'c -> 'd)
    -> 'd t

  val all : ?here:Stdlib.Lexing.position -> 'a t list -> 'a list t
  val all_unit : ?here:Stdlib.Lexing.position -> unit t list -> unit t

  module Applicative_infix : sig
    val ( <*> ) : ?here:Stdlib.Lexing.position -> ('a -> 'b) t -> 'a t -> 'b t
    val ( <* ) : ?here:Stdlib.Lexing.position -> 'a t -> unit t -> 'a t
    val ( *> ) : ?here:Stdlib.Lexing.position -> unit t -> 'a t -> 'a t
    val ( >>| ) : ?here:Stdlib.Lexing.position -> 'a t -> ('a -> 'b) -> 'b t
  end
end

module type Let_syntax = sig
  type 'a t

  module Open_on_rhs_intf : sig
    module type S
  end

  module Let_syntax : sig
    val return : ?here:Stdlib.Lexing.position -> 'a -> 'a t
    val ( <*> ) : ?here:Stdlib.Lexing.position -> ('a -> 'b) t -> 'a t -> 'b t
    val ( <* ) : ?here:Stdlib.Lexing.position -> 'a t -> unit t -> 'a t
    val ( *> ) : ?here:Stdlib.Lexing.position -> unit t -> 'a t -> 'a t
    val ( >>| ) : ?here:Stdlib.Lexing.position -> 'a t -> ('a -> 'b) -> 'b t

    module Let_syntax : sig
      val return : ?here:Stdlib.Lexing.position -> 'a -> 'a t
      val map : ?here:Stdlib.Lexing.position -> 'a t -> f:('a -> 'b) -> 'b t
      val both : ?here:Stdlib.Lexing.position -> 'a t -> 'b t -> ('a * 'b) t

      module Open_on_rhs : Open_on_rhs_intf.S
    end
  end
end

module type Basic_using_map2 = sig
  type 'a t

  val return : ?here:Stdlib.Lexing.position -> 'a -> 'a t
  val map2 : ?here:Stdlib.Lexing.position -> 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val map
    : [ `Define_using_map2
      | `Custom of ?here:Stdlib.Lexing.position -> 'a t -> f:('a -> 'b) -> 'b t
      ]
end

module Make_using_map2 (X : Basic_using_map2) : S with type 'a t := 'a X.t = struct
  include X

  let apply ?(here = Stdlib.Lexing.dummy_pos) tf ta = map2 ~here tf ta ~f:(fun f a -> f a)
  let ( <*> ) = apply

  let derived_map ?(here = Stdlib.Lexing.dummy_pos) t ~f =
    ( <*> ) ~here (return ~here f) t
  ;;

  let map =
    match X.map with
    | `Define_using_map2 -> derived_map
    | `Custom x -> x
  ;;

  let ( >>| ) ?(here = Stdlib.Lexing.dummy_pos) t f = map ~here t ~f
  let both ?(here = Stdlib.Lexing.dummy_pos) ta tb = map2 ~here ta tb ~f:(fun a b -> a, b)

  let map3 ?(here = Stdlib.Lexing.dummy_pos) ta tb tc ~f =
    map2 ~here (map2 ~here ta tb ~f) tc ~f:(fun fab c -> fab c)
  ;;

  let all ?(here = Stdlib.Lexing.dummy_pos) ts =
    List.fold_right ts ~init:(return ~here []) ~f:(map2 ~here ~f:(fun x xs -> x :: xs))
  ;;

  let ( *> ) ?(here = Stdlib.Lexing.dummy_pos) u v = map2 ~here u v ~f:(fun () y -> y)
  let ( <* ) ?(here = Stdlib.Lexing.dummy_pos) u v = map2 ~here u v ~f:(fun x () -> x)

  let all_unit ?(here = Stdlib.Lexing.dummy_pos) ts =
    List.fold ts ~init:(return ~here ()) ~f:(fun acc b -> ( *> ) ~here acc b)
  ;;

  module Applicative_infix = struct
    let ( <*> ) = ( <*> )
    let ( *> ) = ( *> )
    let ( <* ) = ( <* )
    let ( >>| ) = ( >>| )
  end
end

module type Applicative_infix = sig
  type 'a t
  type ('a, 'b) fn := 'a -> 'b

  val ( <*> ) : ?here:Stdlib.Lexing.position -> ('a -> 'b) t -> 'a t -> 'b t
  val ( <* ) : ?here:Stdlib.Lexing.position -> 'a t -> unit t -> 'a t
  val ( *> ) : ?here:Stdlib.Lexing.position -> unit t -> 'a t -> 'a t
  val ( >>| ) : ?here:Stdlib.Lexing.position -> 'a t -> ('a -> 'b, 'b t) fn
end
