type 'a c
type 'a v

val return_v : 'a -> 'a v

module Let_syntax : sig
  val return : 'a v -> 'a c

  module Let_syntax : sig
    val return : 'a v -> 'a c
    val sub : ?here:Lexing.position -> 'a c -> f:('a v -> 'b c) -> 'b c
    val map : ?here:Lexing.position -> 'a v -> f:('a -> 'b) -> 'b v
    val both : 'a v -> 'b v -> ('a * 'b) v

    val switch
      :  here:Lexing.position
      -> match_:int v
      -> branches:int
      -> with_:(int -> 'b c)
      -> 'b c

    val arr : ?here:Lexing.position -> 'a v -> f:('a -> 'b) -> 'b c
  end
end
