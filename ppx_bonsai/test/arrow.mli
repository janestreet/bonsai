type 'a c
type 'a v

val return_v : 'a -> 'a v

module Let_syntax : sig
  val return : 'a v -> 'a c

  module Let_syntax : sig
    val return : 'a v -> 'a c
    val sub : ?here:Stdlib.Lexing.position -> 'a c -> f:('a v -> 'b c) -> 'b c
    val map : ?here:Stdlib.Lexing.position -> 'a v -> f:('a -> 'b) -> 'b v
    val both : 'a v -> 'b v -> ('a * 'b) v

    val switch
      :  ?here:Stdlib.Lexing.position
      -> match_:int v
      -> branches:int
      -> with_:(int -> 'b c)
      -> 'b c

    val arr : ?here:Stdlib.Lexing.position -> 'a v -> f:('a -> 'b) -> 'b c

    val arr2
      :  ?here:Stdlib.Lexing.position
      -> 'a1 v
      -> 'a2 v
      -> f:('a1 -> 'a2 -> 'b)
      -> 'b c

    val arr3
      :  ?here:Stdlib.Lexing.position
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'b)
      -> 'b c

    val arr4
      :  ?here:Stdlib.Lexing.position
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b)
      -> 'b c

    val arr5
      :  ?here:Stdlib.Lexing.position
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> 'a5 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b)
      -> 'b c

    val arr6
      :  ?here:Stdlib.Lexing.position
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> 'a5 v
      -> 'a6 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b)
      -> 'b c

    val arr7
      :  ?here:Stdlib.Lexing.position
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> 'a5 v
      -> 'a6 v
      -> 'a7 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b)
      -> 'b c
  end
end
