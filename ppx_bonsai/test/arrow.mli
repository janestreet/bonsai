type 'a c
type 'a v

val return_v : 'a -> 'a v

module Let_syntax : sig
  val return : 'a v -> 'a c

  module Let_syntax : sig
    val return : 'a v -> 'a c
    val sub : here:[%call_pos] -> 'a c -> f:('a v -> 'b c) -> 'b c
    val map : here:[%call_pos] -> 'a v -> f:('a -> 'b) -> 'b v
    val map2 : here:[%call_pos] -> 'a1 v -> 'a2 v -> f:('a1 -> 'a2 -> 'b) -> 'b v

    val map3
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'b)
      -> 'b v

    val map4
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b)
      -> 'b v

    val map5
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> 'a5 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b)
      -> 'b v

    val map6
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> 'a5 v
      -> 'a6 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b)
      -> 'b v

    val map7
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> 'a5 v
      -> 'a6 v
      -> 'a7 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b)
      -> 'b v

    val both : 'a v -> 'b v -> ('a * 'b) v

    val switch
      :  here:[%call_pos]
      -> match_:int v
      -> branches:int
      -> with_:(int -> 'b c)
      -> 'b c

    val arr : here:[%call_pos] -> 'a v -> f:('a -> 'b) -> 'b c
    val arr2 : here:[%call_pos] -> 'a1 v -> 'a2 v -> f:('a1 -> 'a2 -> 'b) -> 'b c

    val arr3
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'b)
      -> 'b c

    val arr4
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b)
      -> 'b c

    val arr5
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> 'a5 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b)
      -> 'b c

    val arr6
      :  here:[%call_pos]
      -> 'a1 v
      -> 'a2 v
      -> 'a3 v
      -> 'a4 v
      -> 'a5 v
      -> 'a6 v
      -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b)
      -> 'b c

    val arr7
      :  here:[%call_pos]
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
