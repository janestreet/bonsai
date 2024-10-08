type 'a v = 'a
type 'a c = 'a

let return_v x = x

module Impl = struct
  let return x = x
  let sub ?here:(_ = Stdlib.Lexing.dummy_pos) x ~f = f x
  let map ?here:(_ = Stdlib.Lexing.dummy_pos) x ~f = f x
  let both a b = a, b
  let switch ?here:(_ = Stdlib.Lexing.dummy_pos) ~match_ ~branches:_ ~with_ = with_ match_
  let arr ?here:(_ = Stdlib.Lexing.dummy_pos) x ~f = return (f x)
  let arr2 ?here:(_ = Stdlib.Lexing.dummy_pos) x1 x2 ~f = return (f x1 x2)
  let arr3 ?here:(_ = Stdlib.Lexing.dummy_pos) x1 x2 x3 ~f = return (f x1 x2 x3)
  let arr4 ?here:(_ = Stdlib.Lexing.dummy_pos) x1 x2 x3 x4 ~f = return (f x1 x2 x3 x4)

  let arr5 ?here:(_ = Stdlib.Lexing.dummy_pos) x1 x2 x3 x4 x5 ~f =
    return (f x1 x2 x3 x4 x5)
  ;;

  let arr6 ?here:(_ = Stdlib.Lexing.dummy_pos) x1 x2 x3 x4 x5 x6 ~f =
    return (f x1 x2 x3 x4 x5 x6)
  ;;

  let arr7 ?here:(_ = Stdlib.Lexing.dummy_pos) x1 x2 x3 x4 x5 x6 x7 ~f =
    return (f x1 x2 x3 x4 x5 x6 x7)
  ;;
end

module Let_syntax = struct
  let return = Impl.return

  module Let_syntax = Impl
end
