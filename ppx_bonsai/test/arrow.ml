type 'a v = 'a
type 'a c = 'a

let return_v x = x
let return x = x
let sub ?here:_ x ~f = f x
let map ?here:_ x ~f = f x
let both a b = a, b
let switch ~here:_ ~match_ ~branches:_ ~with_ = with_ match_
let arr ?here:_ x ~f = return (f x)

module Let_syntax = struct
  let return = return

  module Let_syntax = struct
    let return = return
    let sub = sub
    let map = map
    let both = both
    let switch = switch
    let arr = arr
  end
end
