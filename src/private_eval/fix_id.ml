open! Core

type 'a t = 'a Type_equal.Id.t [@@deriving sexp_of]

let create () = Type_equal.Id.create ~name:"fix-id" sexp_of_opaque
let type_id = Fn.id
