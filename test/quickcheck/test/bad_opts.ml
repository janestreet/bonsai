open! Core

let bad_sub (type a) (t : a Bonsai.Private.Computation.t) : a Bonsai.Private.Computation.t
  =
  match t with
  | Sub x -> x.into
  | _ -> t
;;
