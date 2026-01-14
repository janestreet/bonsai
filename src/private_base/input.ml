open! Core
open! Import

(* This type represents a combination of static and dynamic data. When only one is
   present, the representation is straightforward, but when both are present, we use the
   [Join] constructor. [Join] contains the dynamic portion as a single incremental node
   (which probably contains a tuple-tree built up with several [both] nodes), and the
   static portion is contained implicitly within the function, call it [f]. [f] rearranges
   the ['a] by inserting all the pieces of static data into their appropriates places in
   the tuple tree. *)
type 'input t =
  | Dynamic : 'input Incr.t -> 'input t
  | Static : 'input -> 'input t
  | Join : 'a Incr.t * ('a -> 'b) -> 'b t

let dynamic input = Dynamic input
let static = Static ()
let static_none = Static None

let map t ~f =
  match t with
  | Dynamic input -> Dynamic (Incr.map input ~f)
  | Static input -> Static (f input)
  | Join (input, g) -> Join (input, fun x -> f (g x))
;;

let iter_incremental (type y) (t : y t) ~f =
  match t with
  | Static _ -> ()
  | Join (incr, _) -> f (Incr.pack incr)
  | Dynamic incr -> f (Incr.pack incr)
;;

let to_incremental = function
  | Dynamic input -> input
  | Static input -> Incr.return input
  | Join (incr, f) -> Incr.map incr ~f
;;

let merge a b =
  match a, b with
  | Dynamic a, Dynamic b -> Dynamic (Incr.both a b)
  | Dynamic a, Static b -> Join (a, fun a -> a, b)
  | Static a, Dynamic b -> Join (b, fun b -> a, b)
  | Static a, Static b -> Static (a, b)
  | Dynamic a, Join (b, f) -> Join (Incr.both a b, fun (a, b) -> a, f b)
  | Static a, Join (b, f) -> Join (b, fun b -> a, f b)
  | Join (a, f), Static b -> Join (a, fun a -> f a, b)
  | Join (a, f), Dynamic b -> Join (Incr.both a b, fun (a, b) -> f a, b)
  | Join (a, f), Join (b, g) -> Join (Incr.both a b, fun (a, b) -> f a, g b)
;;
