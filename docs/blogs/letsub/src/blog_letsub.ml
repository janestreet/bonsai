open Core

(* $MDX part-begin=lang *)
type t =
  | Const of int
  | Add of t * t
  | Map of
      { t : t
      ; f : int -> int
      }

let const i = Const i
let add x y = Add (x, y)
let map t ~f = Map { t; f }

let rec eval : t -> int = function
  | Const i -> i
  | Add (x, y) -> eval x + eval y
  | Map { t; f } -> f (eval t)
;;

(* $MDX part-end *)

(* $MDX part-begin=use1 *)
let print_and_return i =
  printf "got here\n";
  i
;;

let x = map (add (const 5) (const 1)) ~f:(fun i -> print_and_return (i / 2))
let doubled = map x ~f:(fun i -> i * 2)
let tripled = map x ~f:(fun i -> i * 3)
let y = add doubled tripled
let () = printf "y = %d\n" (eval y)
(* $MDX part-end *)

(* $MDX part-begin=applicative *)
module type Applicative = sig
  type 'a t

  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
end
(* $MDX part-end *)

(* $MDX part-begin=arrow *)
module type Arrow = sig
  type ('a, 'b) t

  val arr : ('a -> 'b) -> ('a, 'b) t

  (* Combine two arrows in series *)
  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  (* Combine two arrows in parallel *)
  val ( &&& ) : ('a, 'b1) t -> ('a, 'b2) t -> ('a, 'b1 * 'b2) t
end
(* $MDX part-end *)

module M2 = struct
  (* $MDX part-begin=cache *)
  type t =
    | Const of int
    | Add of t * t
    | Map of
        { t : t
        ; f : int -> int
        }
    | Cached of { mutable value : [ `Not_yet_evaluated of t | `Evaluated of int ] }

  let const i = Const i
  let add x y = Add (x, y)
  let map t ~f = Map { t; f }
  let cache t = Cached { value = `Not_yet_evaluated t }

  let eval t =
    let rec eval = function
      | Const i -> i
      | Add (x, y) -> eval x + eval y
      | Map { t; f } -> f (eval t)
      | Cached cached_value ->
        (match cached_value.value with
         | `Evaluated i -> i
         | `Not_yet_evaluated t ->
           let i = eval t in
           cached_value.value <- `Evaluated i;
           i)
    in
    eval t
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=cache-test *)
  let%expect_test "" =
    let x = map (add (const 5) (const 1)) ~f:(fun i -> print_and_return (i / 2)) in
    let x = cache x in
    let doubled = map x ~f:(fun i -> i * 2) in
    let tripled = map x ~f:(fun i -> i * 3) in
    let y = add doubled tripled in
    printf "y = %d\n" (eval y);
    [%expect {|
      got here
      y = 15 |}]
  ;;
  (* $MDX part-end *)
end

module Bonsai_version1 = struct
  (* $MDX part-begin=bonsai1 *)
  module Uid = Unique_id.Int ()

  type t =
    | Const of int
    | Precomputed_value of Uid.t
    | Add of t * t
    | Map of
        { t : t
        ; f : int -> int
        }
    | Sub of
        { thing_to_precompute : t
        ; name : Uid.t
        ; body_that_uses_precomputed_thing : t
        }

  let const i = Const i
  let add x y = Add (x, y)
  let map t ~f = Map { t; f }

  let sub t ~f =
    let name = Uid.create () in
    Sub
      { thing_to_precompute = t
      ; name
      ; body_that_uses_precomputed_thing = f (Precomputed_value name)
      }
  ;;

  let eval t =
    let rec eval ~scope = function
      | Const i -> i
      | Precomputed_value name -> Map.find_exn scope name
      | Add (x, y) -> eval ~scope x + eval ~scope y
      | Map { t; f } -> f (eval ~scope t)
      | Sub { thing_to_precompute; name; body_that_uses_precomputed_thing } ->
        let i = eval ~scope thing_to_precompute in
        let scope = Map.set scope ~key:name ~data:i in
        eval ~scope body_that_uses_precomputed_thing
    in
    eval ~scope:Uid.Map.empty t
  ;;

  let%expect_test "" =
    let x = map (add (const 5) (const 1)) ~f:(fun i -> print_and_return (i / 2)) in
    let y =
      sub x ~f:(fun x ->
        let doubled = map x ~f:(fun i -> i * 2) in
        let tripled = map x ~f:(fun i -> i * 3) in
        add doubled tripled)
    in
    printf "y = %d\n" (eval y);
    [%expect {|
      got here
      y = 15 |}]
  ;;
  (* $MDX part-end *)
end

module Bonsai_version2 = struct
  module Uid = Unique_id.Int ()

  (* $MDX part-begin=bonsai2 *)
  type precomputed_value = Uid.t

  type t =
    | Const of int
    | Add of t * t
    | Arr of
        { value : precomputed_value
        ; f : int -> int
        }
    | Sub of
        { bound : t
        ; name : Uid.t
        ; body : t
        }

  let const i = Const i
  let add x y = Add (x, y)
  let arr value ~f = Arr { value; f }

  let sub t ~f =
    let name = Uid.create () in
    Sub { bound = t; name; body = f name }
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=eval2 *)
  let eval t =
    let rec eval ~scope = function
      | Const i -> i
      | Add (x, y) -> eval ~scope x + eval ~scope y
      | Arr { value; f } ->
        let i = Map.find_exn scope value in
        f i
      | Sub { bound; name; body } ->
        let i = eval ~scope bound in
        let scope = Map.set scope ~key:name ~data:i in
        eval ~scope body
    in
    eval ~scope:Uid.Map.empty t
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=test2 *)
  let%expect_test "" =
    let x =
      sub
        (add (const 5) (const 1))
        ~f:(fun sum -> arr sum ~f:(fun i -> print_and_return (i / 2)))
    in
    let y =
      sub x ~f:(fun x ->
        let doubled = arr x ~f:(fun i -> i * 2) in
        let tripled = arr x ~f:(fun i -> i * 3) in
        add doubled tripled)
    in
    printf "y = %d\n" (eval y);
    [%expect {|
      got here
      y = 15 |}]
  ;;
  (* $MDX part-end *)
end

open Bonsai
open Bonsai.Let_syntax

(* $MDX part-begin=create_arrow *)
module Arrow_from_bonsai : sig
  type ('a, 'b) t = 'a Value.t -> 'b Computation.t

  val arr : ('a -> 'b) -> ('a, 'b) t
  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( &&& ) : ('a, 'b1) t -> ('a, 'b2) t -> ('a, 'b1 * 'b2) t
end = struct
  type ('a, 'b) t = 'a Value.t -> 'b Computation.t

  let arr f value =
    let%arr value = value in
    f value
  ;;

  let compose t1 t2 value =
    let%sub result = t1 value in
    t2 result
  ;;

  let ( &&& ) t1 t2 value =
    let%sub result1 = t1 value in
    let%sub result2 = t2 value in
    let%arr result1 = result1
    and result2 = result2 in
    result1, result2
  ;;
end
(* $MDX part-end *)
