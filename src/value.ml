open! Core
open! Import
module Constant_id = Unique_id.Int ()

type _ without_position =
  | Constant : 'a * Constant_id.t -> 'a without_position
  | Incr : 'a Incr.t -> 'a without_position
  | Named : 'a Type_equal.Id.t -> 'a without_position
  | Both : 'a t * 'b t -> ('a * 'b) without_position
  | Cutoff :
      { t : 'a t
      ; equal : 'a -> 'a -> bool
      }
      -> 'a without_position
  | Map :
      { t : 'a t
      ; f : 'a -> 'b
      }
      -> 'b without_position
  | Map2 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; f : 't1 -> 't2 -> 'r
      }
      -> 'r without_position
  | Map3 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; f : 't1 -> 't2 -> 't3 -> 'r
      }
      -> 'r without_position
  | Map4 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 'r
      }
      -> 'r without_position
  | Map5 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; t5 : 't5 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 'r
      }
      -> 'r without_position
  | Map6 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; t5 : 't5 t
      ; t6 : 't6 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 'r
      }
      -> 'r without_position
  | Map7 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; t5 : 't5 t
      ; t6 : 't6 t
      ; t7 : 't7 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 'r
      }
      -> 'r without_position

and 'a t =
  { value : 'a without_position
  ; here : Source_code_position.t option
  }

let rec sexp_of_t : type a. a t -> Sexp.t =
  fun { value; _ } ->
  match value with
  | Constant _ -> [%sexp "constant"]
  | Cutoff { t; equal = _ } -> [%sexp "cutoff", (t : t)]
  | Incr _ -> [%sexp "incr"]
  | Named id -> [%sexp "named", (Type_equal.Id.name id : string)]
  | Map { t; f = _ } -> [%message "map" (t : t)]
  | Both (t1, t2) -> [%message "both" (t1 : t) (t2 : t)]
  | Map2 { t1; t2; f = _ } -> [%message "map2" (t1 : t) (t2 : t)]
  | Map3 { t1; t2; t3; f = _ } -> [%message "map3" (t1 : t) (t2 : t) (t3 : t)]
  | Map4 { t1; t2; t3; t4; f = _ } ->
    [%message "map4" (t1 : t) (t2 : t) (t3 : t) (t4 : t)]
  | Map5 { t1; t2; t3; t4; t5; f = _ } ->
    [%message "map5" (t1 : t) (t2 : t) (t3 : t) (t4 : t) (t5 : t)]
  | Map6 { t1; t2; t3; t4; t5; t6; f = _ } ->
    [%message "map6" (t1 : t) (t2 : t) (t3 : t) (t4 : t) (t5 : t) (t6 : t)]
  | Map7 { t1; t2; t3; t4; t5; t6; t7; f = _ } ->
    [%message "map7" (t1 : t) (t2 : t) (t3 : t) (t4 : t) (t5 : t) (t6 : t) (t7 : t)]
;;

let map2 t1 t2 ~f = { value = Map2 { t1; t2; f }; here = None }
let map t ~f = { value = Map { t; f }; here = None }
let named n = { value = Named n; here = None }
let cutoff ~equal t = { value = Cutoff { t; equal }; here = None }

let rec eval : type a. Environment.t -> a t -> a Incr.t =
  fun env { value; _ } ->
  match value with
  | Incr x -> x
  | Cutoff { t; equal } ->
    let t = eval env t in
    Incremental.set_cutoff t (Incremental.Cutoff.of_equal equal);
    t
  | Constant (x, _id) -> Incr.return x
  | Named name ->
    (match Environment.find env name with
     | Some incremental -> incremental
     | None ->
       failwith
         "A Value.t was used outside of the scope that it was declared in! Make sure that \
          you aren't storing any Value.t inside a ref!")
  | Both (t1, t2) -> Incr.both (eval env t1) (eval env t2)
  | Map { t; f } -> Incr.map (eval env t) ~f
  | Map2 { t1; t2; f } -> Incr.map2 (eval env t1) (eval env t2) ~f
  | Map3 { t1; t2; t3; f } -> Incr.map3 (eval env t1) (eval env t2) (eval env t3) ~f
  | Map4 { t1; t2; t3; t4; f } ->
    Incr.map4 (eval env t1) (eval env t2) (eval env t3) (eval env t4) ~f
  | Map5 { t1; t2; t3; t4; t5; f } ->
    Incr.map5 (eval env t1) (eval env t2) (eval env t3) (eval env t4) (eval env t5) ~f
  | Map6 { t1; t2; t3; t4; t5; f; t6 } ->
    Incr.map6
      ~f
      (eval env t1)
      (eval env t2)
      (eval env t3)
      (eval env t4)
      (eval env t5)
      (eval env t6)
  | Map7 { t1; t2; t3; t4; t5; f; t6; t7 } ->
    Incr.map7
      ~f
      (eval env t1)
      (eval env t2)
      (eval env t3)
      (eval env t4)
      (eval env t5)
      (eval env t6)
      (eval env t7)
;;

let eval env t =
  let incr = eval env t in
  annotate Value incr;
  incr
;;

let return a = { value = Constant (a, Constant_id.create ()); here = None }

include Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return = return
    let map2 = map2
    let map = `Custom map
  end)

let both a b = { value = Both (a, b); here = None }
let map3 t1 t2 t3 ~f = { value = Map3 { t1; t2; t3; f }; here = None }
let map4 t1 t2 t3 t4 ~f = { value = Map4 { t1; t2; t3; t4; f }; here = None }
let map5 t1 t2 t3 t4 t5 ~f = { value = Map5 { t1; t2; t3; t4; t5; f }; here = None }

let map6 t1 t2 t3 t4 t5 t6 ~f =
  { value = Map6 { t1; t2; t3; t4; t5; t6; f }; here = None }
;;

let map7 t1 t2 t3 t4 t5 t6 t7 ~f =
  { value = Map7 { t1; t2; t3; t4; t5; t6; t7; f }; here = None }
;;

let rec all = function
  | [] -> return []
  | [ t1 ] -> map t1 ~f:(fun a1 -> [ a1 ])
  | [ t1; t2 ] -> map2 t1 t2 ~f:(fun a1 a2 -> [ a1; a2 ])
  | [ t1; t2; t3 ] -> map3 t1 t2 t3 ~f:(fun a1 a2 a3 -> [ a1; a2; a3 ])
  | [ t1; t2; t3; t4 ] -> map4 t1 t2 t3 t4 ~f:(fun a1 a2 a3 a4 -> [ a1; a2; a3; a4 ])
  | [ t1; t2; t3; t4; t5 ] ->
    map5 t1 t2 t3 t4 t5 ~f:(fun a1 a2 a3 a4 a5 -> [ a1; a2; a3; a4; a5 ])
  | [ t1; t2; t3; t4; t5; t6 ] ->
    map6 t1 t2 t3 t4 t5 t6 ~f:(fun a1 a2 a3 a4 a5 a6 -> [ a1; a2; a3; a4; a5; a6 ])
  | [ t1; t2; t3; t4; t5; t6; t7 ] ->
    map7 t1 t2 t3 t4 t5 t6 t7 ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
      [ a1; a2; a3; a4; a5; a6; a7 ])
  | t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: rest ->
    let left =
      map7 t1 t2 t3 t4 t5 t6 t7 ~f:(fun a1 a2 a3 a4 a5 a6 a7 ->
        [ a1; a2; a3; a4; a5; a6; a7 ])
    in
    let right = all rest in
    map2 left right ~f:(fun left right -> left @ right)
;;

let of_incr x = { value = Incr x; here = None }

module Open_on_rhs_intf = struct
  module type S = sig end
end

module Let_syntax = struct
  let return = return

  include Applicative_infix

  module Let_syntax = struct
    let return = return
    let map = map
    let both = both

    module Open_on_rhs = struct end
  end
end
