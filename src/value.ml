open! Core_kernel
open! Import
module Constant_id = Unique_id.Int ()

type _ t =
  | Constant : 'a * Constant_id.t -> 'a t
  | Incr : 'a Incr.t -> 'a t
  | Named : 'a Type_equal.Id.t -> 'a t
  | Both : 'a t * 'b t -> ('a * 'b) t
  | Cutoff :
      { t : 'a t
      ; equal : 'a -> 'a -> bool
      }
      -> 'a t
  | Map :
      { t : 'a t
      ; f : 'a -> 'b
      }
      -> 'b t
  | Map2 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; f : 't1 -> 't2 -> 'r
      }
      -> 'r t
  | Map3 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; f : 't1 -> 't2 -> 't3 -> 'r
      }
      -> 'r t
  | Map4 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 'r
      }
      -> 'r t
  | Map5 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; t5 : 't5 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 'r
      }
      -> 'r t
  | Map6 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; t3 : 't3 t
      ; t4 : 't4 t
      ; t5 : 't5 t
      ; t6 : 't6 t
      ; f : 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 'r
      }
      -> 'r t
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
      -> 'r t

let rec sexp_of_t : type a. a t -> Sexp.t = function
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

let map2 t1 t2 ~f = Map2 { t1; t2; f }
let map t ~f = Map { t; f }
let named n = Named n
let cutoff ~equal t = Cutoff { t; equal }

let rec eval : type a. Environment.t -> a t -> a Incr.t =
  fun env value ->
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
         "A Bonsai.Value.t was used outside of the scope that it was declared in!  Make \
          sure that you aren't storing any Value.t inside a ref!")
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
  (* Both collapsing *)
  | Both (t1, Both (t2, Both (t3, Both (t4, Both (t5, Both (t6, t7)))))) ->
    Incr.map7
      (eval env t1)
      (eval env t2)
      (eval env t3)
      (eval env t4)
      (eval env t5)
      (eval env t6)
      (eval env t7)
      ~f:(fun t1 t2 t3 t4 t5 t6 t7 -> t1, (t2, (t3, (t4, (t5, (t6, t7))))))
  | Both (t1, Both (t2, Both (t3, Both (t4, Both (t5, t6))))) ->
    Incr.map6
      (eval env t1)
      (eval env t2)
      (eval env t3)
      (eval env t4)
      (eval env t5)
      (eval env t6)
      ~f:(fun t1 t2 t3 t4 t5 t6 -> t1, (t2, (t3, (t4, (t5, t6)))))
  | Both (t1, Both (t2, Both (t3, Both (t4, t5)))) ->
    Incr.map5
      (eval env t1)
      (eval env t2)
      (eval env t3)
      (eval env t4)
      (eval env t5)
      ~f:(fun t1 t2 t3 t4 t5 -> t1, (t2, (t3, (t4, t5))))
  | Both (t1, Both (t2, Both (t3, t4))) ->
    Incr.map4
      (eval env t1)
      (eval env t2)
      (eval env t3)
      (eval env t4)
      ~f:(fun t1 t2 t3 t4 -> t1, (t2, (t3, t4)))
  | Both (t1, Both (t2, t3)) ->
    Incr.map3 (eval env t1) (eval env t2) (eval env t3) ~f:(fun t1 t2 t3 -> t1, (t2, t3))
  | Both (t1, t2) -> Incr.both (eval env t1) (eval env t2)
;;

let return a = Constant (a, Constant_id.create ())

include Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return = return
    let map2 = map2
    let map = `Custom map
  end)

let both a b = Both (a, b)
let map3 t1 t2 t3 ~f = Map3 { t1; t2; t3; f }
let map4 t1 t2 t3 t4 ~f = Map4 { t1; t2; t3; t4; f }
let map5 t1 t2 t3 t4 t5 ~f = Map5 { t1; t2; t3; t4; t5; f }
let map6 t1 t2 t3 t4 t5 t6 ~f = Map6 { t1; t2; t3; t4; t5; t6; f }
let map7 t1 t2 t3 t4 t5 t6 t7 ~f = Map7 { t1; t2; t3; t4; t5; t6; t7; f }
let of_incr x = Incr x

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

let%expect_test "tree bothing with let syntax" =
  let open Let_syntax in
  let x =
    let%map () = return ()
    and () = return ()
    and () = return ()
    and () = return ()
    and () = return ()
    and () = return ()
    and () = return () in
    ()
  in
  print_s (sexp_of_t x);
  [%expect
    {|
    (map
     (t
      (both (t1 constant)
       (t2
        (both (t1 constant)
         (t2
          (both (t1 constant)
           (t2
            (both (t1 constant)
             (t2 (both (t1 constant) (t2 (both (t1 constant) (t2 constant)))))))))))))) |}]
;;

let%expect_test "tree flattening with applicative API" =
  let open Let_syntax in
  let ( <| ) f a = apply f (return a) in
  let x = return (fun () () () () () () -> ()) <| () <| () <| () <| () <| () <| () in
  print_s (sexp_of_t x);
  [%expect
    {|
    (map2
     (t1
      (map2
       (t1
        (map2
         (t1
          (map2 (t1 (map2 (t1 (map2 (t1 constant) (t2 constant))) (t2 constant)))
           (t2 constant)))
         (t2 constant)))
       (t2 constant)))
     (t2 constant)) |}]
;;
