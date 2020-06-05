open! Core_kernel
open! Import

type _ t =
  | Constant : 'a -> 'a t
  | Incr : 'a Incr.t -> 'a t
  | Named : 'a Type_equal.Id.t -> 'a t
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
  | Incr _ -> [%sexp "incr"]
  | Named id -> [%sexp "named", (Type_equal.Id.name id : string)]
  | Map { t; f = _ } -> [%message "map" (t : t)]
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

let map2 t1 t2 ~f:f1 =
  match t1, t2 with
  (* Applicative-apply builds a left-leaning list of map2s *)
  | Map2 { t1; t2; f = f2 }, x ->
    Map3 { t1; t2; t3 = x; f = (fun t1 t2 x -> f1 (f2 t1 t2) x) }
  | Map3 { t1; t2; t3; f = f2 }, x ->
    Map4 { t1; t2; t3; t4 = x; f = (fun t1 t2 t3 x -> f1 (f2 t1 t2 t3) x) }
  | Map4 { t1; t2; t3; t4; f = f2 }, x ->
    Map5 { t1; t2; t3; t4; t5 = x; f = (fun t1 t2 t3 t4 x -> f1 (f2 t1 t2 t3 t4) x) }
  | Map5 { t1; t2; t3; t4; t5; f = f2 }, x ->
    Map6
      { t1
      ; t2
      ; t3
      ; t4
      ; t5
      ; t6 = x
      ; f = (fun t1 t2 t3 t4 t5 x -> f1 (f2 t1 t2 t3 t4 t5) x)
      }
  | Map6 { t1; t2; t3; t4; t5; f = f2; t6 }, x ->
    Map7
      { t1
      ; t2
      ; t3
      ; t4
      ; t5
      ; t6
      ; t7 = x
      ; f = (fun t1 t2 t3 t4 t5 t6 x -> f1 (f2 t1 t2 t3 t4 t5 t6) x)
      }
  (* Let-syntax builds a right-leaning list of map2s *)
  | x, Map2 { t1; t2; f = f2 } ->
    Map3 { t1 = x; t2 = t1; t3 = t2; f = (fun x t2 t3 -> f1 x (f2 t2 t3)) }
  | x, Map3 { t1; t2; t3; f = f2 } ->
    Map4
      { t1 = x; t2 = t1; t3 = t2; t4 = t3; f = (fun x t2 t3 t4 -> f1 x (f2 t2 t3 t4)) }
  | x, Map4 { t1; t2; t3; t4; f = f2 } ->
    Map5
      { t1 = x
      ; t2 = t1
      ; t3 = t2
      ; t4 = t3
      ; t5 = t4
      ; f = (fun x t2 t3 t4 t5 -> f1 x (f2 t2 t3 t4 t5))
      }
  | x, Map5 { t1; t2; t3; t4; t5; f = f2 } ->
    Map6
      { t1 = x
      ; t2 = t1
      ; t3 = t2
      ; t4 = t3
      ; t5 = t4
      ; t6 = t5
      ; f = (fun x t2 t3 t4 t5 t6 -> f1 x (f2 t2 t3 t4 t5 t6))
      }
  | x, Map6 { t1; t2; t3; t4; t5; f = f2; t6 } ->
    Map7
      { t1 = x
      ; t2 = t1
      ; t3 = t2
      ; t4 = t3
      ; t5 = t4
      ; f = (fun x t2 t3 t4 t5 t6 t7 -> f1 x (f2 t2 t3 t4 t5 t6 t7))
      ; t6 = t5
      ; t7 = t6
      }
  | t1, t2 -> Map2 { t1; t2; f = f1 }
;;

let map t ~f:f' =
  match t with
  | Map { t; f } ->
    let f t1 = f' (f t1) in
    Map { t; f }
  | Map2 { t1; t2; f } ->
    let f t1 t2 = f' (f t1 t2) in
    Map2 { t1; t2; f }
  | Map3 { t1; t2; t3; f } ->
    let f t1 t2 t3 = f' (f t1 t2 t3) in
    Map3 { t1; t2; t3; f }
  | Map4 { t1; t2; t3; t4; f } ->
    let f t1 t2 t3 t4 = f' (f t1 t2 t3 t4) in
    Map4 { t1; t2; t3; t4; f }
  | Map5 { t1; t2; t3; t4; t5; f } ->
    let f t1 t2 t3 t4 t5 = f' (f t1 t2 t3 t4 t5) in
    Map5 { t1; t2; t3; t4; t5; f }
  | Map6 { t1; t2; t3; t4; t5; f; t6 } ->
    let f t1 t2 t3 t4 t5 t6 = f' (f t1 t2 t3 t4 t5 t6) in
    Map6 { t1; t2; t3; t4; t5; f; t6 }
  | Map7 { t1; t2; t3; t4; t5; f; t6; t7 } ->
    let f t1 t2 t3 t4 t5 t6 t7 = f' (f t1 t2 t3 t4 t5 t6 t7) in
    Map7 { t1; t2; t3; t4; t5; f; t6; t7 }
  | Constant x -> Constant (f' x)
  | (Named _ | Incr _) as other -> Map { t = other; f = f' }
;;

let named n = Named n

let rec eval : type a. Environment.t -> a t -> a Incr.t =
  fun env value ->
  match value with
  | Incr x -> x
  | Constant x -> Incr.return x
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
;;

module T = Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let map = `Custom map
    let return a = Constant a
    let map2 = map2
  end)

include T

let of_incr x = Incr x

module Open_on_rhs_intf = struct
  module type S = sig end
end

module Let_syntax = struct
  include T

  module Let_syntax = struct
    include T

    let both t1 t2 = map2 t1 t2 ~f:Tuple2.create

    module Open_on_rhs = struct end
  end
end

let%expect_test "tree flattening with let syntax" =
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
    (map7 (t1 constant) (t2 constant) (t3 constant) (t4 constant) (t5 constant)
     (t6 constant) (t7 constant)) |}]
;;

let%expect_test "tree flattening with applicative API" =
  let open Let_syntax in
  let ( <| ) f a = apply f (return a) in
  let x = return (fun () () () () () () -> ()) <| () <| () <| () <| () <| () <| () in
  print_s (sexp_of_t x);
  [%expect
    {|
    (map7 (t1 constant) (t2 constant) (t3 constant) (t4 constant) (t5 constant)
     (t6 constant) (t7 constant)) |}]
;;
