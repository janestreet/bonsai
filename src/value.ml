open! Core
open! Import

type _ without_position =
  | Constant : 'a -> 'a without_position
  | Incr : 'a Incr.t -> 'a without_position
  | Named : 'a without_position
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
  | Lazy : 'a Lazy.t -> 'a without_position

and 'a t =
  { value : 'a without_position
  ; here : Source_code_position.t option
  ; id : 'a Type_equal.Id.t
  }

let value_id name = Type_equal.Id.create ~name sexp_of_opaque

let contents_if_value_is_constant : type a. a t -> a Lazy.t option =
  fun { value; here = _; id = _ } ->
  match value with
  | Constant x -> Some (Lazy.return x)
  | Incr _ -> None
  | Named -> None
  | Both (_, _) -> None
  | Cutoff _ -> None
  | Map _ -> None
  | Map2 _ -> None
  | Map3 _ -> None
  | Map4 _ -> None
  | Map5 _ -> None
  | Map6 _ -> None
  | Map7 _ -> None
  | Lazy x -> Some x
;;

let lazy_ name l = { value = Lazy l; here = None; id = value_id name }

let map2 t1 t2 ~f =
  let open Option.Let_syntax in
  let constant_contents =
    let%bind t1 = contents_if_value_is_constant t1 in
    let%map t2 = contents_if_value_is_constant t2 in
    lazy (f (force t1) (force t2))
  in
  match constant_contents with
  | Some l -> lazy_ "map2" l
  | _ -> { value = Map2 { t1; t2; f }; here = None; id = value_id "map2" }
;;

let fold_constant ~name t ~f =
  match contents_if_value_is_constant t with
  | Some t -> Some (lazy_ name (lazy (f (force t))))
  | _ -> None
;;

(* [map] and [fast_map] only reduce when given linear chains of one another
   e.g. [map -> fast_map -> map -> map -> fast_map], and they fully reduce when
   this function is called, so there will never be a scenario where we need to
   dig deeper than one level to find a new optimization. *)
let map t ~f =
  match fold_constant ~name:"map" t ~f with
  | Some l -> l
  | None -> { value = Map { t; f }; here = None; id = value_id "map" }
;;

let named id = { value = Named; here = None; id }

let cutoff ~equal t =
  match contents_if_value_is_constant t with
  | Some l -> lazy_ "cutoff" l
  | _ ->
    let value =
      match t.value with
      | Cutoff { t; equal = inner_equal } ->
        Cutoff { t; equal = (fun a b -> inner_equal a b || equal a b) }
      | _ -> Cutoff { t; equal }
    in
    { value; here = None; id = value_id "cutoff" }
;;

let rec eval : type a. Environment.t -> a t -> a Incr.t =
  fun env { value; id; here = _ } ->
  match value with
  | Incr x -> x
  | Cutoff { t; equal } ->
    let incremental_node =
      let incremental_node = eval env t in
      (* The result of evaling a [Named] value gets stored in a map, so calling
         [set_cutoff] directly mutates/affects all usages of it. Doing a no-op map
         prevents this. *)
      match t.value with
      | Named -> Ui_incr.map ~f:Fn.id incremental_node
      | _ -> incremental_node
    in
    Incremental.set_cutoff incremental_node (Incremental.Cutoff.of_equal equal);
    incremental_node
  | Constant x -> Incr.return x
  | Lazy x -> Incr.return (force x)
  | Named ->
    (match Environment.find env id with
     | Some incremental -> incremental
     | None ->
       let uid = Type_equal.Id.uid id in
       raise_s
         [%message
           "A Value.t was used outside of the scope that it was declared in! Make sure \
            that you aren't storing any Value.t inside a ref!"
             (uid : Type_equal.Id.Uid.t)])
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

let return a = { value = Constant a; here = None; id = value_id "return" }

include Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return = return
    let map2 = map2
    let map = `Custom map
  end)

let both a b =
  match contents_if_value_is_constant a, contents_if_value_is_constant b with
  | Some l, Some r -> lazy_ "both" (lazy (force l, force r))
  | Some l, None -> map b ~f:(fun b -> force l, b)
  | None, Some r -> map a ~f:(fun a -> a, force r)
  | None, None -> { value = Both (a, b); here = None; id = value_id "both" }
;;

let map3 t1 t2 t3 ~f =
  let open Option.Let_syntax in
  let constant_contents =
    let%bind t1 = contents_if_value_is_constant t1 in
    let%bind t2 = contents_if_value_is_constant t2 in
    let%map t3 = contents_if_value_is_constant t3 in
    lazy (f (force t1) (force t2) (force t3))
  in
  match constant_contents with
  | Some l -> lazy_ "map2" l
  | _ -> { value = Map3 { t1; t2; t3; f }; here = None; id = value_id "map3" }
;;

let map4 t1 t2 t3 t4 ~f =
  let open Option.Let_syntax in
  let constant_contents =
    let%bind t1 = contents_if_value_is_constant t1 in
    let%bind t2 = contents_if_value_is_constant t2 in
    let%bind t3 = contents_if_value_is_constant t3 in
    let%map t4 = contents_if_value_is_constant t4 in
    lazy (f (force t1) (force t2) (force t3) (force t4))
  in
  match constant_contents with
  | Some l -> lazy_ "map4" l
  | _ -> { value = Map4 { t1; t2; t3; t4; f }; here = None; id = value_id "map4" }
;;

let map5 t1 t2 t3 t4 t5 ~f =
  let open Option.Let_syntax in
  let constant_contents =
    let%bind t1 = contents_if_value_is_constant t1 in
    let%bind t2 = contents_if_value_is_constant t2 in
    let%bind t3 = contents_if_value_is_constant t3 in
    let%bind t4 = contents_if_value_is_constant t4 in
    let%map t5 = contents_if_value_is_constant t5 in
    lazy (f (force t1) (force t2) (force t3) (force t4) (force t5))
  in
  match constant_contents with
  | Some l -> lazy_ "map5" l
  | _ -> { value = Map5 { t1; t2; t3; t4; t5; f }; here = None; id = value_id "map5" }
;;

let map6 t1 t2 t3 t4 t5 t6 ~f =
  let open Option.Let_syntax in
  let constant_contents =
    let%bind t1 = contents_if_value_is_constant t1 in
    let%bind t2 = contents_if_value_is_constant t2 in
    let%bind t3 = contents_if_value_is_constant t3 in
    let%bind t4 = contents_if_value_is_constant t4 in
    let%bind t5 = contents_if_value_is_constant t5 in
    let%map t6 = contents_if_value_is_constant t6 in
    lazy (f (force t1) (force t2) (force t3) (force t4) (force t5) (force t6))
  in
  match constant_contents with
  | Some l -> lazy_ "map6" l
  | _ -> { value = Map6 { t1; t2; t3; t4; t5; t6; f }; here = None; id = value_id "map6" }
;;

let map7 t1 t2 t3 t4 t5 t6 t7 ~f =
  let open Option.Let_syntax in
  let constant_contents =
    let%bind t1 = contents_if_value_is_constant t1 in
    let%bind t2 = contents_if_value_is_constant t2 in
    let%bind t3 = contents_if_value_is_constant t3 in
    let%bind t4 = contents_if_value_is_constant t4 in
    let%bind t5 = contents_if_value_is_constant t5 in
    let%bind t6 = contents_if_value_is_constant t6 in
    let%map t7 = contents_if_value_is_constant t7 in
    lazy (f (force t1) (force t2) (force t3) (force t4) (force t5) (force t6) (force t7))
  in
  match constant_contents with
  | Some l -> lazy_ "map7" l
  | _ ->
    { value = Map7 { t1; t2; t3; t4; t5; t6; t7; f }; here = None; id = value_id "map7" }
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

let of_incr x = { value = Incr x; here = None; id = value_id "incr" }

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
