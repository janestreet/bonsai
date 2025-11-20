open! Core
open! Import

module Name_source = struct
  type t =
    | Sub of Source_code_position.t
    | Assoc_like_key
    | Assoc_like_data
    | Wrap_model
    | Wrap_inject
    | App_input
    | Model_resetter
    | Inverted_lifecycles_dependency
    | Fix_recurse

  let to_string = function
    | Sub here ->
      let here =
        if Core.am_running_test
        then
          { Source_code_position.pos_fname = "TEST_FILENAME"
          ; pos_lnum = 0
          ; pos_bol = 0
          ; pos_cnum = 0
          }
        else here
      in
      [%string
        "A Value.t introduced by the [let%sub] expression at %{here#Source_code_position}"]
    | Assoc_like_key ->
      "The Value.t for the key introduced within a [Bonsai.assoc] or [Bonsai.assoc_on] \
       computation"
    | Assoc_like_data ->
      "The Value.t for the data introduced within a [Bonsai.assoc] or [Bonsai.assoc_on] \
       computation"
    | Wrap_model -> "The name for the model introduced within a [Bonsai.wrap] computation"
    | Wrap_inject ->
      "The Value.t for the injection function introduced within a [Bonsai.wrap] \
       computation"
    | App_input -> "The app input Value.t"
    | Model_resetter -> "A model resetter"
    | Inverted_lifecycles_dependency ->
      "The value passed from [compute_dep] into [f] in \
       [Bonsai.with_inverted_lifecycle_ordering]"
    | Fix_recurse ->
      "The value parameter passed to the recursive call within [Bonsai.fix]"
  ;;
end

type _ without_position =
  | Constant : 'a Lazy.t -> 'a without_position
  | Incr : 'a Incr.t -> 'a without_position
  | Named : Name_source.t * 'a Type_equal.Id.t -> 'a without_position
  | Both : 'a t * 'b t -> ('a * 'b) without_position
  | Cutoff :
      { t : 'a t
      ; equal : 'a -> 'a -> bool
      ; added_by_let_syntax : bool
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
  | Exception : exn -> 'r without_position

and 'a t =
  { value : 'a without_position
  ; here : Source_code_position.t
  }

let map2 ~(here : [%call_pos]) t1 t2 ~f = { value = Map2 { t1; t2; f }; here }
let map ~(here : [%call_pos]) t ~f = { value = Map { t; f }; here }
let named ~(here : [%call_pos]) name_source id = { value = Named (name_source, id); here }

let cutoff ~(here : [%call_pos]) ~added_by_let_syntax t ~equal =
  let value = Cutoff { t; equal; added_by_let_syntax } in
  { value; here }
;;

let rec eval : type a. Environment.t -> a t -> a Incr.t =
  fun env { value; here = _ } ->
  match value with
  | Incr x -> x
  | Cutoff { t; equal; added_by_let_syntax = _ } ->
    let incremental_node =
      let incremental_node = eval env t in
      (* In general, we have to create a fresh incremental node here (e.g. by using
         [Ui_incr.map ~f:Fn.id] and set cutoff on the new node. Otherwise it is possible
         that we will set a cutoff on an incremental node which is used in more than one
         place and the cutoff may not be correct then.

         E.g. the result of evaling a [Named] value gets stored in a map, so calling
         [set_cutoff] directly mutates/affects all usages of it. Similarly for [Incr].

         Nodes created by evaluating e.g. [Map] were just created by the [eval] function
         call in which case we can set the cutoff directly on them, saving some memory and
         performance by not creating an extra node. *)
      match t.value with
      | Named _ | Incr _ | Cutoff _ -> Ui_incr.map ~f:Fn.id incremental_node
      | Constant _
      | Exception _
      | Both _
      | Map _
      | Map2 _
      | Map3 _
      | Map4 _
      | Map5 _
      | Map6 _
      | Map7 _ -> incremental_node
    in
    Incremental.set_cutoff incremental_node (Incremental.Cutoff.of_equal equal);
    incremental_node
  | Constant x -> Incr.Expert.Node.(watch (create (fun () -> Lazy.force x)))
  | Exception ex -> Incr.map (Incr.return ()) ~f:(fun () -> raise ex)
  | Named (name_source, id) ->
    (match Environment.find env id with
     | Some incremental -> incremental
     | None ->
       raise_s
         [%message
           [%string
             "%{name_source#Name_source} was used outside of the scope that it was \
              declared in. Make sure that you aren't storing it inside a ref."]])
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
  annotate ~here:t.here Value incr;
  incr
;;

let return ~(here : [%call_pos]) a = { value = Constant (Lazy.from_val a); here }
let return_lazy ~(here : [%call_pos]) a = { value = Constant a; here }
let return_exn ~(here : [%call_pos]) exn = { value = Exception exn; here }

let transpose_opt opt =
  Option.value_map opt ~default:(return None) ~f:(map ~f:Option.some)
;;

include Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return = return
    let map2 = map2
    let map = `Custom map
  end)

let both ~(here : [%call_pos]) a b = { value = Both (a, b); here }
let map3 ~(here : [%call_pos]) t1 t2 t3 ~f = { value = Map3 { t1; t2; t3; f }; here }

let map4 ~(here : [%call_pos]) t1 t2 t3 t4 ~f =
  { value = Map4 { t1; t2; t3; t4; f }; here }
;;

let map5 ~(here : [%call_pos]) t1 t2 t3 t4 t5 ~f =
  { value = Map5 { t1; t2; t3; t4; t5; f }; here }
;;

let map6 ~(here : [%call_pos]) t1 t2 t3 t4 t5 t6 ~f =
  { value = Map6 { t1; t2; t3; t4; t5; t6; f }; here }
;;

let map7 ~(here : [%call_pos]) t1 t2 t3 t4 t5 t6 t7 ~f =
  { value = Map7 { t1; t2; t3; t4; t5; t6; t7; f }; here }
;;

let all ~(here : [%call_pos]) = function
  | [] -> return []
  | [ x ] -> map x ~f:(fun x -> [ x ])
  | xs ->
    (* [Balance_list_tree] guarantees that if there are any [Node]s, they will all be at
       the start of the list. This means we don't need to match on all possible
       permutations of leaves and nodes. *)
    let tree = Balance_list_tree.balance ~n:7 xs |> ok_exn in
    let rec flatten (node : 'a t Balance_list_tree.t) =
      match node with
      | Leaf x -> map ~here x ~f:(fun x -> [ x ])
      | Node [ x1 ] -> flatten x1
      | Node [ Leaf x1; Leaf x2 ] -> map2 ~here x1 x2 ~f:(fun x1 x2 -> [ x1; x2 ])
      | Node [ x1; Leaf x2 ] -> map2 ~here (flatten x1) x2 ~f:(fun x1 x2 -> x1 @ [ x2 ])
      | Node [ x1; x2 ] -> map2 ~here (flatten x1) (flatten x2) ~f:(fun x1 x2 -> x1 @ x2)
      | Node [ Leaf x1; Leaf x2; Leaf x3 ] ->
        map3 ~here x1 x2 x3 ~f:(fun x1 x2 x3 -> [ x1; x2; x3 ])
      | Node [ x1; Leaf x2; Leaf x3 ] ->
        map3 ~here (flatten x1) x2 x3 ~f:(fun x1 x2 x3 -> x1 @ [ x2; x3 ])
      | Node [ x1; x2; Leaf x3 ] ->
        map3 ~here (flatten x1) (flatten x2) x3 ~f:(fun x1 x2 x3 -> x1 @ x2 @ [ x3 ])
      | Node [ x1; x2; x3 ] ->
        map3 ~here (flatten x1) (flatten x2) (flatten x3) ~f:(fun x1 x2 x3 ->
          x1 @ x2 @ x3)
      | Node [ Leaf x1; Leaf x2; Leaf x3; Leaf x4 ] ->
        map4 ~here x1 x2 x3 x4 ~f:(fun x1 x2 x3 x4 -> [ x1; x2; x3; x4 ])
      | Node [ x1; Leaf x2; Leaf x3; Leaf x4 ] ->
        map4 ~here (flatten x1) x2 x3 x4 ~f:(fun x1 x2 x3 x4 -> x1 @ [ x2; x3; x4 ])
      | Node [ x1; x2; Leaf x3; Leaf x4 ] ->
        map4 ~here (flatten x1) (flatten x2) x3 x4 ~f:(fun x1 x2 x3 x4 ->
          x1 @ x2 @ [ x3; x4 ])
      | Node [ x1; x2; x3; Leaf x4 ] ->
        map4 ~here (flatten x1) (flatten x2) (flatten x3) x4 ~f:(fun x1 x2 x3 x4 ->
          x1 @ x2 @ x3 @ [ x4 ])
      | Node [ x1; x2; x3; x4 ] ->
        map4
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          ~f:(fun x1 x2 x3 x4 -> x1 @ x2 @ x3 @ x4)
      | Node [ Leaf x1; Leaf x2; Leaf x3; Leaf x4; Leaf x5 ] ->
        map5 ~here x1 x2 x3 x4 x5 ~f:(fun x1 x2 x3 x4 x5 -> [ x1; x2; x3; x4; x5 ])
      | Node [ x1; Leaf x2; Leaf x3; Leaf x4; Leaf x5 ] ->
        map5 ~here (flatten x1) x2 x3 x4 x5 ~f:(fun x1 x2 x3 x4 x5 ->
          x1 @ [ x2; x3; x4; x5 ])
      | Node [ x1; x2; Leaf x3; Leaf x4; Leaf x5 ] ->
        map5 ~here (flatten x1) (flatten x2) x3 x4 x5 ~f:(fun x1 x2 x3 x4 x5 ->
          x1 @ x2 @ [ x3; x4; x5 ])
      | Node [ x1; x2; x3; Leaf x4; Leaf x5 ] ->
        map5 ~here (flatten x1) (flatten x2) (flatten x3) x4 x5 ~f:(fun x1 x2 x3 x4 x5 ->
          x1 @ x2 @ x3 @ [ x4; x5 ])
      | Node [ x1; x2; x3; x4; Leaf x5 ] ->
        map5
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          x5
          ~f:(fun x1 x2 x3 x4 x5 -> x1 @ x2 @ x3 @ x4 @ [ x5 ])
      | Node [ x1; x2; x3; x4; x5 ] ->
        map5
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          (flatten x5)
          ~f:(fun x1 x2 x3 x4 x5 -> x1 @ x2 @ x3 @ x4 @ x5)
      | Node [ Leaf x1; Leaf x2; Leaf x3; Leaf x4; Leaf x5; Leaf x6 ] ->
        map6 ~here x1 x2 x3 x4 x5 x6 ~f:(fun x1 x2 x3 x4 x5 x6 ->
          [ x1; x2; x3; x4; x5; x6 ])
      | Node [ x1; Leaf x2; Leaf x3; Leaf x4; Leaf x5; Leaf x6 ] ->
        map6 ~here (flatten x1) x2 x3 x4 x5 x6 ~f:(fun x1 x2 x3 x4 x5 x6 ->
          x1 @ [ x2; x3; x4; x5; x6 ])
      | Node [ x1; x2; Leaf x3; Leaf x4; Leaf x5; Leaf x6 ] ->
        map6 ~here (flatten x1) (flatten x2) x3 x4 x5 x6 ~f:(fun x1 x2 x3 x4 x5 x6 ->
          x1 @ x2 @ [ x3; x4; x5; x6 ])
      | Node [ x1; x2; x3; Leaf x4; Leaf x5; Leaf x6 ] ->
        map6
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          x4
          x5
          x6
          ~f:(fun x1 x2 x3 x4 x5 x6 -> x1 @ x2 @ x3 @ [ x4; x5; x6 ])
      | Node [ x1; x2; x3; x4; Leaf x5; Leaf x6 ] ->
        map6
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          x5
          x6
          ~f:(fun x1 x2 x3 x4 x5 x6 -> x1 @ x2 @ x3 @ x4 @ [ x5; x6 ])
      | Node [ x1; x2; x3; x4; x5; Leaf x6 ] ->
        map6
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          (flatten x5)
          x6
          ~f:(fun x1 x2 x3 x4 x5 x6 -> x1 @ x2 @ x3 @ x4 @ x5 @ [ x6 ])
      | Node [ x1; x2; x3; x4; x5; x6 ] ->
        map6
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          (flatten x5)
          (flatten x6)
          ~f:(fun x1 x2 x3 x4 x5 x6 -> x1 @ x2 @ x3 @ x4 @ x5 @ x6)
      | Node [ Leaf x1; Leaf x2; Leaf x3; Leaf x4; Leaf x5; Leaf x6; Leaf x7 ] ->
        map7 ~here x1 x2 x3 x4 x5 x6 x7 ~f:(fun x1 x2 x3 x4 x5 x6 x7 ->
          [ x1; x2; x3; x4; x5; x6; x7 ])
      | Node [ x1; Leaf x2; Leaf x3; Leaf x4; Leaf x5; Leaf x6; Leaf x7 ] ->
        map7 ~here (flatten x1) x2 x3 x4 x5 x6 x7 ~f:(fun x1 x2 x3 x4 x5 x6 x7 ->
          x1 @ [ x2; x3; x4; x5; x6; x7 ])
      | Node [ x1; x2; Leaf x3; Leaf x4; Leaf x5; Leaf x6; Leaf x7 ] ->
        map7
          ~here
          (flatten x1)
          (flatten x2)
          x3
          x4
          x5
          x6
          x7
          ~f:(fun x1 x2 x3 x4 x5 x6 x7 -> x1 @ x2 @ [ x3; x4; x5; x6; x7 ])
      | Node [ x1; x2; x3; Leaf x4; Leaf x5; Leaf x6; Leaf x7 ] ->
        map7
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          x4
          x5
          x6
          x7
          ~f:(fun x1 x2 x3 x4 x5 x6 x7 -> x1 @ x2 @ x3 @ [ x4; x5; x6; x7 ])
      | Node [ x1; x2; x3; x4; Leaf x5; Leaf x6; Leaf x7 ] ->
        map7
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          x5
          x6
          x7
          ~f:(fun x1 x2 x3 x4 x5 x6 x7 -> x1 @ x2 @ x3 @ x4 @ [ x5; x6; x7 ])
      | Node [ x1; x2; x3; x4; x5; Leaf x6; Leaf x7 ] ->
        map7
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          (flatten x5)
          x6
          x7
          ~f:(fun x1 x2 x3 x4 x5 x6 x7 -> x1 @ x2 @ x3 @ x4 @ x5 @ [ x6; x7 ])
      | Node [ x1; x2; x3; x4; x5; x6; Leaf x7 ] ->
        map7
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          (flatten x5)
          (flatten x6)
          x7
          ~f:(fun x1 x2 x3 x4 x5 x6 x7 -> x1 @ x2 @ x3 @ x4 @ x5 @ x6 @ [ x7 ])
      | Node [ x1; x2; x3; x4; x5; x6; x7 ] ->
        map7
          ~here
          (flatten x1)
          (flatten x2)
          (flatten x3)
          (flatten x4)
          (flatten x5)
          (flatten x6)
          (flatten x7)
          ~f:(fun x1 x2 x3 x4 x5 x6 x7 -> x1 @ x2 @ x3 @ x4 @ x5 @ x6 @ x7)
      | Node xs ->
        (* This shouldn't happen, because the balancer guaruntees that each node has at
           most 7 children. But exceptions at runtime are scary, so let's be safe. *)
        Nonempty_list.fold_right xs ~init:(return ~here []) ~f:(fun x acc ->
          map2 ~here (flatten x) acc ~f:(fun x acc -> x @ acc))
    in
    flatten tree
;;

let of_incr ~(here : [%call_pos]) x = { value = Incr x; here }

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
