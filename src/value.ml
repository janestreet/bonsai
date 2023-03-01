open! Core
open! Import

module Name_source = struct
  type t =
    | Sub of Source_code_position.t option
    | Assoc_like_key
    | Assoc_like_data
    | Wrap_model
    | Wrap_inject
    | App_input
    | Model_resetter

  let to_string = function
    | Sub here ->
      (match here with
       | Some here ->
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
           "A Value.t introduced by the [let%sub] expression at \
            %{here#Source_code_position}"]
       | None -> "A Value.t introduced by some [let%sub] expression")
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
  ;;
end

type _ without_position =
  | Constant : 'a -> 'a without_position
  | Incr : 'a Incr.t -> 'a without_position
  | Named : Name_source.t -> 'a without_position
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
  ; here : Source_code_position.t option
  ; id : 'a Type_equal.Id.t
  }

let value_id name = Type_equal.Id.create ~name sexp_of_opaque
let map2 t1 t2 ~f = { value = Map2 { t1; t2; f }; here = None; id = value_id "map2" }
let map t ~f = { value = Map { t; f }; here = None; id = value_id "map" }
let named name_source id = { value = Named name_source; here = None; id }

let cutoff ~added_by_let_syntax t ~equal =
  let value = Cutoff { t; equal; added_by_let_syntax } in
  { value; here = None; id = value_id "cutoff" }
;;

let rec eval : type a. Environment.t -> a t -> a Incr.t =
  fun env { value; id; here = _ } ->
  match value with
  | Incr x -> x
  | Cutoff { t; equal; added_by_let_syntax = _ } ->
    let incremental_node =
      let incremental_node = eval env t in
      (* In general, we have to create a fresh incremental node here (e.g. by using
         [Ui_incr.map ~f:Fn.id] and set cutoff on the new node. Otherwise it is possible
         that we will set a cutoff on an incremental node which is used in more than
         one place and the cutoff may not be correct then.

         E.g. the result of evaling a [Named] value gets stored in a map, so calling
         [set_cutoff] directly mutates/affects all usages of it. Similarly for [Incr].

         Nodes created by evaluating e.g. [Map] were just created by the [eval] function
         call in which case we can set the cutoff directly on them, saving some memory
         and performance by not creating an extra node. *)
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
      | Map7 _ ->
        incremental_node
    in
    Incremental.set_cutoff incremental_node (Incremental.Cutoff.of_equal equal);
    incremental_node
  | Constant x -> Incr.return x
  | Exception ex -> Incr.map (Incr.return ()) ~f:(fun () -> raise ex)
  | Named name_source ->
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
  annotate Value incr;
  incr
;;

let return a = { value = Constant a; here = None; id = value_id "return" }
let return_exn exn = { value = Exception exn; here = None; id = value_id "return exn" }

include Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return = return
    let map2 = map2
    let map = `Custom map
  end)

let both a b = { value = Both (a, b); here = None; id = value_id "both" }

let map3 t1 t2 t3 ~f =
  { value = Map3 { t1; t2; t3; f }; here = None; id = value_id "map3" }
;;

let map4 t1 t2 t3 t4 ~f =
  { value = Map4 { t1; t2; t3; t4; f }; here = None; id = value_id "map4" }
;;

let map5 t1 t2 t3 t4 t5 ~f =
  { value = Map5 { t1; t2; t3; t4; t5; f }; here = None; id = value_id "map5" }
;;

let map6 t1 t2 t3 t4 t5 t6 ~f =
  { value = Map6 { t1; t2; t3; t4; t5; t6; f }; here = None; id = value_id "map6" }
;;

let map7 t1 t2 t3 t4 t5 t6 t7 ~f =
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
