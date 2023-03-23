open! Core
open Js_of_ocaml
open Jsoo_weak_collections

let obj : unit Js.js_array Js.t = new%js Js.array_empty

let%expect_test "weak-map operations" =
  let map = Weak_map.create () in
  assert (Option.is_none (Weak_map.get map obj));
  Weak_map.set map obj 5;
  (match Weak_map.get map obj with
   | Some 5 -> ()
   | _ -> assert false);
  Weak_map.delete map obj;
  assert (Option.is_none (Weak_map.get map obj))
;;

let%expect_test "weak-set operations" =
  let set = Weak_set.create () in
  assert (not (Weak_set.has set obj));
  Weak_set.add set obj;
  assert (Weak_set.has set obj);
  Weak_set.delete set obj;
  assert (not (Weak_set.has set obj))
;;
