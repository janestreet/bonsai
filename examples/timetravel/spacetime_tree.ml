open! Core

type 'a t =
  { value : 'a
  ; children : 'a t Int.Map.t
  }
[@@deriving equal, sexp]

module Cursor = struct
  type t = int list [@@deriving equal, sexp]
end

let create init = { value = init; children = Int.Map.empty }, []

let rec append tree cursor to_insert =
  match cursor with
  | [] ->
    let new_leaf = { value = to_insert; children = Int.Map.empty } in
    let new_cursor_pos = Map.length tree.children in
    let children = Map.add_exn tree.children ~key:new_cursor_pos ~data:new_leaf in
    { tree with children }, [ new_cursor_pos ]
  | x :: rest ->
    let child = Map.find_exn tree.children x in
    let new_child, new_cursor = append child rest to_insert in
    let children = Map.set tree.children ~key:x ~data:new_child in
    { tree with children }, x :: new_cursor
;;

let find tree cursor =
  let go tree x = Map.find_exn tree.children x in
  (List.fold cursor ~init:tree ~f:go).value
;;

let rec width tree =
  1 + (tree.children |> Map.data |> List.map ~f:width |> List.fold ~init:0 ~f:Int.max)
;;

let rec height tree =
  if Map.is_empty tree.children
  then 1
  else tree.children |> Map.data |> List.sum (module Int) ~f:height
;;

let traverse tree ~f =
  let rec traverse_impl { value; children } ~cursor ~f =
    if Map.is_empty children
    then f ~data:value ~cursor ~children:[]
    else (
      let ret =
        Map.fold children ~init:[] ~f:(fun ~key:i ~data:child res ->
          let cursor = List.append cursor [ i ] in
          let ret = traverse_impl child ~cursor ~f in
          ret :: res)
        |> List.rev
      in
      f ~data:value ~cursor ~children:ret)
  in
  traverse_impl tree ~cursor:[] ~f
;;
