open Core

type 'a t =
  | Leaf of 'a
  | Node of 'a t Nonempty_list.t
[@@deriving sexp_of]

let balance ~n nodes =
  let rec loop ls =
    let chunks =
      Nonempty_list.to_list ls
      |> List.chunks_of ~length:n
      |> List.filter_map ~f:Nonempty_list.of_list
      |> Nonempty_list.of_list_exn
      (* [List.chunks_of] on a non_empty list is guaranteed to return at least one element
         list so this [of_list_exn] is safe. We have some quicktest test asserting this
         property should this ever change. *)
    in
    match chunks with
    | [ [ single ] ] -> single
    | [ single_chunk ] -> Node single_chunk
    | _ when Nonempty_list.length chunks > n -> Nonempty_list.map chunks ~f:loop |> loop
    | _ -> Node (Nonempty_list.map chunks ~f:loop)
  in
  loop nodes
;;

let balance ~n list =
  match Nonempty_list.of_list list with
  | None -> Or_error.error_string "expand_letn: list of bindings must be non-empty"
  | Some _ when n <= 0 -> Or_error.error_string "expand_letn: n must be positive"
  | Some [ singleton ] when n = 1 -> Ok (Leaf singleton)
  | Some _ when n = 1 ->
    Or_error.error_string
      "expand_letn: n may only be 1 if the length of the input list is exactly 1"
  | Some list ->
    let nodes = Nonempty_list.map list ~f:(fun x -> Leaf x) in
    Ok (balance ~n nodes)
;;
