open! Core

type t =
  { name : string
  ; int_id : int
  }
[@@deriving compare, equal, fields, sexp]

let sample_data =
  lazy
    (List.mapi [ "prod"; "dev"; "test" ] ~f:(fun i suffix ->
       List.mapi [ "bonsai"; "incremental"; "app" ] ~f:(fun j name ->
         let name = String.concat ~sep:"-" [ name; suffix ] in
         let int_id = (10 * i) + j in
         name, Fields.create ~name ~int_id))
     |> List.concat
     |> String.Map.of_alist_exn)
;;
