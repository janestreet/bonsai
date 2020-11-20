open! Core_kernel
open! Import

module Elem = struct
  module Keyed = struct
    type t =
      | T :
          { key : 'k
          ; id : 'k Type_equal.Id.t
          ; compare : 'k -> 'k -> int
          }
          -> t

    let compare
          (T { key = key1; id = id1; compare = compare1 })
          (T { key = key2; id = id2; compare = _ })
      =
      match Type_equal.Id.same_witness id1 id2 with
      | Some T -> compare1 key1 key2
      | None ->
        (* Use the Uid comparison function so that the comparator is stable.
           This function will never return 0 because we've already established
           that these type-ids are not equal*)
        Type_equal.Id.Uid.compare (Type_equal.Id.uid id1) (Type_equal.Id.uid id2)
    ;;

    let sexp_of_t (T { key; id; compare = _ }) = Type_equal.Id.to_sexp id key
    let create ~key ~id ~compare = T { key; id; compare }
  end

  let keyed ~compare id = stage (fun key -> Keyed.create ~key ~id ~compare)

  type t =
    | Subst_from
    | Subst_into
    | Assoc of Keyed.t
    | Enum of Keyed.t
  [@@deriving sexp_of, compare]
end

(* The path is stored with the child node first and the parent node last so that [append]
   is fast *)
type t = Elem.t list [@@deriving compare, sexp_of]

let empty = []
let append t ele = ele :: t

(* reverse before doing anything *)
let compare a b = compare (List.rev a) (List.rev b)
let sexp_of_t l = sexp_of_t (List.rev l)

include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving compare, sexp_of]
  end)

let to_unique_identifier_string t =
  let offset = Char.to_int 'a' in
  let lower_nibble_to_alpha c = Int.bit_and c 0b1111 + offset |> Char.of_int_exn in
  let char_to_alpha c =
    let c = Char.to_int c in
    let lower = lower_nibble_to_alpha c in
    let upper = lower_nibble_to_alpha (Int.shift_right c 4) in
    [ upper; lower ]
  in
  let keyed_to_string k =
    k
    |> Elem.Keyed.sexp_of_t
    |> Sexp.to_string_mach
    |> String.to_list
    |> List.bind ~f:char_to_alpha
    |> String.of_char_list
  in
  t
  |> List.rev
  |> List.map ~f:(function
    | Elem.Subst_from -> "x"
    | Subst_into -> "y"
    | Assoc k | Enum k -> keyed_to_string k)
  |> String.concat ~sep:"_"
  |> ( ^ ) "bonsai_path_"
;;
