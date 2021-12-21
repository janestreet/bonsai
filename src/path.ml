open! Core
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
    | Switch of int
  [@@deriving sexp_of, compare]

  let to_string =
    let offset = Char.to_int 'a' in
    let lower_nibble_to_alpha c = Int.bit_and c 0b1111 + offset |> Char.of_int_exn in
    let char_to_alpha buf c =
      let c = Char.to_int c in
      let lower = lower_nibble_to_alpha c in
      let upper = lower_nibble_to_alpha (Int.shift_right c 4) in
      Buffer.add_char buf upper;
      Buffer.add_char buf lower
    in
    let keyed_to_string k =
      let buf = Buffer.create 10 in
      Sexp.to_buffer_gen
        (Keyed.sexp_of_t k)
        ~buf
        ~add_char:char_to_alpha
        ~add_string:(fun buf string -> String.iter string ~f:(char_to_alpha buf));
      Buffer.contents buf
    in
    let int_to_string i =
      let buf = Buffer.create 4 in
      String.iter (Int.to_string i) ~f:(char_to_alpha buf);
      Buffer.contents buf
    in
    function
    | Subst_from -> "x"
    | Subst_into -> "y"
    | Assoc k -> keyed_to_string k
    | Switch i -> int_to_string i
  ;;
end

type t =
  { items : Elem.t list
  ; string_repr : string Lazy.t
  }

let sexp_of_t t = [%sexp_of: Elem.t list] t.items
let compare a b = [%compare: Elem.t list] a.items b.items
let empty = { items = []; string_repr = Lazy.return "bonsai_path" }

let append t ele =
  { items = t.items @ [ ele ]
  ; string_repr =
      lazy
        (let (lazy parent) = t.string_repr in
         parent ^ "_" ^ Elem.to_string ele)
  }
;;

include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving compare, sexp_of]
  end)

let to_unique_identifier_string t = Lazy.force t.string_repr
