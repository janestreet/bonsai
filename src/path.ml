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

module A = struct
  type 'a t =
    | Stringified of string
    | Parts of
        { parent : 'a
        ; ele : Elem.t
        }
end

type t =
  { items_rev : Elem.t Reversed_list.t
  ; mutable items : Elem.t list Uopt.t
  ; mutable string_repr : t A.t
  }

let items t =
  match Uopt.to_option t.items with
  | Some items -> items
  | None ->
    let items = Reversed_list.rev t.items_rev in
    t.items <- Uopt.some items;
    items
;;

let sexp_of_t t = [%sexp_of: Elem.t list] (items t)

let compare a b =
  if phys_equal a b then 0 else [%compare: Elem.t list] (items a) (items b)
;;

let empty =
  { items = Uopt.some []; items_rev = []; string_repr = Stringified "bonsai_path" }
;;

let append t ele =
  let items_rev = Reversed_list.(ele :: t.items_rev) in
  let items = Uopt.none in
  let string_repr = A.Parts { parent = t; ele } in
  { items_rev; items; string_repr }
;;

include Comparable.Make_plain (struct
  type nonrec t = t [@@deriving compare, sexp_of]
end)

let rec to_unique_identifier_string t =
  match t.string_repr with
  | Stringified s -> s
  | Parts { ele; parent } ->
    let parent_s = to_unique_identifier_string parent in
    let string_repr = parent_s ^ "_" ^ Elem.to_string ele in
    t.string_repr <- Stringified string_repr;
    string_repr
;;

let raise_duplicate path =
  raise_s
    [%message
      "BUG: [Bonsai.Path.t] should be unique for all components, but duplicate paths \
       were discovered."
        (path : t)]
;;
