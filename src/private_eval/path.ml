open! Core
open! Import

module Elem = struct
  let keyed ~compare id = stage (fun key -> Keyed.create ~key ~id ~compare)

  type t =
    | Subst_into_invert_lifecycles
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
    | Subst_into_invert_lifecycles -> "w"
    | Subst_from -> "x"
    | Subst_into -> "y"
    | Assoc k -> keyed_to_string k
    | Switch i -> int_to_string i
  ;;
end

module Run_length_encoding = struct
  type run =
    { element : Elem.t
    ; count : int
    }
  [@@deriving sexp_of]

  let sexp_of_run { element; count } =
    if count = 1
    then [%sexp_of: Elem.t] element
    else [%sexp ((element, count) : Elem.t * int)]
  ;;

  type t = run list [@@deriving sexp_of]

  (* We can't just use the [@@deriving compare] on [t] because of the custom logic when
     the two runs at the front of the encoding are the same. *)
  let rec compare a b =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | a :: al, b :: bl ->
      let c = [%compare: Elem.t] a.element b.element in
      if c = 0
      then (
        (* If the RLE is done correctly, these recursive calls will not recurse again
           (otherwise, we must have two of the same [Elem.t]s back-to-back). *)
        match Int.sign (a.count - b.count) with
        | Zero -> compare al bl
        | Pos -> compare ({ a with count = a.count - b.count } :: al) bl
        | Neg -> compare al ({ b with count = b.count - a.count } :: bl))
      else c
  ;;

  let of_elem_list (l : Elem.t list) =
    let rec helper ~(acc : run Reversed_list.t) (l : Elem.t list) : run Reversed_list.t =
      match acc, l with
      | [], first :: tl -> helper ~acc:Reversed_list.[ { element = first; count = 1 } ] tl
      | acc, [] -> acc
      | curr_acc :: acc_tl, curr_element :: tl ->
        (match [%compare.equal: Elem.t] curr_acc.element curr_element with
         | true ->
           let acc =
             Reversed_list.({ curr_acc with count = succ curr_acc.count } :: acc_tl)
           in
           helper ~acc tl
         | false ->
           let acc =
             Reversed_list.({ element = curr_element; count = 1 } :: curr_acc :: acc_tl)
           in
           helper ~acc tl)
    in
    helper ~acc:Reversed_list.[] l |> Reversed_list.rev
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
  ; mutable items_for_testing : Elem.t list Uopt.t
  ; mutable string_repr : t A.t
  ; mutable run_length_encoded_items : Run_length_encoding.t Uopt.t
  }

let run_length_encoding t =
  match Uopt.to_option t.run_length_encoded_items with
  | Some items -> items
  | None ->
    let run_length_encoded_items =
      Run_length_encoding.of_elem_list (Reversed_list.rev t.items_rev)
    in
    t.run_length_encoded_items <- Uopt.some run_length_encoded_items;
    run_length_encoded_items
;;

let sexp_of_t t = [%sexp_of: Run_length_encoding.t] (run_length_encoding t)

let compare a b =
  if phys_equal a b
  then 0
  else [%compare: Run_length_encoding.t] (run_length_encoding a) (run_length_encoding b)
;;

let empty =
  { items_for_testing = Uopt.some []
  ; items_rev = []
  ; string_repr = Stringified "bonsai_path"
  ; run_length_encoded_items = Uopt.some []
  }
;;

let append t ele =
  let items_rev = Reversed_list.(ele :: t.items_rev) in
  let items_for_testing = Uopt.none in
  let string_repr = A.Parts { parent = t; ele } in
  let run_length_encoded_items = Uopt.none in
  { items_rev; items_for_testing; string_repr; run_length_encoded_items }
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

module For_testing = struct
  let items t =
    match Uopt.to_option t.items_for_testing with
    | Some items -> items
    | None ->
      let items = Reversed_list.rev t.items_rev in
      t.items_for_testing <- Uopt.some items;
      items
  ;;

  let slow_but_correct_compare_for_bisimulation a b =
    if phys_equal a b then 0 else [%compare: Elem.t list] (items a) (items b)
  ;;
end
