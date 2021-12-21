open! Core
open Composition_infix

module Die = struct
  type t = { num_faces : int } [@@deriving bin_io, compare, equal, sexp]

  let of_int num_faces = { num_faces }
  let to_string_hum { num_faces } = "d" ^ Int.to_string num_faces

  let roll ?(random_state = Random.State.default) { num_faces } =
    { Roll_result.Die.num_faces; result = Random.State.int_incl random_state 1 num_faces }
  ;;

  let parser =
    let open Angstrom in
    char 'd' *> take_while1 Char.is_digit >>| Int.of_string >>| of_int <?> "Die.Spec"
  ;;
end

type t =
  | Const of int
  | Prim of
      { count : int
      ; die : Die.t
      }
  | Add of t list
[@@deriving bin_io, compare, equal, sexp]

let add ts =
  match ts with
  | [] -> Const 0
  | [ t ] -> t
  | _ :: _ :: _ as ts -> Add ts
;;

let of_dice_and_const dice const =
  add
    ((if const <> 0 then [ Const const ] else [])
     @ List.filter_map dice ~f:(fun (count, die) ->
       if count > 0 then Some (Prim { count; die }) else None))
;;

let rec to_string_hum t =
  match t with
  | Const n -> Int.to_string n
  | Prim { count; die } ->
    (if count = 1 then "" else Int.to_string count) ^ Die.to_string_hum die
  | Add ts -> String.concat ~sep:" + " (List.map ts ~f:to_string_hum)
;;

let parser =
  let open Angstrom in
  let ws = skip_while Char.is_whitespace in
  let one =
    lift2
      (fun count die -> Prim { count; die })
      (take_while Char.is_digit
       >>| function
       | "" -> 1
       | s -> Int.of_string s)
      Die.parser
  in
  one |> sep_by1 (ws *> char '+' *> ws) >>| add <?> "Dice_set.Spec"
;;

let of_string string =
  Angstrom.(parse_string ~consume:All parser) string
  |> Result.map_error ~f:(Error.of_string >> Error.tag ~tag:"Roll_spec.of_string")
  |> ok_exn
;;

let%test_unit "of_string" = ignore (of_string "2d6 + d4" : t)

let rec roll ?(random_state = Random.State.default) t =
  match t with
  | Const n -> { Roll_result.dice = []; const = n }
  | Prim { count; die } ->
    { Roll_result.dice = List.init count ~f:(fun _ -> Die.roll die ~random_state)
    ; const = 0
    }
  | Add ts ->
    List.fold ts ~init:{ Roll_result.dice = []; const = 0 } ~f:(fun { dice; const } t ->
      let { Roll_result.dice = dice'; const = const' } = roll t in
      { dice = List.unordered_append dice dice'; const = const + const' })
;;
