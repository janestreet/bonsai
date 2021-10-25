open! Core

type t =
  { symbol : string
  ; edge : float
  ; max_edge : float
  ; bsize : int
  ; bid : float
  ; ask : float
  ; asize : int
  ; position : int
  ; last_fill : Time_ns.t option
  ; trader : string
  }
[@@deriving compare, fields, typed_fields]

let random () : t =
  let fix_digits x = Float.round (x *. 100.) /. 100. in
  let symbol =
    let rchar () = Char.to_int 'A' + Random.int 26 |> Char.of_int_exn in
    String.init 4 ~f:(fun (_ : int) -> rchar ())
  in
  let fair = 10. +. (Float.of_int (Random.int 10000) /. 100.) in
  let bsize = (1 + Random.int 20) * 100 in
  let asize = Int.max 100 (bsize + (100 * (Random.int 5 - 2))) in
  let bid = fix_digits (fair -. (Float.of_int (Random.int 20) /. 100.)) in
  let ask = fix_digits (fair +. (Float.of_int (Random.int 20) /. 100.)) in
  let edge = fix_digits (Float.of_int (Random.int 10) /. 100.) in
  let max_edge = fix_digits (edge +. (Float.of_int (Random.int 10) /. 100.)) in
  let position = (Random.int 500 - 250) * 100 in
  let last_fill = None in
  let trader = List.random_element_exn [ "hsimmons"; "bkent"; "qhayes"; "gfernandez" ] in
  { symbol; edge; max_edge; trader; bsize; asize; bid; ask; position; last_fill }
;;

let many_random n =
  List.init n ~f:(fun _ -> random ())
  |> List.fold ~init:String.Map.empty ~f:(fun acc data ->
    let { symbol = key; _ } = data in
    Map.set acc ~key ~data)
;;
