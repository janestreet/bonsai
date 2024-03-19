open Base
open Core
open Import
open Gen_js_api

type t = Ojs.t

let t_to_js x = x
let create data = Ojs.array_to_js (Ojs.array_to_js Ojs.float_to_js) data

let create' data ~x_to_js ~y_to_js =
  let row_to_js (x, data) =
    let values = Ojs.array_make (Array.length data + 1) in
    Ojs.array_set values 0 (x_to_js x);
    for i = 0 to Array.length data - 1 do
      Ojs.array_set values (i + 1) (y_to_js data.(i))
    done;
    values
  in
  Ojs.array_to_js row_to_js data
;;

let create_option data =
  create' data ~x_to_js:Ojs.float_to_js ~y_to_js:(Ojs.option_to_js Ojs.float_to_js)
;;

let time_ns_to_js time =
  let date =
    new%js Js.date_fromTimeValue (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_ms)
  in
  (Stdlib.Obj.magic (date : Js.date Js.t) : Ojs.t)
;;

let create_date data ~zone =
  let date_to_js date =
    Time_ns.of_date_ofday ~zone date Time_ns.Ofday.start_of_day |> time_ns_to_js
  in
  create' data ~x_to_js:date_to_js ~y_to_js:Ojs.float_to_js
;;

let create_time_ns data = create' data ~x_to_js:time_ns_to_js ~y_to_js:Ojs.float_to_js

let create_time_ns_option data =
  create' data ~x_to_js:time_ns_to_js ~y_to_js:(Ojs.option_to_js Ojs.float_to_js)
;;

let create_from_independent_series' ~min ~equal series =
  if Array.length series = 0
  then [||]
  else (
    let current_idxes = Array.map series ~f:(fun _ -> 0) in
    let safe_get series ~idx =
      if idx < Array.length series then Some series.(idx) else None
    in
    let next_x () =
      Array.fold2_exn ~init:None current_idxes series ~f:(fun earliest_x idx series ->
        match safe_get series ~idx with
        | None -> earliest_x
        | Some (x, _) ->
          Some
            (match earliest_x with
             | None -> x
             | Some earliest_x -> min x earliest_x))
    in
    (* This loop will make an array (in chronological order) of:
       {[ (x-value * (float option array)) ]}

       With points at the union of all series' xs.
    *)
    let rec loop ~data_acc =
      match next_x () with
      | None -> Array.of_list_rev data_acc
      | Some next_x ->
        let next_row =
          Array.mapi series ~f:(fun i series ->
            let idx = current_idxes.(i) in
            match safe_get series ~idx with
            | None -> None
            | Some (x, value) ->
              if equal x next_x
              then (
                (* increment index *)
                current_idxes.(i) <- idx + 1;
                Some value)
              else None)
        in
        loop ~data_acc:((next_x, next_row) :: data_acc)
    in
    loop ~data_acc:[])
;;

let create_from_independent_series series =
  let data = create_from_independent_series' ~min:Float.min ~equal:Float.equal series in
  create_option data
;;

let create_from_independent_time_series' series =
  create_from_independent_series' ~min:Time_ns.min ~equal:Time_ns.equal series
;;

let create_from_independent_time_series series =
  let data = create_from_independent_time_series' series in
  create' data ~x_to_js:time_ns_to_js ~y_to_js:(Ojs.option_to_js Ojs.float_to_js)
;;

let%expect_test "test [create_from_independent_time_series]" =
  let t1 = Time_ns.epoch in
  let t2 = Time_ns.add t1 Time_ns.Span.day in
  let t3 = Time_ns.add t2 Time_ns.Span.day in
  let t4 = Time_ns.add t3 Time_ns.Span.day in
  let ts1 = [| t1, 1.; t3, 3. |] in
  let ts2 = [| t2, 2.; t3, 3.; t4, 4. |] in
  create_from_independent_time_series' [| ts1; ts2 |]
  |> Array.iter ~f:(fun row ->
       [%sexp_of: Time_ns.Alternate_sexp.t * float option array] row |> print_s);
  [%expect
    {|
    ("1970-01-01 00:00:00Z" ((1) ()))
    ("1970-01-02 00:00:00Z" (() (2)))
    ("1970-01-03 00:00:00Z" ((3) (3)))
    ("1970-01-04 00:00:00Z" (() (4)))
    |}]
;;
