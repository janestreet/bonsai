open! Core

type 'timeable_event t = { time : 'a. 'timeable_event -> f:(unit -> 'a) -> 'a }

let create ~start_timer ~stop_timer =
  let time event ~f =
    let timer = start_timer event in
    let result = f () in
    stop_timer timer;
    result
  in
  { time }
;;

type event =
  [ `Graph_application
  | `Preprocess
  | `Gather
  ]
[@@deriving to_string]

type event_timer = event t

let timer : event_timer ref = ref { time = (fun _ ~f -> f ()) }
let set_timer ~timer:new_timer = timer := new_timer
let timer () = !timer
