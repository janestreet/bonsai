open! Core
open! Bonsai_web
module _ = Tracing_zero

let[@trace "hi"] component =
  Bonsai.const
    (Vdom.Node.text
       (sprintf
          "Time_stamp_counter.now: %Ld\n"
          Time_stamp_counter.(now () |> to_int63 |> Int63.to_int64)))
;;

let () = Bonsai_web.Start.start component
