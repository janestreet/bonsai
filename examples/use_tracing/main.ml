open! Core
open! Bonsai_web.Cont
module _ = Tracing_zero

let[@trace "hi"] component _graph =
  Bonsai.return
    (Vdom.Node.text
       (sprintf
          "Time_stamp_counter.now: %Ld\n"
          Time_stamp_counter.(now () |> to_int63 |> Int63.to_int64)))
;;

let () = Bonsai_web.Start.start component
