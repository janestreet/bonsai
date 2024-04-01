open! Core
open! Bonsai_web.Cont

let my_failing_function () = failwith "oh no"

let component =
  Bonsai.Expert.Var.create ()
  |> Bonsai.Expert.Var.value
  |> Bonsai.map ~f:my_failing_function
;;

let () =
  let () = Async_js.init () in
  Bonsai_web.Start.start (fun _graph -> component)
;;

let () = Async_kernel.upon (Async_js.document_loaded ()) (fun () -> Ui_incr.stabilize ())
