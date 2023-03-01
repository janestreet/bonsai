open! Core
open! Bonsai_web

let my_failing_function () = failwith "oh no"

let component =
  Bonsai.Var.create () |> Bonsai.Var.value |> Bonsai.Value.map ~f:my_failing_function
;;

let () =
  let () = Async_js.init () in
  Bonsai_web.Start.start (Bonsai.read component)
;;

let () = Async_kernel.upon (Async_js.document_loaded ()) (fun () -> Ui_incr.stabilize ())
