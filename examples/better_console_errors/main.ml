open! Core
open! Bonsai_web

let my_failing_function () = failwith "oh no"

let component =
  Bonsai.Var.create () |> Bonsai.Var.value |> Bonsai.Value.map ~f:my_failing_function
;;

let (_ : _ Start.Handle.t) =
  let () = Async_js.init () in
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (Bonsai.read component)
;;

let () = Async_kernel.upon (Async_js.document_loaded ()) (fun () -> Ui_incr.stabilize ())
