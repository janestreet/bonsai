open! Core_kernel
open! Async_kernel
open! Bonsai_web

let component =
  Bonsai.pure ~f:(fun ((), inject) ->
    let view =
      Vdom.Node.button
        [ Vdom.Attr.on_click (fun _ -> inject ()) ]
        [ Vdom.Node.text "click me to print a thing to the console" ]
    in
    let inject = Nothing.unreachable_code in
    view, inject)
;;

let () =
  let handle =
    Start.start
      ~initial_input:()
      ~initial_model:()
      ~bind_to_element_with_id:"app"
      component
  in
  don't_wait_for
  @@ Pipe.iter (Start.Handle.outgoing handle) ~f:(fun () ->
    print_endline "a thing";
    Deferred.unit)
;;
