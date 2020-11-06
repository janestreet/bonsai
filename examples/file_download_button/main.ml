open! Core_kernel
open! Bonsai_web

let component =
  Bonsai.const
    (Vdom_file_download_button.create
       ~button_text:"click me!"
       ~data:(fun () -> "hello there!")
       ~output_filename:"top_secret.txt"
       ())
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
