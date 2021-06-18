open! Core
open! Bonsai_web

let component =
  Bonsai.const
    Vdom_file_download.(
      Button.create
        ~button_text:"click me!"
        ~get_download:(fun () ->
          create
            ~contents:"hello there!"
            ~filename:"top_secret.txt"
            ~mimetype:"text/plain")
        ())
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
