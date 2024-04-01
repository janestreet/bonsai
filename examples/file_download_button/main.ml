open! Core
open! Bonsai_web.Cont

let component _graph =
  Bonsai.return
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

let () = Bonsai_web.Start.start component
