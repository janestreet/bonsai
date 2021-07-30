open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let local_storage_var =
  Bonsai_web.Persistent_var.create
    (module String)
    `Local_storage
    ~unique_id:"my-unique-id-local-storage"
    ~default:"This will be saved in local storage!"
;;

let session_storage_var =
  Bonsai_web.Persistent_var.create
    (module String)
    `Session_storage
    ~unique_id:"my-unique-id-session-storage"
    ~default:"This will be saved in session storage!"
;;

let display_text_var ~doc storage_var =
  let set_effect = Bonsai_web.Persistent_var.effect storage_var in
  return
  @@ let%map value = Bonsai_web.Persistent_var.value storage_var in
  Vdom.Node.div
    [ Vdom.Node.text doc
    ; Vdom.Node.br ()
    ; Vdom.Node.input
        ~attr:
          (Vdom.Attr.many
             [ Vdom.Attr.style (Css_gen.width (`Vw (Percent.of_mult 0.5)))
             ; Vdom.Attr.string_property "value" value
             ; Vdom.Attr.on_input (fun _ s -> set_effect s)
             ])
        []
    ]
;;

let component =
  let%sub local_storage_node =
    display_text_var
      ~doc:
        "Write some text in this box and refresh the page or close the tab and reopen \
         it; the text should still be there!"
      local_storage_var
  in
  let%sub session_storage_node =
    display_text_var
      ~doc:
        "Write some text in this box and refresh the page; the text should still be \
         there!"
      session_storage_var
  in
  return
  @@ let%map local_storage_node = local_storage_node
  and session_storage_node = session_storage_node in
  Vdom.Node.div [ local_storage_node; Vdom.Node.br (); session_storage_node ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
