open Core
open Bonsai_web

let make' ?(attrs = []) ~set_url ~page_to_string page children =
  let attrs =
    attrs
    @ [ Vdom.Attr.href (page_to_string page)
      ; Vdom.Attr.on_click (fun event ->
          if List.exists
               [ event##.ctrlKey; event##.shiftKey; event##.altKey; event##.metaKey ]
               ~f:Js_of_ocaml.Js.to_bool
          then
            (* If any modifier key is pressed, let the browser handle it as a normal
               <a>: Ctrl-Click opens in a new tab, etc *)
            Effect.Ignore
          else
            (* Otherwise perform an on-page navigation. *)
            Effect.Many [ set_url page; Effect.Prevent_default ])
      ]
  in
  Vdom.Node.a ~attrs children
;;

let make ?attrs ~set_url ~page_to_string page text =
  make' ?attrs ~set_url ~page_to_string page [ Vdom.Node.text text ]
;;
