open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let text_input =
  let%sub text_contents, set_text_contents = Bonsai.state_opt [%here] (module String) in
  return
  @@ let%map text_contents = text_contents
  and set_text_contents = set_text_contents in
  Vdom_input_widgets.Entry.text ~value:text_contents ~on_input:set_text_contents ()
;;

let date_input =
  let%sub date_contents, set_date_contents = Bonsai.state_opt [%here] (module Date) in
  return
  @@ let%map date_contents = date_contents
  and set_date_contents = set_date_contents in
  Vdom_input_widgets.Entry.date ~value:date_contents ~on_input:set_date_contents ()
;;

let text_input_first_input =
  let%sub text_input_first_contents, set_text_input_first_contents =
    Bonsai.state [%here] (module Bool) ~default_model:false
  in
  return
  @@ let%map text_input_first_contents = text_input_first_contents
  and set_text_input_first_contents = set_text_input_first_contents in
  ( text_input_first_contents
  , Vdom_input_widgets.Checkbox.simple
      ~is_checked:text_input_first_contents
      ~label:
        {| When checked, the text input will be placed first in the DOM. Entry some 
         data into both inputs below and check this box - the data in both boxes 
         should be preserved. If this fails, check the console for an error message.
         It should be noted that this example demonstrates a bug in the virtual-dom
         library. |}
      ~on_toggle:(fun () ->
        set_text_input_first_contents (not text_input_first_contents))
      () )
;;

let wrap_in_div nodes = nodes |> Value.all >>| Vdom.Node.div |> return

let component =
  let%sub text_input = text_input in
  let%sub date_input = date_input in
  let%sub text_input_first, text_input_first_input = text_input_first_input in
  if%sub text_input_first
  then wrap_in_div [ text_input_first_input; text_input; date_input ]
  else wrap_in_div [ text_input_first_input; date_input; text_input ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
