open! Core
open! Bonsai_web

module Style =
[%css
stylesheet
  {|
.container {
  display: flex;
  flex-flow: row nowrap;
  outline: none;
}

.container > div {
  padding: 20px;
}
|}]

let component =
  let%map.Computation big_form = Big_form.component
  and list_form = List_form.component
  and form_with_submit = Form_with_submit.component
  and typed_record = Typed.component
  and file_form = File_form.form in
  Vdom.Node.div
    ~attrs:[ Style.container ]
    [ big_form; list_form; form_with_submit; typed_record; file_form ]
;;

let () = Bonsai_web.Start.start component
