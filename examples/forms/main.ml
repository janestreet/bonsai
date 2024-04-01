open! Core
open! Bonsai_web.Cont

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

let component graph =
  let%map.Bonsai big_form = Big_form.component graph
  and list_form = List_form.component graph
  and form_with_submit = Form_with_submit.component graph
  and typed_record = Typed.component graph
  and file_form = File_form.form graph in
  Vdom.Node.div
    ~attrs:[ Style.container ]
    [ big_form; list_form; form_with_submit; typed_record; file_form ]
;;

let () = Bonsai_web.Start.start component
