open! Core
open! Bonsai_web

module Style =
  [%css.raw
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
  and typed_record = Typed.component in
  Vdom.Node.div
    ~attr:(Vdom.Attr.class_ Style.container)
    [ big_form; list_form; form_with_submit; typed_record ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
