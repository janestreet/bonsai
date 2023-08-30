open! Core
open! Bonsai_web_test
open! Bonsai_web
open Bonsai.Let_syntax

let shared_computation =
  Bonsai_web_ui_freeform_multiselect.Freeform_multiselect.create
    ~placeholder:"Select a value"
    ~split:(String.split ~on:',')
    ()
;;

let view_computation =
  let%sub _, vdom, _ = shared_computation in
  return vdom
;;

let view_and_set_computation =
  let%sub _, vdom, set = shared_computation in
  return (Value.both vdom set)
;;

let input_value handle value =
  Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:value
;;

let%expect_test "Initial multiselect state" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  Handle.show handle;
  [%expect
    {|
<div>
  <input type="text"
         list="bonsai_path_replaced_in_test"
         placeholder="Select a value"
         value=""
         #value=""
         onchange> </input>
</div>
 |}]
;;

let%expect_test "Select two elements" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  Handle.store_view handle;
  input_value handle "this is a thing";
  Handle.recompute_view handle;
  input_value handle "this is yet another thing";
  Handle.show_diff handle;
  (* Expected change: Selected options should disappear from <datalist> and appear as pills
     in a div that is rendered below datalist. *)
  [%expect
    {|
  <div>
    <input type="text"
           list="bonsai_path_replaced_in_test"
           placeholder="Select a value"
           value=""
           #value=""
           onchange> </input>
+|  <div class="bonsai-web-ui-freeform-multiselect-pills">
+|    <span tabindex="0" data-value="this is a thing" onclick onkeyup> this is a thing × </span>
+|    <span tabindex="0" data-value="this is yet another thing" onclick onkeyup> this is yet another thing × </span>
+|  </div>
  </div> |}]
;;

let%expect_test "Deselect an element" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  input_value handle "this is a thing";
  Handle.recompute_view handle;
  input_value handle "this is yet another thing";
  Handle.store_view handle;
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"[data-value='this is a thing']";
  Handle.show_diff handle;
  (* Expected change: this is a thing should disappear from pills *)
  [%expect
    {|
  <div>
    <input type="text"
           list="bonsai_path_replaced_in_test"
           placeholder="Select a value"
           value=""
           #value=""
           onchange> </input>
    <div class="bonsai-web-ui-freeform-multiselect-pills">
-|    <span tabindex="0" data-value="this is a thing" onclick onkeyup> this is a thing × </span>
      <span tabindex="0" data-value="this is yet another thing" onclick onkeyup> this is yet another thing × </span>
    </div>
  </div> |}]
;;

let%expect_test "set the elements" =
  let handle =
    Handle.create
      (module struct
        type incoming = String.Set.t
        type t = Vdom.Node.t * (String.Set.t -> unit Ui_effect.t)

        let view (vdom, _) =
          let module V = (val Result_spec.vdom Fn.id) in
          V.view vdom
        ;;

        let incoming (_, inject) = inject
      end)
      view_and_set_computation
  in
  Handle.store_view handle;
  Handle.do_actions
    handle
    [ String.Set.of_list [ "this is a thing"; "this is yet another thing" ] ];
  Handle.show_diff handle;
  [%expect
    {|
  <div>
    <input type="text"
           list="bonsai_path_replaced_in_test"
           placeholder="Select a value"
           value=""
           #value=""
           onchange> </input>
+|  <div class="bonsai-web-ui-freeform-multiselect-pills">
+|    <span tabindex="0" data-value="this is a thing" onclick onkeyup> this is a thing × </span>
+|    <span tabindex="0" data-value="this is yet another thing" onclick onkeyup> this is yet another thing × </span>
+|  </div>
  </div> |}]
;;

let%expect_test "input multiple elements" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  Handle.store_view handle;
  Handle.input_text
    handle
    ~get_vdom:Fn.id
    ~selector:"input"
    ~text:"this is a thing,this is yet another thing";
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
               value=""
               #value=""
               onchange> </input>
    +|  <div class="bonsai-web-ui-freeform-multiselect-pills">
    +|    <span tabindex="0" data-value="this is a thing" onclick onkeyup> this is a thing × </span>
    +|    <span tabindex="0" data-value="this is yet another thing" onclick onkeyup> this is yet another thing × </span>
    +|  </div>
      </div> |}]
;;
