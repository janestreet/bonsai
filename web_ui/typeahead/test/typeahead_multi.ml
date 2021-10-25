open! Core
open! Bonsai_web_test
open! Bonsai_web
open  Bonsai.Let_syntax

let shared_computation =
  Bonsai_web_ui_typeahead.Typeahead.create_multi
    (module Data)
    ~all_options:(Value.return Data.all)
    ~placeholder:"Select a value"
    ~to_string:Data.to_string
    ~split:(String.split ~on:',')
;;

let view_computation =
  let%sub _, vdom, _ = shared_computation in
  return vdom
;;

let view_and_set_computation =
  let%sub _, vdom, set = shared_computation in
  return (Value.both vdom set)
;;

let view_and_result_computation =
  let%sub result, vdom, _ = shared_computation in
  return (Value.both vdom result)
;;

let input_value handle value =
  Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:(Data.to_string value)
;;

let%expect_test "Initial multi typeahead state" =
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
  <datalist id="bonsai_path_replaced_in_test">
    <option value="Option A"> Option A </option>
    <option value="Option B"> Option B </option>
    <option value="Option C"> Option C </option>
  </datalist>
</div>
 |}]
;;

let%expect_test "Select two elements" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  Handle.store_view handle;
  input_value handle Data.Option_B;
  input_value handle Data.Option_C;
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
    <datalist id="bonsai_path_replaced_in_test">
      <option value="Option A"> Option A </option>
-|    <option value="Option B"> Option B </option>
-|    <option value="Option C"> Option C </option>
+|  </datalist>
+|  <div class="bonsai-web-ui-typeahead-pills">
+|    <span tabindex="0" data-value="Option B" onclick onkeyup> Option B × </span>
+|    <span tabindex="0" data-value="Option C" onclick onkeyup> Option C × </span>
-|  </datalist>
+|  </div>
  </div> |}]
;;

let%expect_test "Deselect an element" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  input_value handle Data.Option_B;
  input_value handle Data.Option_C;
  Handle.store_view handle;
  Handle.click_on   handle ~get_vdom:Fn.id ~selector:"[data-value=Option B]";
  Handle.show_diff  handle;
  (* Expected change: Option B should disappear from the pill div and reappear in the
     datalist. *)
  [%expect
    {|
  <div>
    <input type="text"
           list="bonsai_path_replaced_in_test"
           placeholder="Select a value"
           value=""
           #value=""
           onchange> </input>
    <datalist id="bonsai_path_replaced_in_test">
      <option value="Option A"> Option A </option>
+|    <option value="Option B"> Option B </option>
    </datalist>
    <div class="bonsai-web-ui-typeahead-pills">
-|    <span tabindex="0" data-value="Option B" onclick onkeyup> Option B × </span>
      <span tabindex="0" data-value="Option C" onclick onkeyup> Option C × </span>
    </div>
  </div> |}]
;;

let%expect_test "set the elements" =
  let handle =
    Handle.create
      (module struct
        type incoming = Data.Set.t
        type t        = Vdom.Node.t * (Data.Set.t -> unit Ui_effect.t)

        let view (vdom, _) =
          let module V = (val Result_spec.vdom Fn.id) in
          V.view vdom
        ;;

        let incoming (_, inject) = inject
      end)
      view_and_set_computation
  in
  Handle.store_view handle;
  Handle.do_actions handle [ Data.Set.of_list [ Data.Option_C ] ];
  Handle.show_diff  handle;
  [%expect
    {|
  <div>
    <input type="text"
           list="bonsai_path_replaced_in_test"
           placeholder="Select a value"
           value=""
           #value=""
           onchange> </input>
    <datalist id="bonsai_path_replaced_in_test">
      <option value="Option A"> Option A </option>
      <option value="Option B"> Option B </option>
-|    <option value="Option C"> Option C </option>
    </datalist>
+|  <div class="bonsai-web-ui-typeahead-pills">
+|    <span tabindex="0" data-value="Option C" onclick onkeyup> Option C × </span>
+|  </div>
  </div> |}]
;;

let%expect_test "Select two elements using partial inputs" =
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = Vdom.Node.t * Data.Set.t

           let sexp_of_t (_view, result) = [%sexp_of: Data.Set.t] result
         end))
      view_and_result_computation
  in
  Handle.show handle;
  [%expect {| () |}];
  Handle.input_text handle ~get_vdom:Tuple2.get1 ~selector:"input" ~text:"b";
  Handle.show       handle;
  [%expect {| (Option_B) |}];
  Handle.input_text handle ~get_vdom:Tuple2.get1 ~selector:"input" ~text:"C";
  Handle.show       handle;
  [%expect {| (Option_B Option_C) |}]
;;

let%expect_test "input multiple elements" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  Handle.store_view handle;
  Handle.input_text
    handle
    ~get_vdom:Fn.id
    ~selector:"input"
    ~text:(Data.to_string Data.Option_A ^ "," ^ Data.to_string Data.Option_B);
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
        <datalist id="bonsai_path_replaced_in_test">
    -|    <option value="Option A"> Option A </option>
    -|    <option value="Option B"> Option B </option>
          <option value="Option C"> Option C </option>
        </datalist>
    +|  <div class="bonsai-web-ui-typeahead-pills">
    +|    <span tabindex="0" data-value="Option A" onclick onkeyup> Option A × </span>
    +|    <span tabindex="0" data-value="Option B" onclick onkeyup> Option B × </span>
    +|  </div>
      </div> |}]
;;
