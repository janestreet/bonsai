open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
open! Import
open Virtual_dom

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

let test_view (type a) ?customizations (module M : S with type t = a) (t : a) =
  let view =
    Auto_generated.view
      ?customizations
      (module M)
      (Bonsai.Value.return t)
      `This_view_may_change_without_notice
  in
  let handle = Handle.create (Result_spec.vdom Fn.id) view in
  Handle.show handle
;;

let sexp_form_handle
  (type a)
  ?optimize
  ?get_vdom
  ?customizations
  (module M : S with type t = a)
  =
  let form = Auto_generated.form (module M) ?customizations () in
  Handle.create ?optimize (form_result_spec ?get_vdom M.sexp_of_t) form
;;

let%expect_test "any" =
  test_view (module Sexp) [%message "this is a message" (5 : int) ("hi" : string)];
  [%expect {| <pre> ("this is a message" (5 5) ("\"hi\"" hi)) </pre> |}]
;;

let%expect_test "bool" =
  test_view (module Bool) true;
  [%expect {| <span class="with_whitespace_hash_replaced_in_test"> true </span> |}];
  test_view (module Bool) false;
  [%expect {| <span class="with_whitespace_hash_replaced_in_test"> false </span> |}]
;;

let%expect_test "char" =
  test_view (module Char) 'a';
  [%expect {| <pre> a </pre> |}];
  test_view (module Char) ' ';
  [%expect {| <pre>   </pre> |}]
;;

let%expect_test "int" =
  test_view (module Int) 5;
  [%expect {| <pre> 5 </pre> |}];
  test_view (module Int) (-10);
  [%expect {| <pre> -10 </pre> |}]
;;

let%expect_test "float" =
  test_view (module Float) 123.456;
  [%expect {| <pre> 123.456 </pre> |}];
  test_view (module Float) 1e10;
  [%expect {| <pre> 10000000000 </pre> |}]
;;

let%expect_test "string" =
  test_view (module String) "hello world";
  [%expect {| <span class="with_whitespace_hash_replaced_in_test"> hello world </span> |}]
;;

let%expect_test "option" =
  let module T = struct
    type t = string option [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) (Some "abc");
  [%expect {| <span class="with_whitespace_hash_replaced_in_test"> abc </span> |}];
  test_view (module T) None;
  [%expect {| <pre> None </pre> |}];
  test_view (module T) (Some "None");
  [%expect {| <span class="with_whitespace_hash_replaced_in_test"> None </span> |}]
;;

let%expect_test "list" =
  let module T = struct
    type t = int list [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) [];
  [%expect {| <pre> [] </pre> |}];
  test_view (module T) [ 5 ];
  [%expect {|
    <ol>
      <li>
        <pre> 5 </pre>
      </li>
    </ol> |}];
  test_view (module T) [ 1; 2; 3; 4; 5 ];
  [%expect
    {|
    <ol>
      <li>
        <pre> 1 </pre>
      </li>
      <li>
        <pre> 2 </pre>
      </li>
      <li>
        <pre> 3 </pre>
      </li>
      <li>
        <pre> 4 </pre>
      </li>
      <li>
        <pre> 5 </pre>
      </li>
    </ol> |}]
;;

let%expect_test "variant" =
  let module T = struct
    type t =
      | A of string
      | B of int
      | C
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) (A "hi");
  [%expect
    {|
    <div>
      A
      <ul>
        <li>
          <span class="with_whitespace_hash_replaced_in_test"> hi </span>
        </li>
      </ul>
    </div> |}];
  test_view (module T) (B 3);
  [%expect
    {|
    <div>
      B
      <ul>
        <li>
          <pre> 3 </pre>
        </li>
      </ul>
    </div> |}];
  test_view (module T) C;
  [%expect {|
    <div> C </div> |}]
;;

let%expect_test "recursion" =
  let module T = struct
    type t =
      | A
      | B of t * int
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) A;
  [%expect {|
    <div> A </div> |}];
  test_view (module T) (B (A, 1));
  [%expect
    {|
    <div>
      B
      <ul>
        <li>
          <div> A </div>
        </li>
        <li>
          <pre> 1 </pre>
        </li>
      </ul>
    </div> |}]
;;

let%expect_test "recursion 2" =
  let module T = struct
    type t =
      | B of t * int
      | A
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) A;
  [%expect {|
    <div> A </div> |}];
  test_view (module T) (B (A, 1));
  [%expect
    {|
    <div>
      B
      <ul>
        <li>
          <div> A </div>
        </li>
        <li>
          <pre> 1 </pre>
        </li>
      </ul>
    </div> |}]
;;

let%expect_test "record" =
  let module T = struct
    type t =
      { a : int * int
      ; b : string
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) { T.a = 1, -1; b = "hi" };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <ol>
            <li>
              <pre> 1 </pre>
            </li>
            <li>
              <pre> -1 </pre>
            </li>
          </ol>
        </div>
      </li>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> b </span>
          <br> </br>
          <span class="with_whitespace_hash_replaced_in_test"> hi </span>
        </div>
      </li>
    </ul> |}]
;;

let%expect_test "record whose fields are non-alphabetical" =
  let module T = struct
    type t =
      { b : string
      ; a : int * int
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) { T.a = 1, -1; b = "hi" };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> b </span>
          <br> </br>
          <span class="with_whitespace_hash_replaced_in_test"> hi </span>
        </div>
      </li>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <ol>
            <li>
              <pre> 1 </pre>
            </li>
            <li>
              <pre> -1 </pre>
            </li>
          </ol>
        </div>
      </li>
    </ul> |}]
;;

let%expect_test "record with list field" =
  let module T = struct
    type t = { a : int list } [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) { T.a = [] };
  test_view (module T) { T.a = [ 0 ] };
  test_view (module T) { T.a = [ 0; 1 ] };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <pre> [] </pre>
        </div>
      </li>
    </ul>
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <ol>
            <li>
              <pre> 0 </pre>
            </li>
          </ol>
        </div>
      </li>
    </ul>
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <ol>
            <li>
              <pre> 0 </pre>
            </li>
            <li>
              <pre> 1 </pre>
            </li>
          </ol>
        </div>
      </li>
    </ul> |}]
;;

let%expect_test "record with list field" =
  let module T = struct
    type t = { a : int list [@sexp.list] } [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) { T.a = [] };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <pre> Non-required field not present </pre>
        </div>
      </li>
    </ul> |}];
  test_view (module T) { T.a = [ 0 ] };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <ol>
            <li>
              <pre> 0 </pre>
            </li>
          </ol>
        </div>
      </li>
    </ul> |}];
  test_view (module T) { T.a = [ 0; 1 ] };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <ol>
            <li>
              <pre> 0 </pre>
            </li>
            <li>
              <pre> 1 </pre>
            </li>
          </ol>
        </div>
      </li>
    </ul> |}]
;;

let%expect_test "record with optional field" =
  let module T = struct
    type t = { a : int [@sexp.default 0] } [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) { T.a = 0 };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <pre> 0 </pre>
        </div>
      </li>
    </ul> |}];
  test_view (module T) { T.a = 1 };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <pre> 1 </pre>
        </div>
      </li>
    </ul> |}];
  test_view (module T) { T.a = 2 };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <pre> 2 </pre>
        </div>
      </li>
    </ul> |}]
;;

let%expect_test "record with many of the field annotations" =
  let module T = struct
    type t =
      { a : int option [@sexp.option]
      ; b : int list [@sexp.list]
      ; c : bool [@sexp.bool]
      ; d : int array [@sexp.array]
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_view (module T) { T.a = None; b = []; c = false; d = [||] };
  [%expect
    {|
    <ul>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> a </span>
          <br> </br>
          <pre> Non-required field not present </pre>
        </div>
      </li>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> b </span>
          <br> </br>
          <pre> Non-required field not present </pre>
        </div>
      </li>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> c </span>
          <br> </br>
          <pre> Non-required field not present </pre>
        </div>
      </li>
      <li>
        <div>
          <span class="record_field_name_hash_replaced_in_test"> d </span>
          <br> </br>
          <pre> Non-required field not present </pre>
        </div>
      </li>
    </ul> |}]
;;

let%expect_test "custom view for time" =
  let customize_t =
    Auto_generated.Customization.create_for_view
      ~apply_to_tag:(fun ~key ~value ->
        String.equal key Sexplib0.Sexp_grammar.type_name_tag
        && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Alternate_sexp.t"))
      (fun sexp ->
        let%arr sexp = sexp in
        let time = [%of_sexp: Time_ns.Alternate_sexp.t] sexp in
        List.map
          ~f:Vdom.Node.text
          (Time_ns.to_string_abs_parts ~zone:Time_float.Zone.utc time)
        |> Vdom.Node.pre)
  in
  test_view
    ~customizations:[ customize_t ]
    (module Time_ns.Alternate_sexp)
    (Time_ns.of_string_with_utc_offset "2022-04-05 13:31:55.0Z");
  [%expect {| <pre> 2022-04-05 13:31:55.000000000Z </pre> |}]
;;

let%expect_test "nothing form" =
  let module T = struct
    type t = | [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect {|
    (Error "no grammars in union")

    ==============
    <div> </div> |}]
;;

let%expect_test "Union like Css_gen form" =
  let module T = struct
    type global =
      [ `Inherit
      | `Initial
      ]
    [@@deriving sexp, sexp_grammar]

    type t =
      [ `Var of string
      | global
      ]
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> inherit </option>
      <option value="2" #selected="false"> initial </option>
      <option value="3" #selected="false"> var </option>
    </select> |}]
;;

let%expect_test "setting option form" =
  let module T = struct
    type t = int option [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <input type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           onclick
           style={
             margin-left: 0px;
           }> </input> |}];
  Handle.do_actions handle [ Some 3 ];
  Handle.show handle;
  [%expect
    {|
    (Ok (3))

    ==============
    <div>
      <input type="checkbox"
             id="bonsai_path_replaced_in_test"
             #checked="true"
             onclick
             style={
               margin-left: 0px;
             }> </input>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=3
             oninput> </input>
    </div> |}];
  Handle.do_actions handle [ None ];
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <input type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           onclick
           style={
             margin-left: 0px;
           }> </input> |}]
;;

let%expect_test "interacting with option form" =
  let module T = struct
    type t = int option [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <input type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           onclick
           style={
             margin-left: 0px;
           }> </input> |}];
  Handle.set_checkbox handle ~get_vdom ~selector:"input" ~checked:true;
  Handle.show handle;
  [%expect
    {|
    (Ok (0))

    ==============
    <div>
      <input type="checkbox"
             id="bonsai_path_replaced_in_test"
             #checked="true"
             onclick
             style={
               margin-left: 0px;
             }> </input>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=0
             oninput> </input>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(2)" ~text:"3";
  Handle.show handle;
  [%expect
    {|
    (Ok (3))

    ==============
    <div>
      <input type="checkbox"
             id="bonsai_path_replaced_in_test"
             #checked="true"
             onclick
             style={
               margin-left: 0px;
             }> </input>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=3
             oninput> </input>
    </div> |}]
;;

let%expect_test "record's field order is preserved" =
  (* In this example, the fields are in alphabetical order. *)
  let module T = struct
    type t =
      { a : int
      ; b : int
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 0)
      (b 0)))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> a </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   oninput> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> b </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   oninput> </input>
          </td>
        </tr>
      </tbody>
    </table> |}];
  (* In this example, the fields are not in alphabetical order *)
  let module T = struct
    type t =
      { b : int
      ; a : int
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (b 0)
      (a 0)))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> b </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   oninput> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> a </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   oninput> </input>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "a tuple form gets labels on the elements" =
  let module T = struct
    type t = int * string [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok (0 ""))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> 1st </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   oninput> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> 2nd </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "a list field within a record form gets a label" =
  let module T = struct
    type t =
      { a : int
      ; b : string list
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok ((a 0) (b ())))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> a </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   oninput> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <label style={ display: block; }> b </label>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" onclick> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "a nested record gets a label" =
  let module T = struct
    type r =
      { c : string
      ; d : int
      }
    [@@deriving sexp, sexp_grammar]

    type t =
      { a : int
      ; b : r
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 0)
      (b (
        (c "")
        (d 0)))))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> a </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   oninput> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <label style={ display: block; }> b </label>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 2em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> c </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 2em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> d </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   oninput> </input>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "record with doc comments gets a tooltip" =
  let module T = struct
    type t = { b : string (** doc comment *) }
    [@@deriving sexp, sexp_grammar ~tags_of_doc_comments]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok ((b "")))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> b </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
              <div class="container_hash_replaced_in_test">
                <label class="label_hash_replaced_in_test" style={ color: blue; }>
                  <input type="checkbox" tabindex="-1" class="checkbox_hash_replaced_in_test"> </input>
                  <span class="span_hash_replaced_in_test"> ⓘ </span>
                  <div class="above_hash_replaced_in_test text_hash_replaced_in_test"
                       style={
                         border: 1px solid darkblue;
                         color: black;
                         background-color: azure;
                       }>  doc comment  </div>
                </label>
              </div>

            </div>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "variant with doc comments gets a tooltip" =
  let module T = struct
    type t =
      | Foo (** This is a foo! *)
      | Bar (** This is a bar! *)
    [@@deriving sexp, sexp_grammar ~tags_of_doc_comments]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.do_actions handle [ Foo ];
  (* Note: we see there's a tooltip with "This is a foo!" inside *)
  Handle.show handle;
  [%expect
    {|
    (Ok Foo)

    ==============
    <table>
      <tbody>
        <tr>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <div style={ display: flex; }>
              <select id="bonsai_path_replaced_in_test"
                      class="widget-dropdown"
                      onchange
                      style={
                        width: 100.00%;
                      }>
                <option value="0" #selected="false">  </option>
                <option value="1" #selected="true"> foo </option>
                <option value="2" #selected="false"> bar </option>
              </select>
              <span class="inline_padding_hash_replaced_in_test right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
                ?
                <div class="scrollable_tooltip_hash_replaced_in_test tooltip_hash_replaced_in_test"
                     custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
                  <div style={ display: flex; flex-direction: column; row-gap: 0.15rem; }>
                    <div style={ display: flex; flex-direction: column; }>
                      <span class="bold_text_hash_replaced_in_test"> Bar </span>
                       This is a bar!
                    </div>
                    <div style={ display: flex; flex-direction: column; }>
                      <span class="bold_text_hash_replaced_in_test"> Foo </span>
                       This is a foo!
                    </div>
                  </div>
                </div>
              </span>
            </div>
          </td>
        </tr>
      </tbody>
    </table> |}];
  Handle.do_actions handle [ Bar ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok Foo)
    +|(Ok Bar)

      ==============
      <table>
        <tbody>
          <tr>
            <td style={
                  padding-left: 0em;
                  padding-right: 1em;
                  text-align: left;
                  font-weight: bold;
                  user-select: none;
                }>  </td>
            <td>
              <div style={ display: flex; }>
                <select id="bonsai_path_replaced_in_test"
                        class="widget-dropdown"
                        onchange
                        style={
                          width: 100.00%;
                        }>
                  <option value="0" #selected="false">  </option>
    -|            <option value="1" #selected="true"> foo </option>
    +|            <option value="1" #selected="false"> foo </option>
    -|            <option value="2" #selected="false"> bar </option>
    +|            <option value="2" #selected="true"> bar </option>
                </select>
                <span class="inline_padding_hash_replaced_in_test right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
                  ?
                  <div class="scrollable_tooltip_hash_replaced_in_test tooltip_hash_replaced_in_test"
                       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
                    <div style={ display: flex; flex-direction: column; row-gap: 0.15rem; }>
                      <div style={ display: flex; flex-direction: column; }>
                        <span class="bold_text_hash_replaced_in_test"> Bar </span>
                         This is a bar!
                      </div>
                      <div style={ display: flex; flex-direction: column; }>
                        <span class="bold_text_hash_replaced_in_test"> Foo </span>
                         This is a foo!
                      </div>
                    </div>
                  </div> |}]
;;

let%expect_test "setting variant forms" =
  let module T = struct
    type t =
      | A
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select> |}];
  Handle.do_actions handle [ T.A ];
  Handle.show handle;
  [%expect
    {|
    (Ok A)

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="true"> a </option>
      <option value="2" #selected="false"> b </option>
    </select> |}];
  Handle.do_actions handle [ T.B 2 ];
  Handle.show handle;
  [%expect
    {|
    (Ok (B 2))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=2
             oninput> </input>
    </div> |}]
;;

let%expect_test "interacting with variant forms" =
  let module T = struct
    type t =
      | A
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.show handle;
  [%expect
    {|
    (Ok A)

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="true"> a </option>
      <option value="2" #selected="false"> b </option>
    </select> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok (B 0))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=0
             oninput> </input>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"3";
  Handle.show handle;
  [%expect
    {|
    (Ok (B 3))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=3
             oninput> </input>
    </div> |}]
;;

let%expect_test "setting recursive variant form" =
  let module M = struct
    type t =
      | Empty
      | Cons of string * t
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> empty </option>
      <option value="2" #selected="false"> cons </option>
    </select> |}];
  Handle.do_actions handle [ Cons ("hello", Cons ("there", Cons ("world", Empty))) ];
  Handle.show handle;
  [%expect
    {|
    (Ok (Cons hello (Cons there (Cons world Empty))))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hello oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=there oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=world oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> empty </option>
        <option value="2" #selected="false"> cons </option>
      </select>
    </div> |}]
;;

let%expect_test "setting recursive variant form 2" =
  let module M = struct
    type t =
      | Cons of string * t
      | Empty
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> cons </option>
      <option value="2" #selected="false"> empty </option>
    </select> |}];
  Handle.do_actions handle [ Cons ("hello", Cons ("there", Cons ("world", Empty))) ];
  Handle.show handle;
  [%expect
    {|
    (Ok (Cons hello (Cons there (Cons world Empty))))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> cons </option>
        <option value="2" #selected="false"> empty </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hello oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> cons </option>
        <option value="2" #selected="false"> empty </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=there oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> cons </option>
        <option value="2" #selected="false"> empty </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=world oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> cons </option>
        <option value="2" #selected="true"> empty </option>
      </select>
    </div> |}]
;;

let%expect_test "interacting with recursive variant form" =
  let module M = struct
    type t =
      | Empty
      | Cons of string * t
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> empty </option>
      <option value="2" #selected="false"> cons </option>
    </select> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="true">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="false"> cons </option>
      </select>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"textarea" ~text:"hello";
  Handle.change handle ~get_vdom ~selector:"select:nth-child(3)" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hello oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="true">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="false"> cons </option>
      </select>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"textarea:nth-child(4)" ~text:"world";
  Handle.change handle ~get_vdom ~selector:"select:nth-child(5)" ~value:"1";
  Handle.show handle;
  [%expect
    {|
    (Ok (Cons hello (Cons world Empty)))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hello oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=world oninput> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> empty </option>
        <option value="2" #selected="false"> cons </option>
      </select>
    </div> |}]
;;

let%expect_test "setting custom time form" =
  let customize_time =
    Auto_generated.Customization.constant_form
      (module Time_ns.Alternate_sexp)
      ~apply_to_tag:(fun ~key ~value ->
        String.equal key Sexplib0.Sexp_grammar.type_name_tag
        && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Alternate_sexp.t"))
      (Form.Elements.Date_time.datetime_local ())
  in
  let handle =
    sexp_form_handle ~customizations:[ customize_time ] (module Time_ns.Alternate_sexp)
  in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input> |}];
  Handle.do_actions handle [ Time_ns.of_string_with_utc_offset "2022-04-05 13:31:55Z" ];
  Handle.show handle;
  [%expect
    {|
    (Ok "2022-04-05 13:31:55Z")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=2022-04-05T13:31:55
           oninput> </input> |}]
;;

let%expect_test "interacting with custom time form" =
  let customize_time =
    Auto_generated.Customization.constant_form
      (module Time_ns.Alternate_sexp)
      ~apply_to_tag:(fun ~key ~value ->
        String.equal key Sexplib0.Sexp_grammar.type_name_tag
        && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Alternate_sexp.t"))
      (Form.Elements.Date_time.datetime_local ())
  in
  let handle =
    sexp_form_handle ~customizations:[ customize_time ] (module Time_ns.Alternate_sexp)
  in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2022-04-05T09:31:55";
  Handle.show handle;
  [%expect
    {|
    (Ok "2022-04-05 09:31:00Z")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=2022-04-05T09:31:00
           oninput> </input> |}]
;;

let%expect_test "customizing a tuple within a list" =
  let module M = struct
    module Pair = struct
      type t = (int * int[@tag Sexplib0.Sexp_grammar.type_name_tag = Atom "my_pair"])
      [@@deriving sexp, sexp_grammar]
    end

    type t = Pair.t list [@@deriving sexp, sexp_grammar]
  end
  in
  let customize_pair =
    Auto_generated.Customization.transform_form
      ~apply_to_tag:(fun ~key ~value ->
        String.equal key Sexplib0.Sexp_grammar.type_name_tag
        && Sexp.equal value ([%sexp_of: string] "my_pair"))
      (fun (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Value.t) ~recurse ->
        let%sub grammar =
          let%arr with_tag = with_tag in
          with_tag.grammar
        in
        match%sub (grammar : Sexplib0.Sexp_grammar.grammar Value.t) with
        | List (Cons (first, Cons (second, Empty))) ->
          let%sub first = recurse first in
          let%sub second = recurse second in
          let%arr first = first
          and second = second in
          let view =
            Form.View.tuple
              [ Form.view (Form.label "Key" first); Form.view (Form.label "Data" second) ]
          in
          let value =
            match Or_error.both (Form.value first) (Form.value second) with
            | Ok (first, second) -> Ok (Sexp.List [ first; second ])
            | Error _ as err -> err
          in
          let set sexp =
            match sexp with
            | Sexp.List [ first_val; second_val ] ->
              Effect.Many [ Form.set first first_val; Form.set second second_val ]
            | _ -> Effect.Ignore
          in
          Form.Expert.create ~value ~view ~set
        | _ -> recurse grammar)
  in
  let handle =
    sexp_form_handle
      ~get_vdom:get_vdom_detailed
      ~customizations:[ customize_pair ]
      (module M)
  in
  Handle.do_actions handle [ [ 1, 2 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((1 2)))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div>
              0 -
              <button type="button"
                      onclick
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Key </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   oninput> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Data </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   oninput> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" onclick> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "customizing an alist" =
  let module M = struct
    type t = (string, int) List.Assoc.t [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.do_actions handle [ [ "hi", 2 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((hi 2)))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div>
              0 -
              <button type="button"
                      onclick
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Key </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hi oninput> </textarea>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Data </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   oninput> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" onclick> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "customizing a map" =
  let module M = struct
    type t = int Map.M(String).t [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.do_actions handle [ String.Map.of_alist_exn [ "hi", 2 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((hi 2)))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div>
              0 -
              <button type="button"
                      onclick
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Key </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hi oninput> </textarea>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Data </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   oninput> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" onclick> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "optional fields are not rendered at first, and the default is selected" =
  let module M = struct
    type t = { a : int [@default 0] } [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok ((a 0)))

    ==============
    <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" onclick> [default] </div> |}]
;;

let%expect_test "interacting with an optional field" =
  let module M = struct
    type t =
      { a : int [@default 0]
      ; b : string
      }
    [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 0)
      (b "")))

    ==============
    <div>
      <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" onclick> [default] </div>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"textarea" ~text:"foo";
  Handle.recompute_view handle;
  Handle.click_on handle ~get_vdom ~selector:"div > div";
  Handle.recompute_view handle;
  Handle.input_text handle ~get_vdom ~selector:"div > input" ~text:"1";
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 1)
      (b foo)))

    ==============
    <div>
      <div class="override_showing_hash_replaced_in_test override_text_hash_replaced_in_test" onclick> [override] </div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=1
             oninput> </input>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=foo oninput> </textarea>
    </div> |}]
;;

let%expect_test "setting into an optional field without the value in the sexp" =
  let module M = struct
    type t =
      { a : int [@default 0]
      ; b : string
      }
    [@@deriving sexp_grammar, sexp]

    let sexp_of_t { b; _ } = Sexp.List [ List [ Atom "b"; Atom b ] ]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok ((b "")))

    ==============
    <div>
      <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" onclick> [default] </div>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea>
    </div> |}];
  Handle.do_actions handle [ { a = 1; b = "foo" } ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((b foo)))

    ==============
    <div>
      <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" onclick> [default] </div>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=foo oninput> </textarea>
    </div> |}]
;;

let%expect_test "setting into an optional field with the value" =
  let module M = struct
    type t =
      { a : int [@default 0]
      ; b : string
      }
    [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 0)
      (b "")))

    ==============
    <div>
      <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" onclick> [default] </div>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea>
    </div> |}];
  Handle.do_actions handle [ { a = 1; b = "foo" } ];
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 1)
      (b foo)))

    ==============
    <div>
      <div class="override_showing_hash_replaced_in_test override_text_hash_replaced_in_test" onclick> [override] </div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=1
             oninput> </input>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=foo oninput> </textarea>
    </div> |}]
;;

let%expect_test "regression test: optional groups have keys attached" =
  (* This test verifies that checkbox inputs for optional fields have a key attached to
     them when they are clicked. This prevents a bug in the vdom library's patching code
     that can trigger if there are consecutive inputs without keys. *)
  let module M = struct
    type record =
      { one : string
      ; two : string option
      ; three : string option
      }
    [@@deriving sexp, sexp_grammar]

    type t = { record : record } [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:Auto_generated.view_as_vdom (module M) in
  Handle.store_view handle;
  Handle.set_checkbox
    handle
    ~get_vdom:get_vdom_detailed
    ~selector:"tr:nth-child(4) input"
    ~checked:true;
  Handle.show_diff handle;
  (* Observe that when the checkbox is checked, there is still a key on the [td]
     surrounding it. *)
  [%expect
    {|
    -|(Ok ((
    -|  record (
    -|    (one "")
    -|    (two   ())
    -|    (three ())))))
    +|(Ok ((record ((one "") (two ()) (three (""))))))

      ==============
      <table class="form_hash_replaced_in_test">
        <tbody>
          <tr>
            <td>
              <div colspan="2" class="container_hash_replaced_in_test label_hash_replaced_in_test"> record </div>
            </td>
          </tr>
          <tr>
            <td colspan="100">
              <table class="mod_depth_2_hash_replaced_in_test nested_table_hash_replaced_in_test">
                <tr @key=bonsai_path_replaced_in_test>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test">
                      <label for="bonsai_path_replaced_in_test" style={ display: block; }> one </label>

                             margin-left: 0px;
                           }> </input>
                  </td>
                </tr>
                <tr>
                  <td colspan="100">
                    <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test"> </table>
                  </td>
                </tr>
                <tr>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test"> three </div>
                  </td>
                  <td>
                    <input type="checkbox"
                           id="bonsai_path_replaced_in_test"
    -|                     #checked="false"
    +|                     #checked="true"
                           onclick
                           style={
                             margin-left: 0px;
                           }> </input>
                  </td>
                </tr>
                <tr>
                  <td colspan="100">
    -|              <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test"> </table>
    +|              <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test">
    +|                <tr @key=bonsai_path_replaced_in_test>
    +|                  <td>
    +|                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test">  </div>
    +|                  </td>
    +|                  <td>
    +|                    <textarea placeholder=""
    +|                              id="bonsai_path_replaced_in_test"
    +|                              value:normalized=""
    +|                              oninput> </textarea>
    +|                  </td>
    +|                </tr>
    +|              </table>
                  </td>
                </tr>
              </table>
            </td>
          </tr>
        </tbody>
      </table> |}];
  Handle.set_checkbox
    handle
    ~get_vdom:get_vdom_detailed
    ~selector:"tr:nth-child(3) input"
    ~checked:true;
  Handle.show_diff handle;
  (* Observe that when the checkbox is checked, there is still a key on the [td]
     surrounding it. *)
  [%expect
    {|
    -|(Ok ((record ((one "") (two ()) (three (""))))))
    +|(Ok ((
    +|  record (
    +|    (one "")
    +|    (two   (""))
    +|    (three (""))))))

      ==============
      <table class="form_hash_replaced_in_test">
        <tbody>
          <tr>
            <td>
              <div colspan="2" class="container_hash_replaced_in_test label_hash_replaced_in_test"> record </div>
            </td>
          </tr>
          <tr>
            <td colspan="100">
              <table class="mod_depth_2_hash_replaced_in_test nested_table_hash_replaced_in_test">
                <tr @key=bonsai_path_replaced_in_test>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test">
                      <label for="bonsai_path_replaced_in_test" style={ display: block; }> one </label>
                    </div>
                  </td>
                  <td>
                    <textarea placeholder=""
                              id="bonsai_path_replaced_in_test"
                              value:normalized=""
                              oninput> </textarea>
                  </td>
                </tr>
                <tr>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test"> two </div>
                  </td>
                  <td>
                    <input type="checkbox"
                           id="bonsai_path_replaced_in_test"
    -|                     #checked="false"
    +|                     #checked="true"
                           onclick
                           style={
                             margin-left: 0px;
                           }> </input>
                  </td>
                </tr>
                <tr>
                  <td colspan="100">
    -|              <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test"> </table>
    +|              <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test">
    +|                <tr @key=bonsai_path_replaced_in_test>
    +|                  <td>
    +|                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test">  </div>
    +|                  </td>
    +|                  <td>
    +|                    <textarea placeholder=""
    +|                              id="bonsai_path_replaced_in_test"
    +|                              value:normalized=""
    +|                              oninput> </textarea>
    +|                  </td>
    +|                </tr>
    +|              </table>
                  </td>
                </tr>
                <tr>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test"> three </div>
                  </td>
                  <td>
                    <input type="checkbox"
                           id="bonsai_path_replaced_in_test"
                           #checked="true"
                           onclick
                           style={
                             margin-left: 0px;
                           }> </input>
                  </td>
                </tr> |}]
;;

let%expect_test "dynamic sexp_grammar form is rendered" =
  let grammar = [%sexp_grammar: int] in
  let grammar' = [%sexp_grammar: bool] in
  let grammar_var = Bonsai.Var.create grammar.untyped in
  let grammar_value = Bonsai.Var.value grammar_var in
  let form = Auto_generated.form' grammar_value in
  let handle = Handle.create (form_result_spec Fn.id) form in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           oninput> </input> |}];
  Bonsai.Var.set grammar_var grammar'.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok false)

    ==============
    <input type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           onclick
           style={
             margin-left: 0px;
           }> </input> |}]
;;

let%expect_test "interactions with dynamic sexp_grammar form" =
  let grammar = [%sexp_grammar: int] in
  let grammar' = [%sexp_grammar: bool] in
  let grammar_var = Bonsai.Var.create grammar.untyped in
  let grammar_value = Bonsai.Var.value grammar_var in
  let form = Auto_generated.form' grammar_value in
  let handle = Handle.create (form_result_spec Fn.id) form in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok 2)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=2
           oninput> </input> |}];
  Bonsai.Var.set grammar_var grammar'.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok false)

    ==============
    <input type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           onclick
           style={
             margin-left: 0px;
           }> </input> |}];
  Handle.set_checkbox handle ~get_vdom ~selector:"input" ~checked:true;
  Handle.show handle;
  [%expect
    {|
    (Ok true)

    ==============
    <input type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="true"
           onclick
           style={
             margin-left: 0px;
           }> </input> |}];
  (* We remember the value of the form before the grammar was changed *)
  Bonsai.Var.set grammar_var grammar.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok 2)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=2
           oninput> </input> |}]
;;

let%expect_test "setting into a dynamic grammar form works, but should be done with care" =
  let grammar = [%sexp_grammar: int] in
  let grammar' = [%sexp_grammar: bool] in
  let grammar_var = Bonsai.Var.create grammar.untyped in
  let grammar_value = Bonsai.Var.value grammar_var in
  let form = Auto_generated.form' grammar_value in
  let handle = Handle.create (form_result_spec Fn.id) form in
  (* Everything is good as long as you set sexps that match the current grammar *)
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           oninput> </input> |}];
  Handle.do_actions handle [ [%sexp_of: int] 5 ];
  Handle.show handle;
  [%expect
    {|
    (Ok 5)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=5
           oninput> </input> |}];
  Bonsai.Var.set grammar_var grammar'.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok false)

    ==============
    <input type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           onclick
           style={
             margin-left: 0px;
           }> </input> |}];
  Handle.do_actions handle [ [%sexp_of: bool] true ];
  Handle.show handle;
  [%expect
    {|
    (Ok true)

    ==============
    <input type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="true"
           onclick
           style={
             margin-left: 0px;
           }> </input> |}];
  (* But if you set something that doesn't match the current grammar, [on_set_error] will
     be called *)
  Handle.do_actions handle [ [%sexp_of: int] 4 ];
  [%expect
    {|
      ("BUG: Sexp representation of set form value does not match sexp grammar. Does your sexp_of_t function match your sexp grammar?"
       (value 4)
       (error
        ("invalid bool"
         (reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 4)))))) |}]
;;

let%expect_test "customizations are applied dynamically" =
  let grammar = [%sexp_grammar: int] in
  let grammar' =
    Sexplib0.Sexp_grammar.tag [%sexp_grammar: string] ~key:"key" ~value:(Sexp.List [])
  in
  let grammar_var = Bonsai.Var.create grammar.untyped in
  let grammar_value = Bonsai.Var.value grammar_var in
  let customization =
    Auto_generated.Customization.constant_form
      (module String)
      ~apply_to_tag:(fun ~key ~value:_ -> String.equal key "key")
      (Form.Elements.Textarea.string ())
  in
  let form = Auto_generated.form' ~customizations:[ customization ] grammar_value in
  let handle = Handle.create (form_result_spec Fn.id) form in
  (* No customizations are applied to the original grammar *)
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           oninput> </input> |}];
  (* When the grammar transitions to a grammar which customizations are defined for, those
     customizations are applied *)
  Bonsai.Var.set grammar_var grammar'.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" oninput> </textarea> |}];
  (* And transitioning back still does not apply customizations *)
  Bonsai.Var.set grammar_var grammar.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           oninput> </input> |}]
;;

let%expect_test "customizing a list to have better button text" =
  let module M = struct
    type t = (int list[@tag "grammar.add_element_text" = Atom "add new integer"])
    [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <button type="button" onclick> add new integer </button>
          </td>
        </tr>
      </tbody>
    </table> |}];
  Handle.do_actions handle [ [ 1 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok (1))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div>
              0 -
              <button type="button"
                      onclick
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   oninput> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" onclick> add new integer </button>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "customizing a list in a record to have better button text" =
  let module M = struct
    type t = { a : (int list[@tag "grammar.add_element_text" = Atom "add new integer"]) }
    [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok ((a ())))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <label style={ display: block; }> a </label>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" onclick> add new integer </button>
          </td>
        </tr>
      </tbody>
    </table> |}];
  Handle.do_actions handle [ { a = [ 1 ] } ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((a (1))))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <label style={ display: block; }> a </label>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <div>
              0 -
              <button type="button"
                      onclick
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 2em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   oninput> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 2em; font-weight: bold; }>
            <button type="button" onclick> add new integer </button>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "model state is not shared between variants even when they have \
                 identical arguments"
  =
  let module T = struct
    type t =
      | A of int
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.recompute_view handle;
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok (A 2))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> a </option>
        <option value="2" #selected="false"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=2
             oninput> </input>
    </div> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok (B 0))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=0
             oninput> </input>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"3";
  Handle.show handle;
  [%expect
    {|
    (Ok (B 3))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=3
             oninput> </input>
    </div> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.show handle;
  [%expect
    {|
    (Ok (A 2))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> a </option>
        <option value="2" #selected="false"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=2
             oninput> </input>
    </div> |}]
;;

let%test_module "Stabilization tests" =
  (module struct
    let filter_don't_stabilize output =
      String.split_lines output
      |> List.filter ~f:(fun line -> String.equal line "stabilized")
      |> String.concat ~sep:"\n"
      |> print_endline
    ;;

    let%expect_test "A simple record doesn't stabilize" =
      let module T = struct
        type t =
          { a : int
          ; b : string
          ; c : bool
          }
        [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ { T.a = 10; b = "hello world"; c = true } ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {| |}]
    ;;

    let%expect_test "A list stabilizes once" =
      let module T = struct
        type t = int list [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ [ 1; 2; 3; 4 ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {|
        stabilized |}]
    ;;

    let%expect_test "A variant stabilizes once" =
      let module T = struct
        type t =
          | A of int
          | B of bool
        [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ B false ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {|
        stabilized |}]
    ;;

    let%expect_test "Nested lists and variants stabilize linearly with the depth of the \
                     type (depth 2)"
      =
      let module T = struct
        type s =
          | A of int
          | B of bool

        and t = s list [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ [ A 1; B false; B true; A 4 ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {|
        stabilized
        stabilized |}]
    ;;

    let%expect_test "Nested lists and variants stabilize linearly with the depth of the \
                     type (depth 3)"
      =
      let module T = struct
        type r =
          | A of int
          | B of bool

        and s = r list

        and t =
          | C of s
          | D of float
        [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ C [ A 1; B false; B true; A 4 ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {|
        stabilized
        stabilized
        stabilized |}]
    ;;

    let%expect_test "Nested lists and variants stabilize linearly with the depth of the \
                     type (depth 4)"
      =
      let module T = struct
        type q =
          | A of int
          | B of bool

        and r = q list

        and s =
          | C of r
          | D of float

        and t = s list [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions
        handle
        [ [ C [ A 1; B false; B true; A 4 ]; D 1.; C [ B true; B false; A 1; A 10 ] ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect
        {|
        stabilized
        stabilized
        stabilized
        stabilized |}]
    ;;

    let%expect_test "maps!" =
      let module S = struct
        module T = struct
          type t =
            | None
            | Some of t
          [@@deriving compare, sexp, sexp_grammar]
        end

        include T
        include Comparable.Make (T)
      end
      in
      let module T = struct
        type s = S.t Map.M(S).t list
        and t = s list [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      let m =
        S.Map.of_alist_exn
          S.[ None, None; Some None, Some None; Some (Some None), Some (Some None) ]
      in
      Handle.do_actions handle [ [ [ m; m ]; [ m; m ] ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect
        {|
        stabilized
        stabilized
        stabilized
        stabilized
        stabilized |}]
    ;;

    let%expect_test "records with defaults!" =
      let module T = struct
        type s =
          | My of int
          | Your of string
          | Our of bool

        and r =
          { b : s [@sexp.default My 10]
          ; a : bool [@sexp.default false]
          ; c : int [@sexp.default 0]
          ; aa : int [@sexp.default 3]
          }

        and t = r [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ { c = 1; aa = 4; b = Your "hi"; a = true } ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {|
        stabilized
        stabilized |}]
    ;;
  end)
;;
