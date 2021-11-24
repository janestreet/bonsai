open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

let get_vdom form =
  match Form.View.to_vdom_plain (Form.view form) with
  | [ v ] -> v
  | other -> Vdom.Node.div other
;;

let get_vdom_verbose ?on_submit ?editable f = Form.view_as_vdom ?on_submit ?editable f

let form_result_spec (type a) ?filter_printed_attributes ?(get_vdom = get_vdom) sexp_of_a
  : (module Result_spec.S with type t = a Form.t and type incoming = a)
  =
  (module struct
    type t = a Form.t
    type incoming = a

    let view form =
      let module V = (val Result_spec.vdom ?filter_printed_attributes Fn.id) in
      let vdom = get_vdom form in
      let vdom = V.view vdom in
      let value =
        Form.value form
        |> [%sexp_of: a Or_error.t]
        |> Expect_test_helpers_base.sexp_to_string
      in
      sprintf "%s\n==============\n%s\n" value vdom
    ;;

    let incoming = Form.set
  end)
;;

let verbose_form_result_spec (type a) sexp_of_a
  : (module Result_spec.S with type t = a Form.t and type incoming = a)
  =
  (module struct
    type t = a Form.t
    type incoming = a

    let view form =
      let module V = (val Result_spec.vdom Fn.id) in
      let vdom = Form.View.to_vdom (Form.view form) in
      let vdom = V.view vdom in
      let value =
        Form.value form
        |> [%sexp_of: a Or_error.t]
        |> Expect_test_helpers_base.sexp_to_string
      in
      sprintf "%s\n==============\n%s\n" value vdom
    ;;

    let incoming = Form.set
  end)
;;

let%expect_test "typing into a string textbox" =
  let component = Form.Elements.Textbox.string [%here] in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             oninput> </input> |}]
;;

let%expect_test "dropdown starting empty" =
  let component =
    Form.Elements.Dropdown.list
      [%here]
      (module String)
      (Value.return [ "hello"; "world" ])
      ~init:`Empty
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="false"> world </option>
    </select> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok hello)

      ==============
      <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
    -|  <option value="0" #selected="true">  </option>
    +|  <option value="0" #selected="false">  </option>
    -|  <option value="1" #selected="false"> hello </option>
    +|  <option value="1" #selected="true"> hello </option>
        <option value="2" #selected="false"> world </option>
      </select> |}]
;;

let%expect_test "dropdown with default value" =
  let component =
    Form.Elements.Dropdown.list
      [%here]
      (module String)
      (Value.return [ "hello"; "world" ])
      ~init:(`This (Value.return "world"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok world)

    ==============
    <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="false"> hello </option>
      <option value="1" #selected="true"> world </option>
    </select> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"0";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok world)
    +|(Ok hello)

      ==============
      <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
    -|  <option value="0" #selected="false"> hello </option>
    +|  <option value="0" #selected="true"> hello </option>
    -|  <option value="1" #selected="true"> world </option>
    +|  <option value="1" #selected="false"> world </option>
      </select> |}]
;;

let%expect_test "dropdown_opt with default value" =
  let component =
    Form.Elements.Dropdown.list_opt
      [%here]
      (module String)
      (Value.return [ "hello"; "world" ])
      ~init:(`This (Value.return "world"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string option]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok (world))

    ==============
    <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="true"> world </option>
    </select> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"0";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (world))
    +|(Ok ())

      ==============
      <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
    -|  <option value="0" #selected="false">  </option>
    +|  <option value="0" #selected="true">  </option>
        <option value="1" #selected="false"> hello </option>
    -|  <option value="2" #selected="true"> world </option>
    +|  <option value="2" #selected="false"> world </option>
      </select> |}]
;;

let%expect_test "dropdown" =
  let component =
    Form.Elements.Dropdown.list
      [%here]
      (module String)
      (Value.return [ "hello"; "world" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok hello)

    ==============
    <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true"> hello </option>
      <option value="1" #selected="false"> world </option>
    </select> |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok hello)
    +|(Ok world)

      ==============
      <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
    -|  <option value="0" #selected="true"> hello </option>
    +|  <option value="0" #selected="false"> hello </option>
    -|  <option value="1" #selected="false"> world </option>
    +|  <option value="1" #selected="true"> world </option>
      </select> |}]
;;

let%expect_test "dropdown but without any elements to pick from " =
  let component = Form.Elements.Dropdown.list [%here] (module String) (Value.return []) in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true">  </option>
    </select> |}]
;;

let%expect_test "collapsible group" =
  let component =
    let%sub textbox = Form.Elements.Textbox.string [%here] in
    Form.Dynamic.collapsible_group (Value.return "group-name") textbox
  in
  let get_vdom = get_vdom_verbose in
  let handle = Handle.create (form_result_spec ~get_vdom [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div onclick style={ user-select: none; cursor: pointer; }> ▾ group-name </div>
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
            <input type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
          </td>
        </tr>
      </tbody>
    </table> |}];
  Handle.click_on ~get_vdom ~selector:"tr:nth-child(1) div" handle;
  Handle.show_diff handle;
  [%expect
    {|
      (Ok "")

      ==============
      <table>
        <tbody>
          <tr>
            <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
    -|        <div onclick style={ user-select: none; cursor: pointer; }> ▾ group-name </div>
    -|      </td>
    -|    </tr>
    -|    <tr @key=bonsai_path_replaced_in_test>
    -|      <td style={
    -|            padding-left: 1em;
    -|            padding-right: 1em;
    -|            text-align: left;
    -|            font-weight: bold;
    -|            user-select: none;
    -|          }>  </td>
    -|      <td>
    -|        <input type="text"
    -|               placeholder=""
    -|               spellcheck="false"
    -|               id="bonsai_path_replaced_in_test"
    -|               value:normalized=""
    -|               oninput> </input>
    -|      </td>
    -|      <td>
    -|        <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
    +|        <div onclick style={ user-select: none; cursor: pointer; }> ► group-name </div>
            </td>
          </tr>
        </tbody>
      </table> |}]
;;

let%expect_test "setting into a string textbox" =
  let component = Form.Elements.Textbox.string [%here] in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input> |}];
  Handle.do_actions handle [ "hello world" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             oninput> </input> |}]
;;

let%expect_test "typing into a int textbox" =
  let component = Form.Elements.Textbox.int [%here] in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expecting an integer")

    ==============
    <input type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expecting an integer")
    +|(Ok 123)

      ==============
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized=123
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 123)
    +|(Error "Expecting an integer")

      ==============
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=123
    +|       value:normalized="hello world"
             oninput> </input> |}]
;;

let%expect_test "setting into a int textbox" =
  let component = Form.Elements.Textbox.int [%here] in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expecting an integer")

    ==============
    <input type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input> |}];
  Handle.do_actions handle [ 123 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expecting an integer")
    +|(Ok 123)

      ==============
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized=123
             oninput> </input> |}]
;;

let%expect_test "typing into a paired string textbox * int textbox " =
  let component =
    let%sub string_form = Form.Elements.Textbox.string [%here] in
    let%sub int_form = Form.Elements.Textbox.int [%here] in
    return
    @@ let%map string_form = string_form
    and int_form = int_form in
    Form.both string_form int_form
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string * int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expecting an integer")

    ==============
    <div>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(1)" ~text:"hello world";
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(2)" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expecting an integer")
    +|(Ok ("hello world" 123))

      ==============
      <div>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized="hello world"
               oninput> </input>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized=123
               oninput> </input>
      </div> |}]
;;

let%expect_test "setting into a paired string textbox * int textbox " =
  let component =
    let%sub string_form = Form.Elements.Textbox.string [%here] in
    let%sub int_form = Form.Elements.Textbox.int [%here] in
    return
    @@ let%map string_form = string_form
    and int_form = int_form in
    Form.both string_form int_form
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string * int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expecting an integer")

    ==============
    <div>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div> |}];
  Handle.do_actions handle [ "hello world", 123 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expecting an integer")
    +|(Ok ("hello world" 123))

      ==============
      <div>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized="hello world"
               oninput> </input>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized=123
               oninput> </input>
      </div> |}]
;;

let%expect_test "adding more things to a string list (indented button)" =
  let component =
    Form.Elements.Multiple.list
      [%here]
      ~button_placement:`Indented
      (Form.Elements.Textbox.string [%here])
  in
  let handle =
    Handle.create
      (form_result_spec ~get_vdom:get_vdom_verbose [%sexp_of: string list])
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <button onclick> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table> |}];
  Handle.click_on handle ~get_vdom:get_vdom_verbose ~selector:"button";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (""))

      ==============
      <table>
        <tbody>
    +|    <tr>
    +|      <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
    +|        <div>
    +|          0 -
    +|          <button type="button"
    +|                  onclick
    +|                  style={
    +|                    border: none;
    +|                    cursor: pointer;
    +|                    color: blue;
    +|                    background: none;
    +|                  }> [ remove ] </button>
    +|        </div>
    +|      </td>
    +|    </tr>
    +|    <tr @key=bonsai_path_replaced_in_test>
    +|      <td style={
    +|            padding-left: 1em;
    +|            padding-right: 1em;
    +|            text-align: left;
    +|            font-weight: bold;
    +|            user-select: none;
    +|          }>  </td>
    +|      <td>
    +|        <input type="text"
    +|               placeholder=""
    +|               spellcheck="false"
    +|               id="bonsai_path_replaced_in_test"
    +|               value:normalized=""
    +|               oninput> </input>
    +|      </td>
    +|      <td>
    +|        <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
    +|      </td>
    +|    </tr>
          <tr>
            <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
              <button onclick> Add new element </button>
            </td>
          </tr>
        </tbody>
      </table> |}];
  Handle.input_text
    handle
    ~get_vdom:get_vdom_verbose
    ~selector:"input"
    ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (""))
    +|(Ok ("hello world"))

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
              <input type="text"
                     placeholder=""
                     spellcheck="false"
                     id="bonsai_path_replaced_in_test"
    -|               value:normalized=""
    +|               value:normalized="hello world"
                     oninput> </input>
            </td>
            <td>
              <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
            </td>
          </tr>
          <tr>
            <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
              <button onclick> Add new element </button>
            </td>
          </tr>
        </tbody>
      </table> |}]
;;

let%expect_test "adding more things to a string list" =
  let component =
    Form.Elements.Multiple.list
      [%here]
      ~button_placement:`Inline
      (Form.Elements.Textbox.string [%here])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string list]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <button onclick> Add new element </button> |}];
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (""))

      ==============
    +|<div>
    +|  <input type="text"
    +|         placeholder=""
    +|         spellcheck="false"
    +|         id="bonsai_path_replaced_in_test"
    +|         value:normalized=""
    +|         oninput> </input>
        <button onclick> Add new element </button>
    +|</div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (""))
    +|(Ok ("hello world"))

      ==============
      <div>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized="hello world"
               oninput> </input>
        <button onclick> Add new element </button>
      </div> |}]
;;

let%expect_test "using the same component twice" =
  let component =
    let%sub textbox = Form.Elements.Textbox.string [%here] in
    return @@ Bonsai.Value.map2 ~f:Form.both textbox textbox
  in
  let handle =
    Handle.create
      (form_result_spec ~get_vdom:get_vdom_verbose [%sexp_of: string * string])
      component
  in
  Handle.disable_bonsai_path_censoring handle;
  Handle.do_actions handle [ "a", "b" ];
  Handle.show handle;
  (* The real bug on display here is that two nodes have the same [key].
     This crashes the vdom library. *)
  [%expect
    {|
    (Ok (b b))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_x_x_x_x_x>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_x_x_x_x_x"
                   value:normalized=b
                   oninput> </input>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
          </td>
        </tr>
        <tr @key=bonsai_path_x_x_x_x_x>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_x_x_x_x_x"
                   value:normalized=b
                   oninput> </input>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "setting things to a string list" =
  let component =
    Form.Elements.Multiple.list
      [%here]
      ~button_placement:`Inline
      (Form.Elements.Textbox.string [%here])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string list]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <button onclick> Add new element </button> |}];
  Handle.do_actions handle [ [ "hello"; "there"; "world" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (hello there world))

      ==============
    +|<div>
    +|  <input type="text"
    +|         placeholder=""
    +|         spellcheck="false"
    +|         id="bonsai_path_replaced_in_test"
    +|         value:normalized=hello
    +|         oninput> </input>
    +|  <input type="text"
    +|         placeholder=""
    +|         spellcheck="false"
    +|         id="bonsai_path_replaced_in_test"
    +|         value:normalized=there
    +|         oninput> </input>
    +|  <input type="text"
    +|         placeholder=""
    +|         spellcheck="false"
    +|         id="bonsai_path_replaced_in_test"
    +|         value:normalized=world
    +|         oninput> </input>
        <button onclick> Add new element </button>
    +|</div> |}]
;;

let%expect_test "setting things to a string list (verbose)" =
  let component =
    Form.Elements.Multiple.list
      [%here]
      ~button_placement:`Inline
      (Form.Elements.Textbox.string [%here])
  in
  let handle =
    Handle.create (verbose_form_result_spec [%sexp_of: string list]) component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

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
              }>  </td>
          <td>
            <button onclick> Add new element </button>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
          </td>
        </tr>
      </tbody>
    </table> |}];
  Handle.do_actions handle [ [ "hello"; "there"; "world" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (hello there world))

      ==============
      <table>
        <tbody>
    +|    <tr>
    +|      <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
    +|        <div>
    +|          0 -
    +|          <button type="button"
    +|                  onclick
    +|                  style={
    +|                    border: none;
    +|                    cursor: pointer;
    +|                    color: blue;
    +|                    background: none;
    +|                  }> [ remove ] </button>
    +|        </div>
    +|      </td>
    +|    </tr>
    +|    <tr @key=bonsai_path_replaced_in_test>
    +|      <td style={
    +|            padding-left: 1em;
    +|            padding-right: 1em;
    +|            text-align: left;
    +|            font-weight: bold;
    +|            user-select: none;
    +|          }>  </td>
    +|      <td>
    +|        <input type="text"
    +|               placeholder=""
    +|               spellcheck="false"
    +|               id="bonsai_path_replaced_in_test"
    +|               value:normalized=hello
    +|               oninput> </input>
    +|      </td>
    +|      <td>
    +|        <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
    +|      </td>
    +|    </tr>
    +|    <tr>
    +|      <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
    +|        <div>
    +|          1 -
    +|          <button type="button"
    +|                  onclick
    +|                  style={
    +|                    border: none;
    +|                    cursor: pointer;
    +|                    color: blue;
    +|                    background: none;
    +|                  }> [ remove ] </button>
    +|        </div>
    +|      </td>
    +|    </tr>
    +|    <tr @key=bonsai_path_replaced_in_test>
    +|      <td style={
    +|            padding-left: 1em;
    +|            padding-right: 1em;
    +|            text-align: left;
    +|            font-weight: bold;
    +|            user-select: none;
    +|          }>  </td>
    +|      <td>
    +|        <input type="text"
    +|               placeholder=""
    +|               spellcheck="false"
    +|               id="bonsai_path_replaced_in_test"
    +|               value:normalized=there
    +|               oninput> </input>
    +|      </td>
    +|      <td>
    +|        <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
    +|      </td>
    +|    </tr>
    +|    <tr>
    +|      <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
    +|        <div>
    +|          2 -
    +|          <button type="button"
    +|                  onclick
    +|                  style={
    +|                    border: none;
    +|                    cursor: pointer;
    +|                    color: blue;
    +|                    background: none;
    +|                  }> [ remove ] </button>
    +|        </div>
    +|      </td>
    +|    </tr>
    +|    <tr @key=bonsai_path_replaced_in_test>
    +|      <td style={
    +|            padding-left: 1em;
    +|            padding-right: 1em;
    +|            text-align: left;
    +|            font-weight: bold;
    +|            user-select: none;
    +|          }>  </td>
    +|      <td>
    +|        <input type="text"
    +|               placeholder=""
    +|               spellcheck="false"
    +|               id="bonsai_path_replaced_in_test"
    +|               value:normalized=world
    +|               oninput> </input>
    +|      </td>
    +|      <td>
    +|        <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
    +|      </td>
    +|    </tr>
          <tr @key=bonsai_path_replaced_in_test>
            <td style={
                  padding-left: 0em;
                  padding-right: 1em;
                  text-align: left;
                  font-weight: bold;
                  user-select: none;
                }>  </td>
            <td>
              <button onclick> Add new element </button>
            </td>
            <td>
              <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
            </td>
          </tr>
        </tbody> |}]
;;

let%expect_test "typing into an int number element" =
  let component =
    Form.Elements.Number.int [%here] ~default:0 ~step:1 ~min:(-1) ~max:10 ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
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
           min="-1"
           max="10"
           value:normalized=0
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"10";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 0)
    +|(Ok 10)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=0
    +|       value:normalized=10
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"-1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10)
    +|(Ok -1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=10
    +|       value:normalized=-1
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"11";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 11) "higher than allowed threshold" (S.max (10))))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=-1
    +|       value:normalized=11
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"-2";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 11) "higher than allowed threshold" (S.max (10))))
    +|(Error ((value -2) "lower than allowed threshold" (S.min (-1))))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=11
    +|       value:normalized=-2
             oninput> </input> |}]
;;

let%expect_test "setting into an int number element" =
  let component =
    Form.Elements.Number.int [%here] ~default:0 ~step:1 ~min:(-1) ~max:10 ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
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
           min="-1"
           max="10"
           value:normalized=0
           oninput> </input> |}];
  Handle.do_actions handle [ 10 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 0)
    +|(Ok 10)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=0
    +|       value:normalized=10
             oninput> </input> |}];
  Handle.do_actions handle [ -1 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10)
    +|(Ok -1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=10
    +|       value:normalized=-1
             oninput> </input> |}];
  Handle.do_actions handle [ 11 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 11) "higher than allowed threshold" (S.max (10))))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=-1
    +|       value:normalized=11
             oninput> </input> |}];
  Handle.do_actions handle [ -2 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 11) "higher than allowed threshold" (S.max (10))))
    +|(Error ((value -2) "lower than allowed threshold" (S.min (-1))))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=11
    +|       value:normalized=-2
             oninput> </input> |}]
;;

let%expect_test "typing into a float number element" =
  let component =
    Form.Elements.Number.float [%here] ~default:0. ~step:1. ~min:(-1.) ~max:10.1 ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: float]) component in
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
           min="-1"
           max="10.1"
           value:normalized=0
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"10.1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 0)
    +|(Ok 10.1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10.1"
    -|       value:normalized=0
    +|       value:normalized=10.1
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"-1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10.1)
    +|(Ok -1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10.1"
    -|       value:normalized=10.1
    +|       value:normalized=-1
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"10.2";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 10.2) "higher than allowed threshold" (S.max (10.1))))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10.1"
    -|       value:normalized=-1
    +|       value:normalized=10.2
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"-1.1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 10.2) "higher than allowed threshold" (S.max (10.1))))
    +|(Error ((value -1.1) "lower than allowed threshold" (S.min (-1))))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10.1"
    -|       value:normalized=10.2
    +|       value:normalized=-1.1
             oninput> </input> |}]
;;

let%expect_test "setting into an int number element" =
  let component =
    Form.Elements.Number.float [%here] ~default:0. ~step:1. ~min:(-1.) ~max:10.1 ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: float]) component in
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
           min="-1"
           max="10.1"
           value:normalized=0
           oninput> </input> |}];
  Handle.do_actions handle [ 10.1 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 0)
    +|(Ok 10.1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10.1"
    -|       value:normalized=0
    +|       value:normalized=10.1
             oninput> </input> |}];
  Handle.do_actions handle [ -1. ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10.1)
    +|(Ok -1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10.1"
    -|       value:normalized=10.1
    +|       value:normalized=-1
             oninput> </input> |}];
  Handle.do_actions handle [ 10.2 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 10.2) "higher than allowed threshold" (S.max (10.1))))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10.1"
    -|       value:normalized=-1
    +|       value:normalized=10.2
             oninput> </input> |}];
  Handle.do_actions handle [ -1.1 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 10.2) "higher than allowed threshold" (S.max (10.1))))
    +|(Error ((value -1.1) "lower than allowed threshold" (S.min (-1))))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10.1"
    -|       value:normalized=10.2
    +|       value:normalized=-1.1
             oninput> </input> |}]
;;

let%expect_test "clicking on radio buttons" =
  let component =
    Form.Elements.Radio_buttons.list
      [%here]
      ~to_string:Fn.id
      (module String)
      ~layout:`Vertical
      (Value.return [ "first"; "second"; "third" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <ul id="bonsai_path_replaced_in_test"
        class="radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: block; }>
        <label>
          <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
          third
        </label>
      </li>
    </ul> |}];
  Handle.click_on handle ~get_vdom ~selector:"label:nth-child(1) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok first)

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
    -|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
    +|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="true" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul> |}];
  Handle.click_on handle ~get_vdom ~selector:"li:nth-child(2) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok first)
    +|(Ok second)

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
    -|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="true" onclick> </input>
    +|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    -|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
    +|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul> |}]
;;

let%expect_test "setting into radio buttons" =
  let component =
    Form.Elements.Radio_buttons.list
      [%here]
      ~to_string:Fn.id
      (module String)
      ~layout:`Vertical
      (Value.return [ "first"; "second"; "third" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <ul id="bonsai_path_replaced_in_test"
        class="radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: block; }>
        <label>
          <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
          third
        </label>
      </li>
    </ul> |}];
  Handle.do_actions handle [ "first" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok first)

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
    -|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
    +|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="true" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul> |}];
  Handle.do_actions handle [ "second" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok first)
    +|(Ok second)

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
    -|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="true" onclick> </input>
    +|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    -|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
    +|      <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul> |}]
;;

let%expect_test "horizontal radio buttons render with correct styles applied" =
  let component =
    Form.Elements.Radio_buttons.list
      [%here]
      ~to_string:Fn.id
      (module String)
      ~layout:`Horizontal
      (Value.return [ "first" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <ul id="bonsai_path_replaced_in_test"
        class="radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: inline-block; }>
        <label>
          <input type="radio" name="bonsai_path_replaced_in_test" class="radio-button" #checked="false" onclick> </input>
          first
        </label>
      </li>
    </ul> |}]
;;

let%expect_test "clicking a set checklist" =
  let component =
    Form.Elements.Checkbox.set
      [%here]
      ~to_string:Fn.id
      (module String)
      (Value.return [ "first"; "second"; "third" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Set.M(String).t]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <ul id="bonsai_path_replaced_in_test"
        class="checkbox-container widget-checklist"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: block; }>
        <label id="bonsai_path_replaced_in_test">
          <input type="checkbox" #checked="false" onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label id="bonsai_path_replaced_in_test">
          <input type="checkbox" #checked="false" onclick> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label id="bonsai_path_replaced_in_test">
          <input type="checkbox" #checked="false" onclick> </input>
          third
        </label>
      </li>
    </ul> |}];
  Handle.click_on handle ~get_vdom ~selector:"li:nth-child(1) input";
  Handle.click_on handle ~get_vdom ~selector:"li:nth-child(2) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (first second))

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="checkbox-container widget-checklist"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
    -|      <input type="checkbox" #checked="false" onclick> </input>
    +|      <input type="checkbox" #checked="true" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
    -|      <input type="checkbox" #checked="false" onclick> </input>
    +|      <input type="checkbox" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
            <input type="checkbox" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul> |}];
  Handle.click_on handle ~get_vdom ~selector:"li:nth-child(1) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (first second))
    +|(Ok (second))

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="checkbox-container widget-checklist"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
    -|      <input type="checkbox" #checked="true" onclick> </input>
    +|      <input type="checkbox" #checked="false" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
            <input type="checkbox" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
            <input type="checkbox" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul> |}]
;;

let%expect_test "setting into a set checklist" =
  let component =
    Form.Elements.Checkbox.set
      [%here]
      ~to_string:Fn.id
      (module String)
      (Value.return [ "first"; "second"; "third" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Set.M(String).t]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <ul id="bonsai_path_replaced_in_test"
        class="checkbox-container widget-checklist"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: block; }>
        <label id="bonsai_path_replaced_in_test">
          <input type="checkbox" #checked="false" onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label id="bonsai_path_replaced_in_test">
          <input type="checkbox" #checked="false" onclick> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label id="bonsai_path_replaced_in_test">
          <input type="checkbox" #checked="false" onclick> </input>
          third
        </label>
      </li>
    </ul> |}];
  Handle.do_actions handle [ Set.of_list (module String) [ "first"; "second" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (first second))

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="checkbox-container widget-checklist"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
    -|      <input type="checkbox" #checked="false" onclick> </input>
    +|      <input type="checkbox" #checked="true" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
    -|      <input type="checkbox" #checked="false" onclick> </input>
    +|      <input type="checkbox" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
            <input type="checkbox" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul> |}];
  Handle.do_actions handle [ Set.of_list (module String) [ "second" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (first second))
    +|(Ok (second))

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="checkbox-container widget-checklist"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
    -|      <input type="checkbox" #checked="true" onclick> </input>
    +|      <input type="checkbox" #checked="false" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
            <input type="checkbox" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label id="bonsai_path_replaced_in_test">
            <input type="checkbox" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul> |}]
;;

let%expect_test "forms using the Header_group view format/indent properly" =
  (* we use our own get_vdom for this one, because we care about the labels and
     padding *)
  let get_vdom form = form |> Form.view |> Form.View.to_vdom in
  let component =
    let%sub string1 = Form.Elements.Textbox.string [%here] in
    let%sub string2 = Form.Elements.Textbox.string [%here] in
    let%sub string3 = Form.Elements.Textbox.string [%here] in
    return
    @@ let%map string1 = string1
    and string2 = string2
    and string3 = string3 in
    let string3 =
      Form.tooltip "innermost form" (Form.label "innermost form" string3)
    in
    let value =
      let open Or_error.Let_syntax in
      let%bind string1 = Form.value string1 in
      let%bind string2 = Form.value string2 in
      let%bind string3 = Form.value string3 in
      return (string1, string2, string3)
    in
    let inner_view =
      Form.View.Private.Header_group
        { label = Some (Vdom.Node.text "middle form")
        ; tooltip = Some (Vdom.Node.text "middle form")
        ; header_view = Form.view string2
        ; view = Form.view string3
        }
    in
    let view =
      Form.View.Private.Header_group
        { label = Some (Vdom.Node.text "outermost form")
        ; tooltip = Some (Vdom.Node.text "outermost form")
        ; header_view = Form.view string1
        ; view = inner_view
        }
    in
    let set (s1, s2, s3) =
      Ui_effect.Many [ Form.set string1 s1; Form.set string2 s2; Form.set string3 s3 ]
    in
    Form.Expert.create ~value ~view ~set
  in
  let handle =
    Handle.create
      (form_result_spec ~get_vdom [%sexp_of: string * string * string])
      component
  in
  Handle.show handle;
  [%expect
    {|
     (Ok ("" "" ""))

     ==============
     <table>
       <tbody>
         <tr>
           <td style={ padding-left: 0em; font-weight: bold; }> outermost form </td>
           <td>
             <input type="text"
                    placeholder=""
                    spellcheck="false"
                    id="bonsai_path_replaced_in_test"
                    value:normalized=""
                    oninput> </input>
           </td>
           <td>
             <div class="container_hash_replaced_in_test">
               <label class="label_hash_replaced_in_test">
                 <input type="checkbox" class="checkbox_hash_replaced_in_test"> </input>
                 <span class="span_hash_replaced_in_test"> ⓘ </span>
                 <div class="text_hash_replaced_in_test"> outermost form </div>
               </label>
             </div>
           </td>
         </tr>
         <tr>
           <td style={ padding-left: 1em; font-weight: bold; }> middle form </td>
           <td>
             <input type="text"
                    placeholder=""
                    spellcheck="false"
                    id="bonsai_path_replaced_in_test"
                    value:normalized=""
                    oninput> </input>
           </td>
           <td>
             <div class="container_hash_replaced_in_test">
               <label class="label_hash_replaced_in_test">
                 <input type="checkbox" class="checkbox_hash_replaced_in_test"> </input>
                 <span class="span_hash_replaced_in_test"> ⓘ </span>
                 <div class="text_hash_replaced_in_test"> middle form </div>
               </label>
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
               }>
             <label for="bonsai_path_replaced_in_test" style={ display: block; }> innermost form </label>
           </td>
           <td>
             <input type="text"
                    placeholder=""
                    spellcheck="false"
                    id="bonsai_path_replaced_in_test"
                    value:normalized=""
                    oninput> </input>
           </td>
           <td>
             <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
               <div class="container_hash_replaced_in_test">
                 <label class="label_hash_replaced_in_test">
                   <input type="checkbox" class="checkbox_hash_replaced_in_test"> </input>
                   <span class="span_hash_replaced_in_test"> ⓘ </span>
                   <div class="text_hash_replaced_in_test"> innermost form </div>
                 </label>
               </div>

             </div>
           </td>
         </tr>
       </tbody>
     </table> |}]
;;

let%expect_test "form of nested record of int and float" =
  let module T = struct
    module Nested = struct
      type t =
        { age : int
        ; height : float
        }
      [@@deriving fields, sexp]

      let form =
        let%sub age = Form.Elements.Textbox.int [%here] in
        let%sub height = Form.Elements.Textbox.float [%here] in
        Form.Dynamic.Record_builder.(
          build_for_record (Fields.make_creator ~age:(field age) ~height:(field height)))
      ;;
    end

    type t =
      { unit : unit
      ; nested : Nested.t
      }
    [@@deriving fields, sexp]

    let form =
      let unit = Form.return () |> Value.return in
      let%sub nested = Nested.form in
      Form.Dynamic.Record_builder.(
        build_for_record (Fields.make_creator ~unit:(field unit) ~nested:(field nested)))
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Error (
      "in field nested"
      ("in field age" "Expecting an integer")
      ("in field height" (Invalid_argument "Float.of_string "))))

    ==============
    <div>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(1)" ~text:"123";
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(2)" ~text:"123.456";
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (unit ())
      (nested (
        (age    123)
        (height 123.456)))))

    ==============
    <div>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=123
             oninput> </input>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=123.456
             oninput> </input>
    </div> |}]
;;

let%expect_test "form of nested record of int and float (typed fields)" =
  let module T = struct
    module Nested = struct
      type t =
        { age : int
        ; height : float
        }
      [@@deriving typed_fields, sexp]

      let form =
        Form.Typed.Record.make
          (module struct
            module Typed_field = Typed_field

            let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
              = function
                | Age -> Form.Elements.Textbox.int [%here]
                | Height -> Form.Elements.Textbox.float [%here]
            ;;
          end)
      ;;
    end

    type t =
      { unit : unit
      ; nested : Nested.t
      }
    [@@deriving typed_fields, sexp]

    let form =
      Form.Typed.Record.make
        (module struct
          module Typed_field = Typed_field

          let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
            = function
              | Unit -> Bonsai.const (Form.return ())
              | Nested -> Nested.form
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Error (
      "in field nested"
      ("in field age" "Expecting an integer")
      ("in field height" (Invalid_argument "Float.of_string "))))

    ==============
    <div>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(1)" ~text:"123";
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(2)" ~text:"123.456";
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (unit ())
      (nested (
        (age    123)
        (height 123.456)))))

    ==============
    <div>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=123
             oninput> </input>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=123.456
             oninput> </input>
    </div> |}];
  Handle.do_actions
    handle
    [ { T.unit = (); nested = { T.Nested.age = 20; height = 1.23 } } ];
  Handle.show_diff handle;
  [%expect
    {|
      (Ok (
        (unit ())
        (nested (
    -|    (age    123)
    +|    (age    20)
    -|    (height 123.456)))))
    +|    (height 1.23)))))

      ==============
      <div>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=123
    +|         value:normalized=20
               oninput> </input>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=123.456
    +|         value:normalized=1.230
               oninput> </input>
      </div> |}]
;;

let%expect_test "typed variants recursive" =
  let module T = struct
    type me =
      | Nil
      | Cons of int * me
    [@@deriving typed_variants, sexp]

    let rec form () =
      Form.Typed.Variant.make
        (module struct
          module Typed_variant = Typed_variant_of_me

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
              | Nil -> Bonsai.const (Form.return ())
              | Cons ->
                let%map.Computation int = Form.Elements.Textbox.int [%here]
                and me = Bonsai.lazy_ (lazy (form ())) in
                Form.both int me
          ;;
        end)
    ;;

    let form = form ()
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.me]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Ok Nil)

    ==============
    <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true"> nil </option>
      <option value="1" #selected="false"> cons </option>
    </select> |}];
  Handle.input_text handle ~get_vdom ~selector:"select" ~text:"1";
  Handle.show handle;
  [%expect
    {|
    (Error "Expecting an integer")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
        <option value="0" #selected="false"> nil </option>
        <option value="1" #selected="true"> cons </option>
      </select>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="true"> nil </option>
        <option value="1" #selected="false"> cons </option>
      </select>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expecting an integer")
    +|(Ok (Cons 123 Nil))

      ==============
      <div>
        <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
          <option value="0" #selected="false"> nil </option>
          <option value="1" #selected="true"> cons </option>
        </select>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized=123
               oninput> </input>
        <select id="bonsai_path_replaced_in_test"
                class="widget-dropdown"
                onchange
                style={
                  width: 100.00%;
                }>
          <option value="0" #selected="true"> nil </option>
          <option value="1" #selected="false"> cons </option>
        </select>
      </div> |}]
;;

let%expect_test "typed variants" =
  let module T = struct
    type t =
      | Unit
      | Integer of int
      | Text of string
    [@@deriving typed_variants, sexp]

    let form =
      Form.Typed.Variant.make
        (module struct
          module Typed_variant = Typed_variant

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
              | Unit -> Bonsai.const (Form.return ())
              | Integer -> Form.Elements.Textbox.int [%here]
              | Text -> Form.Elements.Textbox.string [%here]
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Ok Unit)

    ==============
    <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true"> unit </option>
      <option value="1" #selected="false"> integer </option>
      <option value="2" #selected="false"> text </option>
    </select> |}];
  Handle.input_text handle ~get_vdom ~selector:"select" ~text:"1";
  Handle.show handle;
  [%expect
    {|
    (Error "Expecting an integer")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
        <option value="0" #selected="false"> unit </option>
        <option value="1" #selected="true"> integer </option>
        <option value="2" #selected="false"> text </option>
      </select>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expecting an integer")
    +|(Ok (Integer 123))

      ==============
      <div>
        <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
          <option value="0" #selected="false"> unit </option>
          <option value="1" #selected="true"> integer </option>
          <option value="2" #selected="false"> text </option>
        </select>
        <input type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized=123
               oninput> </input>
      </div> |}];
  Handle.do_actions handle [ Unit ];
  Handle.show handle;
  [%expect
    {|
    (Ok Unit)

    ==============
    <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true"> unit </option>
      <option value="1" #selected="false"> integer </option>
      <option value="2" #selected="false"> text </option>
    </select> |}];
  Handle.do_actions handle [ Text "abc" ];
  Handle.show handle;
  [%expect
    {|
    (Ok (Text abc))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test" class="widget-dropdown" onchange style={ width: 100.00%; }>
        <option value="0" #selected="false"> unit </option>
        <option value="1" #selected="false"> integer </option>
        <option value="2" #selected="true"> text </option>
      </select>
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=abc
             oninput> </input>
    </div> |}]
;;

let%expect_test "file picker single" =
  let component =
    Form.Elements.File_select.single_opt
      [%here]
      ~accept:[ `Extension ".txt"; `Extension "png"; `Mimetype "image/jpeg" ]
      ()
  in
  let handle =
    Handle.create (form_result_spec [%sexp_of: Bonsai_web_ui_file.t option]) component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <input type="file" accept=".txt,.png,image/jpeg" id="bonsai_path_replaced_in_test" oninput> </input>

      |}];
  let test_file =
    Bonsai_web_ui_file.For_testing.(
      create (Test_data.create_static ~filename:"foo.txt" ~contents:"hello world"))
  in
  Handle.do_actions handle [ Some test_file ];
  Handle.show_diff handle;
  [%expect
    {|
  -|(Ok ())
  +|(Ok ("<file foo.txt>"))

    ==============
    <input type="file" accept=".txt,.png,image/jpeg" id="bonsai_path_replaced_in_test" oninput> </input> |}]
;;

let%expect_test "file picker list" =
  let component =
    Form.Elements.File_select.multiple
      [%here]
      ~accept:[ `Extension ".doc"; `Extension "docx" ]
      ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Bonsai_web_ui_file.t Filename.Map.t])
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <input type="file" accept=".doc,.docx" multiple="" id="bonsai_path_replaced_in_test" oninput> </input>

      |}];
  Handle.do_actions
    handle
    [ Filename.Map.of_alist_exn
        [ ( "foo.txt"
          , Bonsai_web_ui_file.For_testing.(
              create (Test_data.create_static ~filename:"foo.txt" ~contents:"hello world"))
          )
        ; ( "bar.pdf"
          , Bonsai_web_ui_file.For_testing.(
              create (Test_data.create_static ~filename:"bar.pdf" ~contents:"world hello"))
          )
        ]
    ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (
    +|  (bar.pdf "<file bar.pdf>")
    +|  (foo.txt "<file foo.txt>")))

      ==============
      <input type="file" accept=".doc,.docx" multiple="" id="bonsai_path_replaced_in_test" oninput> </input> |}]
;;

let%expect_test "on_change handler should fire when input is changed" =
  let component =
    let%sub input = Form.Elements.Textbox.string [%here] in
    let%sub () =
      Form.Dynamic.on_change
        (module String)
        ~f:
          (Value.return
           @@ fun new_value ->
           Ui_effect.print_s [%message "the input changed to" (new_value : string)])
        input
    in
    Bonsai.read input
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input>

    ("the input changed to" (new_value "")) |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             oninput> </input>
    ("the input changed to" (new_value "hello world")) |}]
;;

let submit_test_attrs = function
  | "onsubmit" | "onclick" | "disabled" | "value:normalized" -> true
  | _ -> false
;;

let%expect_test "form with both submit button and form on-submit" =
  let component = Form.Elements.Textbox.string [%here] in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:(Form.Submit.create () ~f:(fun s -> Ui_effect.print_s (Sexp.Atom s)))
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:submit_test_attrs
         ~get_vdom)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <form onsubmit>
      <table>
        <tbody>
          <tr>
            <td>  </td>
            <td>
              <input value:normalized=""> </input>
            </td>
            <td>
              <div>   </div>
            </td>
          </tr>
          <tr>
            <td>
              <button onclick> submit </button>
            </td>
          </tr>
        </tbody>
      </table>
    </form> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.flush handle;
  [%expect {| "hello world" |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello there";
  Handle.submit_form handle ~get_vdom ~selector:"form";
  [%expect {| "hello there" |}]
;;

let%expect_test "both button and on-submit are disabled when the form doesn't validate" =
  let component = Form.Elements.Textbox.int [%here] in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create () ~f:(fun s ->
           Ui_effect.print_s (Sexp.Atom (Int.to_string s))))
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: int]
         ~filter_printed_attributes:submit_test_attrs
         ~get_vdom)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "Expecting an integer")

    ==============
    <form onsubmit>
      <table>
        <tbody>
          <tr>
            <td>  </td>
            <td>
              <input value:normalized=""> </input>
            </td>
            <td>
              <div>   </div>
            </td>
          </tr>
          <tr>
            <td>
              <button disabled=""> submit </button>
            </td>
          </tr>
        </tbody>
      </table>
    </form> |}];
  (* This form submission does nothing.  Well, it does prevent-default and
     stop-propagation, but the callback certainly doesn't fire. *)
  Handle.submit_form handle ~get_vdom ~selector:"form";
  Handle.flush handle;
  [%expect {||}];
  (* Once we put a valid number in, the form will be clickable, and
     submittable *)
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.submit_form handle ~get_vdom ~selector:"form";
  Handle.flush handle;
  [%expect {| 123 |}];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expecting an integer")
    +|(Ok 123)

      ==============
      <form onsubmit>
        <table>
          <tbody>
            <tr>
              <td>  </td>
              <td>
    -|          <input value:normalized=""> </input>
    +|          <input value:normalized=123> </input>
              </td>
              <td>
                <div>   </div>
              </td>
            </tr>
            <tr>
              <td>
    -|          <button disabled=""> submit </button>
    +|          <button onclick> submit </button>
              </td>
            </tr>
          </tbody>
        </table>
      </form> |}]
;;

let%expect_test "form with just button" =
  let component = Form.Elements.Textbox.string [%here] in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create () ~handle_enter:false ~f:(fun s ->
           Ui_effect.print_s (Sexp.Atom s)))
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:submit_test_attrs
         ~get_vdom)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <table>
      <tbody>
        <tr>
          <td>  </td>
          <td>
            <input value:normalized=""> </input>
          </td>
          <td>
            <div>   </div>
          </td>
        </tr>
        <tr>
          <td>
            <button onclick> submit </button>
          </td>
        </tr>
      </tbody>
    </table> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.flush handle;
  [%expect {| "hello world" |}]
;;

let%expect_test "form with just enter" =
  let component = Form.Elements.Textbox.string [%here] in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create () ~f:(fun s -> Ui_effect.print_s (Sexp.Atom s)) ~button:None)
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:submit_test_attrs
         ~get_vdom)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <form onsubmit>
      <table>
        <tbody>
          <tr>
            <td>  </td>
            <td>
              <input value:normalized=""> </input>
            </td>
            <td>
              <div>   </div>
            </td>
          </tr>
        </tbody>
      </table>
    </form> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello there";
  Handle.submit_form handle ~get_vdom ~selector:"form";
  [%expect {| "hello there" |}]
;;

let%expect_test "form validated with an effect" =
  let module Q = Effect.For_testing.Query_response_tracker in
  let tracker = Q.create () in
  let print_queued () = print_s [%sexp (Q.queries_pending_response tracker : int list)] in
  let not_positive = Error (Error.of_string "not positive") in
  let f =
    tracker |> Effect.For_testing.of_query_response_tracker |> Bonsai.Value.return
  in
  let component =
    let%sub textbox = Form.Elements.Textbox.int [%here] in
    Form.Dynamic.validate_via_effect (module Int) textbox ~f
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: int]
         ~filter_printed_attributes:(Fn.const false)
         ~get_vdom)
      component
  in
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input> |}];
  print_queued ();
  [%expect {| (2) |}];
  Q.maybe_respond tracker ~f:(fun i ->
    Respond (if Int.is_positive i then Ok () else not_positive));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Ok 2)

    ==============
    <input> </input> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"5";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"-3";
  Handle.recompute_view_until_stable handle;
  print_queued ();
  [%expect {| (-3 5) |}];
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input> |}];
  Q.maybe_respond tracker ~f:(function
    | 5 -> Respond (Ok ())
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input> |}];
  Q.maybe_respond tracker ~f:(function
    | -3 -> Respond not_positive
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error "not positive")

    ==============
    <input> </input> |}]
;;

let%expect_test "slider input" =
  let component = Form.Elements.Range.int [%here] ~min:0 ~max:100 ~default:0 ~step:1 () in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create
           ()
           ~f:(fun x -> Ui_effect.print_s (Sexp.Atom (Int.to_string x)))
           ~button:None)
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: int]
         ~filter_printed_attributes:submit_test_attrs
         ~get_vdom)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <form onsubmit>
      <table>
        <tbody>
          <tr>
            <td>  </td>
            <td>
              <input value:normalized=0> </input>
            </td>
            <td>
              <div>   </div>
            </td>
          </tr>
        </tbody>
      </table>
    </form> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"20";
  Handle.submit_form handle ~get_vdom ~selector:"form";
  [%expect {|
    20 |}]
;;

let%expect_test "slider input" =
  let var = Bonsai.Var.create (String.Map.of_alist_exn [ "abc", "abc"; "def", "def" ]) in
  let value = Bonsai.Var.value var in
  let component = Form.Elements.Query_box.stringable (module String) value in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create () ~f:(fun x -> Ui_effect.print_s (Sexp.Atom x)) ~button:None)
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:(function
           | "style.color" -> true
           | _ -> false)
         ~get_vdom)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <form>
      <table>
        <tbody>
          <tr>
            <td>  </td>
            <td>
              <div>
                <div style={ color: gray; }> Nothing selected </div>
                <div>
                  <input> </input>
                  <div>
                    <div> </div>
                  </div>
                </div>
              </div>
            </td>
            <td>
              <div>   </div>
            </td>
          </tr>
        </tbody>
      </table>
    </form> |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"a";
  Handle.keydown handle ~get_vdom ~selector:"input" ~key:Enter;
  Handle.show_diff handle;
  [%expect
    {|
    ("default prevented" (key Enter))

    -|(Error "a value is required")
    +|(Ok abc)

      ==============
      <form>
        <table>
          <tbody>
            <tr>
              <td>  </td>
              <td>
                <div>
    -|            <div style={ color: gray; }> Nothing selected </div>
    +|            <div> abc </div>
                  <div>
                    <input> </input>
                    <div>
                      <div> </div>
                    </div>
                  </div>
                </div>
              </td>
              <td>
                <div>   </div>
              </td>
            </tr>
          </tbody>
        </table>
      </form> |}];
  Bonsai.Var.update var ~f:(fun map -> String.Map.remove map "abc");
  Handle.show_diff handle;
  [%expect
    {|
      (Ok abc)

      ==============
      <form>
        <table>
          <tbody>
            <tr>
              <td>  </td>
              <td>
                <div>
    -|            <div> abc </div>
    +|            <div style={ color: red; }> Selected item is not an input option </div>
                  <div>
                    <input> </input>
                    <div>
                      <div> </div>
                    </div>
                  </div>
                </div>
              </td>
              <td>
                <div>   </div>
              </td>
            </tr>
          </tbody>
        </table>
      </form> |}]
;;

let%expect_test "add tooltip to form" =
  let component =
    let%map.Computation x = Form.Elements.Textbox.string [%here] in
    Form.tooltip "tooltip" x
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: string] ~get_vdom:get_vdom_verbose)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

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
              }>  </td>
          <td>
            <input type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
              <div class="container_hash_replaced_in_test">
                <label class="label_hash_replaced_in_test">
                  <input type="checkbox" class="checkbox_hash_replaced_in_test"> </input>
                  <span class="span_hash_replaced_in_test"> ⓘ </span>
                  <div class="text_hash_replaced_in_test"> tooltip </div>
                </label>
              </div>

            </div>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "add tooltip to group" =
  let component =
    let%map.Computation x = Form.Elements.Textbox.string [%here] in
    Form.tooltip "group tooltip" (Form.group "group" x)
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: string] ~get_vdom:get_vdom_verbose)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }> group </td>
          <td>
            <div class="container_hash_replaced_in_test">
              <label class="label_hash_replaced_in_test">
                <input type="checkbox" class="checkbox_hash_replaced_in_test"> </input>
                <span class="span_hash_replaced_in_test"> ⓘ </span>
                <div class="text_hash_replaced_in_test"> group tooltip </div>
              </label>
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
            <input type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "Bonsai_form.Typed sets groups/labels correctly on nested records" =
  let module A = struct
    let checkbox = Form.Elements.Checkbox.bool ~default:false

    module B = struct
      type t =
        { b_1 : bool
        ; b_2 : bool
        }
      [@@deriving typed_fields, sexp]

      let form () =
        Form.Typed.Record.make
          (module struct
            module Typed_field = Typed_field

            let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
              = function
                | B_1 -> checkbox [%here]
                | B_2 -> checkbox [%here]
            ;;
          end)
      ;;
    end

    type t =
      { a_1 : bool
      ; a_2 : B.t
      }
    [@@deriving typed_fields, sexp]

    let form () =
      Form.Typed.Record.make
        (module struct
          module Typed_field = Typed_field

          let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
            = function
              | A_1 -> checkbox [%here]
              | A_2 -> B.form ()
          ;;
        end)
    ;;
  end
  in
  let component = A.form () in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: A.t]
         ~filter_printed_attributes:(function
           | "style.padding-left" -> true
           | _ -> false)
         ~get_vdom:get_vdom_verbose)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a_1 false)
      (a_2 (
        (b_1 false)
        (b_2 false)))))

    ==============
    <table>
      <tbody>
        <tr>
          <td style={ padding-left: 0em; }>
            <label> a_1 </label>
          </td>
          <td>
            <input> </input>
          </td>
          <td>
            <div>   </div>
          </td>
        </tr>
        <tr>
          <td style={ padding-left: 0em; }> a_2 </td>
        </tr>
        <tr>
          <td style={ padding-left: 1em; }>
            <label> b_1 </label>
          </td>
          <td>
            <input> </input>
          </td>
          <td>
            <div>   </div>
          </td>
        </tr>
        <tr>
          <td style={ padding-left: 1em; }>
            <label> b_2 </label>
          </td>
          <td>
            <input> </input>
          </td>
          <td>
            <div>   </div>
          </td>
        </tr>
      </tbody>
    </table> |}]
;;

let%expect_test "view_as_vdom editable:`Currently_yes" =
  let component = Form.Elements.Textbox.string [%here] in
  let get_vdom = get_vdom_verbose ~editable:`Currently_yes in
  let handle = Handle.create (form_result_spec ~get_vdom [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <fieldset class="clear_fieldset_styles_hash_replaced_in_test">
      <table>
        <tbody>
          <tr @key=bonsai_path_replaced_in_test>
            <td style={
                  padding-left: 0em;
                  padding-right: 1em;
                  text-align: left;
                  font-weight: bold;
                  user-select: none;
                }>  </td>
            <td>
              <input type="text"
                     placeholder=""
                     spellcheck="false"
                     id="bonsai_path_replaced_in_test"
                     value:normalized=""
                     oninput> </input>
            </td>
            <td>
              <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
            </td>
          </tr>
        </tbody>
      </table>
    </fieldset> |}]
;;

let%expect_test "view_as_vdom editable:`Currently_no" =
  (* We can't programatically test when inputs aren't editable, because they can still be
     set using javascript events like onclick, so just validate that there is a disabled
     fieldset wrapping the textbox *)
  let component = Form.Elements.Textbox.string [%here] in
  let get_vdom = get_vdom_verbose ~editable:`Currently_no in
  let handle = Handle.create (form_result_spec ~get_vdom [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <fieldset disabled="" class="clear_fieldset_styles_hash_replaced_in_test">
      <table>
        <tbody>
          <tr @key=bonsai_path_replaced_in_test>
            <td style={
                  padding-left: 0em;
                  padding-right: 1em;
                  text-align: left;
                  font-weight: bold;
                  user-select: none;
                }>  </td>
            <td>
              <input type="text"
                     placeholder=""
                     spellcheck="false"
                     id="bonsai_path_replaced_in_test"
                     value:normalized=""
                     oninput> </input>
            </td>
            <td>
              <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
            </td>
          </tr>
        </tbody>
      </table>
    </fieldset> |}]
;;

let%expect_test "view_as_vdom not editable, with on_submit" =
  let component = Form.Elements.Textbox.string [%here] in
  let on_submit = Form.Submit.create ~f:(fun _ -> Effect.Ignore) () in
  let get_vdom = get_vdom_verbose ~on_submit ~editable:`Currently_no in
  let handle = Handle.create (form_result_spec ~get_vdom [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <form onsubmit>
      <fieldset disabled="" class="clear_fieldset_styles_hash_replaced_in_test">
        <table>
          <tbody>
            <tr @key=bonsai_path_replaced_in_test>
              <td style={
                    padding-left: 0em;
                    padding-right: 1em;
                    text-align: left;
                    font-weight: bold;
                    user-select: none;
                  }>  </td>
              <td>
                <input type="text"
                       placeholder=""
                       spellcheck="false"
                       id="bonsai_path_replaced_in_test"
                       value:normalized=""
                       oninput> </input>
              </td>
              <td>
                <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>   </div>
              </td>
            </tr>
            <tr>
              <td colspan="2" style={ padding-left: 0em; }>
                <button onclick> submit </button>
              </td>
            </tr>
          </tbody>
        </table>
      </fieldset>
    </form> |}]
;;
