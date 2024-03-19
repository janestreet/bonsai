open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

let get_vdom form =
  match Form.View.to_vdom_plain (Form.view form) with
  | [ v ] -> v
  | other -> Vdom.Node.div other
;;

let get_vdom_verbose ?on_submit ?editable f = Form.view_as_vdom ?on_submit ?editable f

let form_result_spec
  (type a)
  ?filter_printed_attributes
  ?censor_paths
  ?(get_vdom = get_vdom)
  sexp_of_a
  : (a Form.t, a) Result_spec.t
  =
  (module struct
    type t = a Form.t
    type incoming = a

    let view form =
      let module V = (val Result_spec.vdom ?filter_printed_attributes ?censor_paths Fn.id)
      in
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

let verbose_form_result_spec (type a) sexp_of_a : (a Form.t, a) Result_spec.t =
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

let%expect_test "setting a constant form does nothing" =
  let component =
    Form.Elements.Non_interactive.constant
      (Value.return (Vdom.Node.text "test"))
      (Value.return (Ok "test"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect {|
    (Ok test)

    ==============
    test
    |}];
  Handle.do_actions handle [ "not test" ];
  Handle.show_diff handle
;;

let%expect_test "typing into a string textbox" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             oninput> </input>
    |}]
;;

let%expect_test "typing into a string password textbox" =
  let component = Form.Elements.Password.string ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="password"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="password"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             oninput> </input>
    |}]
;;

let%expect_test "dropdown starting empty" =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Value.return [ "hello"; "world" ])
      ~init:`Empty
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select @key=bonsai_path_replaced_in_test
            id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="false"> world </option>
    </select>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok hello)

      ==============
      <select @key=bonsai_path_replaced_in_test
              id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
    -|  <option value="0" #selected="true">  </option>
    +|  <option value="0" #selected="false">  </option>
    -|  <option value="1" #selected="false"> hello </option>
    +|  <option value="1" #selected="true"> hello </option>
        <option value="2" #selected="false"> world </option>
      </select>
    |}]
;;

let%expect_test "dropdown with default value" =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Value.return [ "hello"; "world" ])
      ~init:(`This (Value.return "world"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok world)

    ==============
    <select @key=bonsai_path_replaced_in_test
            id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false"> hello </option>
      <option value="1" #selected="true"> world </option>
    </select>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"0";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok world)
    +|(Ok hello)

      ==============
      <select @key=bonsai_path_replaced_in_test
              id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
    -|  <option value="0" #selected="false"> hello </option>
    +|  <option value="0" #selected="true"> hello </option>
    -|  <option value="1" #selected="true"> world </option>
    +|  <option value="1" #selected="false"> world </option>
      </select>
    |}]
;;

let%expect_test "dropdown_opt with default value" =
  let component =
    Form.Elements.Dropdown.list_opt
      (module String)
      ~equal:[%equal: String.t]
      (Value.return [ "hello"; "world" ])
      ~init:(`This (Value.return "world"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string option]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok (world))

    ==============
    <select @key=bonsai_path_replaced_in_test
            id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="true"> world </option>
    </select>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"0";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (world))
    +|(Ok ())

      ==============
      <select @key=bonsai_path_replaced_in_test
              id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
    -|  <option value="0" #selected="false">  </option>
    +|  <option value="0" #selected="true">  </option>
        <option value="1" #selected="false"> hello </option>
    -|  <option value="2" #selected="true"> world </option>
    +|  <option value="2" #selected="false"> world </option>
      </select>
    |}]
;;

let%expect_test "dropdown" =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Value.return [ "hello"; "world" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok hello)

    ==============
    <select @key=bonsai_path_replaced_in_test
            id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true"> hello </option>
      <option value="1" #selected="false"> world </option>
    </select>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok hello)
    +|(Ok world)

      ==============
      <select @key=bonsai_path_replaced_in_test
              id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
    -|  <option value="0" #selected="true"> hello </option>
    +|  <option value="0" #selected="false"> hello </option>
    -|  <option value="1" #selected="false"> world </option>
    +|  <option value="1" #selected="true"> world </option>
      </select>
    |}]
;;

let%expect_test "dropdown but without any elements to pick from " =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Value.return [])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select @key=bonsai_path_replaced_in_test
            id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
    </select>
    |}]
;;

let%expect_test "collapsible group" =
  let component =
    let%sub textbox =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
    in
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
            <label style={ display: block; }>
              <div onclick style={ user-select: none; cursor: pointer; }> ▾ group-name </div>
            </label>
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
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
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
              <label style={ display: block; }>
    -|          <div onclick style={ user-select: none; cursor: pointer; }> ▾ group-name </div>
    +|          <div onclick style={ user-select: none; cursor: pointer; }> ► group-name </div>
              </label>
            </td>
          </tr>
    -|    <tr @key=bonsai_path_replaced_in_test>
    -|      <td style={
    -|            padding-left: 1em;
    -|            padding-right: 1em;
    -|            text-align: left;
    -|            font-weight: bold;
    -|            user-select: none;
    -|          }>  </td>
    -|      <td>
    -|        <input @key=bonsai_path_replaced_in_test
    -|               type="text"
    -|               placeholder=""
    -|               spellcheck="false"
    -|               id="bonsai_path_replaced_in_test"
    -|               value:normalized=""
    -|               oninput> </input>
    -|      </td>
    -|    </tr>
        </tbody>
      </table>
    |}]
;;

let%expect_test "setting into a string textbox" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input>
    |}];
  Handle.do_actions handle [ "hello world" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             oninput> </input>
    |}]
;;

let%expect_test "typing into a int textbox" =
  let component = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok 123)

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized=123
             oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 123)
    +|(Error "Expected an integer")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=123
    +|       value:normalized="hello world"
             oninput> </input>
    |}]
;;

let%expect_test "setting into a int textbox" =
  let component = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input>
    |}];
  Handle.do_actions handle [ 123 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok 123)

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized=123
             oninput> </input>
    |}]
;;

let%expect_test "typing into a paired string textbox * int textbox " =
  let component =
    let%sub string_form =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
    in
    let%sub int_form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    let%arr string_form = string_form
    and int_form = int_form in
    Form.both string_form int_form
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string * int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(1)" ~text:"hello world";
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(2)" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok ("hello world" 123))

      ==============
      <div>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized="hello world"
               oninput> </input>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized=123
               oninput> </input>
      </div>
    |}]
;;

let%expect_test "setting into a paired string textbox * int textbox " =
  let component =
    let%sub string_form =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
    in
    let%sub int_form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    let%arr string_form = string_form
    and int_form = int_form in
    Form.both string_form int_form
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string * int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.do_actions handle [ "hello world", 123 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok ("hello world" 123))

      ==============
      <div>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized="hello world"
               oninput> </input>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized=123
               oninput> </input>
      </div>
    |}]
;;

let%test_module "Form.all" =
  (module struct
    let make_handle () =
      let component =
        let%sub string_forms =
          List.init 3 ~f:(fun _ ->
            Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
          |> Computation.all
        in
        let%arr string_forms = string_forms in
        Form.all string_forms
      in
      Handle.create (form_result_spec [%sexp_of: string list]) component
    ;;

    let%expect_test "typing into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
        (Ok ("" "" ""))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
        </div>
        |}];
      Handle.input_text
        handle
        ~get_vdom
        ~selector:"input:nth-child(1)"
        ~text:"hello world";
      Handle.input_text handle ~get_vdom ~selector:"input:nth-child(2)" ~text:"quack";
      Handle.show_diff handle;
      [%expect
        {|
        -|(Ok ("" "" ""))
        +|(Ok ("hello world" quack ""))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized=quack
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
        (Ok ("" "" ""))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
        </div>
        |}];
      Handle.do_actions handle [ [ "hello world"; "quack"; "" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        -|(Ok ("" "" ""))
        +|(Ok ("hello world" quack ""))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized=quack
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes (more values than forms)" =
      let handle = make_handle () in
      Handle.store_view handle;
      Handle.do_actions handle [ [ "hello world"; "quack"; ""; "oh no" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        ("WARNING: Form.set called on result of Form.all with a list value whose length doesn't match the number of forms "
         "more values than forms" (form_count 3) (edits_count 4)
         "dropping left-over values")

        -|(Ok ("" "" ""))
        +|(Ok ("hello world" quack ""))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized=quack
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes (more forms than values)" =
      let handle = make_handle () in
      Handle.store_view handle;
      Handle.do_actions handle [ [ "hello world"; "quack" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        ("WARNING: Form.set called on result of Form.all with a list value whose length doesn't match the number of forms "
         "more forms than values" (form_count 3) (edits_count 2)
         "not setting left-over forms")

        -|(Ok ("" "" ""))
        +|(Ok ("hello world" quack ""))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized=quack
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </div>
        |}]
    ;;
  end)
;;

let%test_module "Form.all_map" =
  (module struct
    let make_handle () =
      let component =
        let%sub string_forms =
          List.init 3 ~f:(fun i ->
            i, Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
          |> Int.Map.of_alist_exn
          |> Computation.all_map
        in
        let%arr string_forms = string_forms in
        Form.all_map string_forms
      in
      Handle.create (form_result_spec [%sexp_of: string Int.Map.t]) component
    ;;

    let%expect_test "typing into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
        (Ok (
          (0 "")
          (1 "")
          (2 "")))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
        </div>
        |}];
      Handle.input_text
        handle
        ~get_vdom
        ~selector:"input:nth-child(1)"
        ~text:"hello world";
      Handle.input_text handle ~get_vdom ~selector:"input:nth-child(2)" ~text:"quack";
      Handle.show_diff handle;
      [%expect
        {|
          (Ok (
        -|  (0 "")
        +|  (0 "hello world")
        -|  (1 "")
        +|  (1 quack)
            (2 "")))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized=quack
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
        (Ok (
          (0 "")
          (1 "")
          (2 "")))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=""
                 oninput> </input>
        </div>
        |}];
      Handle.do_actions
        handle
        [ Int.Map.of_alist_exn [ 0, "hello world"; 1, "quack"; 2, "" ] ];
      Handle.show_diff handle;
      [%expect
        {|
          (Ok (
        -|  (0 "")
        +|  (0 "hello world")
        -|  (1 "")
        +|  (1 quack)
            (2 "")))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized=quack
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes (more values than forms)" =
      let handle = make_handle () in
      Handle.store_view handle;
      Handle.do_actions
        handle
        [ Int.Map.of_alist_exn [ 0, "hello world"; 1, "quack"; 2, ""; 3, "oh no" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        ("WARNING: Form.set on the result of Form.all_map has mismatched keys"
         "update contains key not present in active forms" (key 3))

          (Ok (
        -|  (0 "")
        +|  (0 "hello world")
        -|  (1 "")
        +|  (1 quack)
            (2 "")))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized=quack
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes (more forms than values)" =
      let handle = make_handle () in
      Handle.store_view handle;
      Handle.do_actions handle [ Int.Map.of_alist_exn [ 0, "hello world"; 1, "quack" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        ("WARNING: Form.set on the result of Form.all_map has mismatched keys"
         "update is missing key present in active form" (key 2))

          (Ok (
        -|  (0 "")
        +|  (0 "hello world")
        -|  (1 "")
        +|  (1 quack)
            (2 "")))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
        -|         value:normalized=""
        +|         value:normalized=quack
                   oninput> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
          </div>
        |}]
    ;;
  end)
;;

let%expect_test "typing into a time span textbox" =
  let component =
    Form.Elements.Date_time.time_span ~allow_updates_when_focused:`Never ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Time_ns.Span.t]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}];
  Handle.input_text handle ~selector:"input" ~get_vdom ~text:"24";
  Handle.show handle;
  [%expect
    {|
    (Ok 24s)

    ==============
    <div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=24
             oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}];
  Handle.change handle ~selector:"select" ~get_vdom ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok 24m)

    ==============
    <div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=24
             oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="false"> s </option>
        <option value="2" #selected="true"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}]
;;

let%expect_test "setting into a time span textbox" =
  let component =
    Form.Elements.Date_time.time_span ~allow_updates_when_focused:`Never ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Time_ns.Span.t]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}];
  Handle.do_actions handle [ Time_ns.Span.of_sec 24. ];
  Handle.show handle;
  [%expect
    {|
    (Ok 24s)

    ==============
    <div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=24
             oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}];
  Handle.do_actions handle [ Time_ns.Span.of_hr 24. ];
  Handle.show handle;
  [%expect
    {|
    (Ok 1d)

    ==============
    <div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=24
             oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="false"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="true"> h </option>
      </select>
    </div>
    |}]
;;

let%expect_test "typing into a time range textbox, with strict inequality required" =
  let component =
    Form.Elements.Date_time.Range.time ~allow_updates_when_focused:`Never ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Time_ns.Ofday.t * Time_ns.Ofday.t])
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~text:"11:11 AM" ~selector:"input:nth-child(1)";
  Handle.show handle;
  [%expect
    {|
    (Error "A value is required for the end of this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~text:"10:00 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Error "Start time must be strictly before the end time.")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=10:00:00.000
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~text:"11:11 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Error "Start time must be strictly before the end time.")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~text:"11:12 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Ok (11:11:00.000000000 11:12:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:12:00.000
             oninput> </input>
    </div>
    |}]
;;

let%expect_test "typing into a time range textbox, with equality allowed" =
  let component =
    Form.Elements.Date_time.Range.time
      ~allow_equal:true
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Time_ns.Ofday.t * Time_ns.Ofday.t])
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~text:"11:11 AM" ~selector:"input:nth-child(1)";
  Handle.input_text handle ~get_vdom ~text:"10:00 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Error "Start time must be before or the same as the end time.")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=10:00:00.000
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~text:"11:11 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Ok (11:11:00.000000000 11:11:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             oninput> </input>
    </div>
    |}]
;;

let%expect_test "setting into a date range, with strict inequality required" =
  let component =
    Form.Elements.Date_time.Range.time ~allow_updates_when_focused:`Never ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Time_ns.Ofday.t * Time_ns.Ofday.t])
      component
  in
  let ten_am = Time_ns.Ofday.of_string "10:00 AM" in
  let eleven_am = Time_ns.Ofday.of_string "11:00 AM" in
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  (* Somehow, a bad range got set, so no setting should happen *)
  Handle.do_actions handle [ eleven_am, ten_am ];
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  (* A range where the endpoints are equal is not allowed in this case *)
  Handle.do_actions handle [ eleven_am, eleven_am ];
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  (* Finally, a good range! *)
  Handle.do_actions handle [ ten_am, eleven_am ];
  Handle.show handle;
  [%expect
    {|
    (Ok (10:00:00.000000000 11:00:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=10:00:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:00:00.000
             oninput> </input>
    </div>
    |}]
;;

let%expect_test "setting into a date range, with equality allowed" =
  let component =
    Form.Elements.Date_time.Range.time
      ~allow_equal:true
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Time_ns.Ofday.t * Time_ns.Ofday.t])
      component
  in
  let ten_am = Time_ns.Ofday.of_string "10:00 AM" in
  let eleven_am = Time_ns.Ofday.of_string "11:00 AM" in
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.do_actions handle [ eleven_am, ten_am ];
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.do_actions handle [ eleven_am, eleven_am ];
  Handle.show handle;
  [%expect
    {|
    (Ok (11:00:00.000000000 11:00:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:00:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:00:00.000
             oninput> </input>
    </div>
    |}];
  Handle.do_actions handle [ ten_am, eleven_am ];
  Handle.show handle;
  [%expect
    {|
    (Ok (10:00:00.000000000 11:00:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=10:00:00.000
             oninput> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:00:00.000
             oninput> </input>
    </div>
    |}]
;;

let%expect_test "adding more things to a string list (indented button)" =
  let component =
    Form.Elements.Multiple.list
      ~button_placement:`Indented
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
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
            <button type="button" onclick> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Handle.click_on handle ~get_vdom:get_vdom_verbose ~selector:"button";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (""))

      ==============
      <table>
        <tbody>
          <tr>
            <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
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
    +|        <input @key=bonsai_path_replaced_in_test
    +|               type="text"
    +|               placeholder=""
    +|               spellcheck="false"
    +|               id="bonsai_path_replaced_in_test"
    +|               value:normalized=""
    +|               oninput> </input>
    +|      </td>
    +|    </tr>
    +|    <tr>
    +|      <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
              <button type="button" onclick> Add new element </button>
            </td>
          </tr>
        </tbody>
      </table>
    |}];
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
              <input @key=bonsai_path_replaced_in_test
                     type="text"
                     placeholder=""
                     spellcheck="false"
                     id="bonsai_path_replaced_in_test"
    -|               value:normalized=""
    +|               value:normalized="hello world"
                     oninput> </input>
            </td>
          </tr>
          <tr>
            <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
              <button type="button" onclick> Add new element </button>
            </td>
          </tr>
        </tbody>
      </table>
    |}]
;;

let%expect_test "adding more things to a string list" =
  let component =
    Form.Elements.Multiple.list
      ~button_placement:`Inline
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string list]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <button type="button" onclick> Add new element </button>
    |}];
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (""))

      ==============
    +|<div>
    +|  <input @key=bonsai_path_replaced_in_test
    +|         type="text"
    +|         placeholder=""
    +|         spellcheck="false"
    +|         id="bonsai_path_replaced_in_test"
    +|         value:normalized=""
    +|         oninput> </input>
        <button type="button" onclick> Add new element </button>
    +|</div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (""))
    +|(Ok ("hello world"))

      ==============
      <div>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized="hello world"
               oninput> </input>
        <button type="button" onclick> Add new element </button>
      </div>
    |}]
;;

let%expect_test "using the same component twice" =
  let component =
    let%sub textbox =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
    in
    return @@ Bonsai.Value.map2 ~f:Form.both textbox textbox
  in
  let handle =
    Handle.create
      (form_result_spec
         ~censor_paths:false
         ~get_vdom:get_vdom_verbose
         [%sexp_of: string * string])
      component
  in
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
        <tr @key=bonsai_path_x>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input @key=bonsai_path_y
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_x"
                   value:normalized=b
                   oninput> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_x>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input @key=bonsai_path_y
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_x"
                   value:normalized=b
                   oninput> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "setting things to a string list" =
  let component =
    Form.Elements.Multiple.list
      ~button_placement:`Inline
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string list]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <button type="button" onclick> Add new element </button>
    |}];
  Handle.do_actions handle [ [ "hello"; "there"; "world" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (hello there world))

      ==============
    +|<div>
    +|  <input @key=bonsai_path_replaced_in_test
    +|         type="text"
    +|         placeholder=""
    +|         spellcheck="false"
    +|         id="bonsai_path_replaced_in_test"
    +|         value:normalized=hello
    +|         oninput> </input>
    +|  <input @key=bonsai_path_replaced_in_test
    +|         type="text"
    +|         placeholder=""
    +|         spellcheck="false"
    +|         id="bonsai_path_replaced_in_test"
    +|         value:normalized=there
    +|         oninput> </input>
    +|  <input @key=bonsai_path_replaced_in_test
    +|         type="text"
    +|         placeholder=""
    +|         spellcheck="false"
    +|         id="bonsai_path_replaced_in_test"
    +|         value:normalized=world
    +|         oninput> </input>
        <button type="button" onclick> Add new element </button>
    +|</div>
    |}]
;;

let%expect_test "setting things to a string list (verbose)" =
  let component =
    Form.Elements.Multiple.list
      ~button_placement:`Inline
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
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
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <button type="button" onclick> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
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
    +|        <input @key=bonsai_path_replaced_in_test
    +|               type="text"
    +|               placeholder=""
    +|               spellcheck="false"
    +|               id="bonsai_path_replaced_in_test"
    +|               value:normalized=hello
    +|               oninput> </input>
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
    +|        <input @key=bonsai_path_replaced_in_test
    +|               type="text"
    +|               placeholder=""
    +|               spellcheck="false"
    +|               id="bonsai_path_replaced_in_test"
    +|               value:normalized=there
    +|               oninput> </input>
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
    +|        <input @key=bonsai_path_replaced_in_test
    +|               type="text"
    +|               placeholder=""
    +|               spellcheck="false"
    +|               id="bonsai_path_replaced_in_test"
    +|               value:normalized=world
    +|               oninput> </input>
    +|      </td>
    +|    </tr>
          <tr>
            <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
              <button type="button" onclick> Add new element </button>
            </td>
          </tr>
        </tbody>
      </table>
    |}]
;;

let%expect_test "nonempty list" =
  let component =
    Form.Elements.Multiple.nonempty_list
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
  in
  let get_vdom = get_vdom_verbose in
  let handle =
    Handle.create
      (form_result_spec
         ~get_vdom
         ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
         [%sexp_of: string Nonempty_list.t])
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok (""))

    ==============
    <table>
      <tbody>
        <tr>
          <td> </td>
        </tr>
        <tr>
          <td>  </td>
          <td>
            <input> </input>
          </td>
        </tr>
        <tr>
          <td>
            <button> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (""))
    +|(Ok ("" ""))

      ==============
      <table>
        <tbody>
          <tr>
            <td> </td>
          </tr>
    +|    <tr>
    +|      <td>  </td>
    +|      <td>
    +|        <input> </input>
    +|      </td>
    +|    </tr>
    +|    <tr>
    +|      <td>
    +|        <div>
    +|          1 -
    +|          <button> [ remove ] </button>
    +|        </div>
    +|      </td>
    +|    </tr>
          <tr>
            <td>  </td>
            <td>
              <input> </input>
            </td>
          </tr>
          <tr>
            <td>
              <button> Add new element </button>
            </td>
          </tr>
        </tbody>
      </table>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"tr:nth-child(2) input" ~text:"hi!";
  Handle.input_text handle ~get_vdom ~selector:"tr:nth-child(4) input" ~text:"bye!";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ("" ""))
    +|(Ok (hi! bye!))

      ==============
      <table>
        <tbody>
          <tr>
            <td> </td>
          </tr>
          <tr>
            <td>  </td>
            <td>
              <input> </input>
            </td>
          </tr>
          <tr>
            <td>
              <div>
    |}];
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (hi! bye!))
    +|(Ok (hi!))

      ==============
      <table>
        <tbody>
          <tr>
            <td> </td>
          </tr>
    -|    <tr>
    -|      <td>  </td>
    -|      <td>
    -|        <input> </input>
    -|      </td>
    -|    </tr>
    -|    <tr>
    -|      <td>
    -|        <div>
    -|          1 -
    -|          <button> [ remove ] </button>
    -|        </div>
    -|      </td>
    -|    </tr>
          <tr>
            <td>  </td>
            <td>
              <input> </input>
            </td>
          </tr>
          <tr>
            <td>
              <button> Add new element </button>
            </td>
          </tr>
        </tbody>
      </table>
    |}]
;;

let%expect_test "typing into an int number element (no default)" =
  let component =
    Form.Elements.Number.int
      ~step:1
      ~min:(-1)
      ~max:10
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "value not specified")

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           min="-1"
           max="10"
           value:normalized=""
           oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"10";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "value not specified")
    +|(Ok 10)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=""
    +|       value:normalized=10
             oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10)
    +|(Error "value not specified")

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=10
    +|       value:normalized=""
             oninput> </input>
    |}]
;;

let%expect_test "typing into an int number element" =
  let component =
    Form.Elements.Number.int
      ~default:0
      ~step:1
      ~min:(-1)
      ~max:10
      ~allow_updates_when_focused:`Never
      ()
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
           oninput> </input>
    |}];
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
             oninput> </input>
    |}];
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
             oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"11";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 11) "higher than allowed threshold" (max 10)))

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
             oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"-2";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 11) "higher than allowed threshold" (max 10)))
    +|(Error ((value -2) "lower than allowed threshold" (min -1)))

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
             oninput> </input>
    |}]
;;

let%expect_test "setting into an int number element (no default)" =
  let component =
    Form.Elements.Number.int
      ~step:1
      ~min:(-1)
      ~max:10
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "value not specified")

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           min="-1"
           max="10"
           value:normalized=""
           oninput> </input>
    |}];
  Handle.do_actions handle [ 10 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "value not specified")
    +|(Ok 10)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="-1"
             max="10"
    -|       value:normalized=""
    +|       value:normalized=10
             oninput> </input>
    |}]
;;

let%expect_test "setting into an int number element" =
  let component =
    Form.Elements.Number.int
      ~default:0
      ~step:1
      ~min:(-1)
      ~max:10
      ~allow_updates_when_focused:`Never
      ()
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
           oninput> </input>
    |}];
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
             oninput> </input>
    |}];
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
             oninput> </input>
    |}];
  Handle.do_actions handle [ 11 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 11) "higher than allowed threshold" (max 10)))

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
             oninput> </input>
    |}];
  Handle.do_actions handle [ -2 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 11) "higher than allowed threshold" (max 10)))
    +|(Error ((value -2) "lower than allowed threshold" (min -1)))

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
             oninput> </input>
    |}]
;;

let%expect_test "typing into a float number element" =
  let component =
    Form.Elements.Number.float
      ~default:0.
      ~step:1.
      ~min:(-1.)
      ~max:10.1
      ~allow_updates_when_focused:`Never
      ()
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
           oninput> </input>
    |}];
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
             oninput> </input>
    |}];
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
             oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"10.2";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 10.2) "higher than allowed threshold" (max 10.1)))

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
             oninput> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"-1.1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 10.2) "higher than allowed threshold" (max 10.1)))
    +|(Error ((value -1.1) "lower than allowed threshold" (min -1)))

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
             oninput> </input>
    |}]
;;

let%expect_test "setting into an int number element" =
  let component =
    Form.Elements.Number.float
      ~default:0.
      ~step:1.
      ~min:(-1.)
      ~max:10.1
      ~allow_updates_when_focused:`Never
      ()
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
           oninput> </input>
    |}];
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
             oninput> </input>
    |}];
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
             oninput> </input>
    |}];
  Handle.do_actions handle [ 10.2 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 10.2) "higher than allowed threshold" (max 10.1)))

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
             oninput> </input>
    |}];
  Handle.do_actions handle [ -1.1 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 10.2) "higher than allowed threshold" (max 10.1)))
    +|(Error ((value -1.1) "lower than allowed threshold" (min -1)))

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
             oninput> </input>
    |}]
;;

let%expect_test "clicking on radio buttons" =
  let component =
    Form.Elements.Radio_buttons.list
      ~to_string:Fn.id
      (module String)
      ~equal:[%equal: String.t]
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
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          third
        </label>
      </li>
    </ul>
    |}];
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
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="false"
    +|             #checked="true"
                   onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="false"
                   onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    |}];
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
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="true"
    +|             #checked="false"
                   onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="false"
    +|             #checked="true"
                   onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="false"
                   onclick> </input>
            third
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "setting into radio buttons" =
  let component =
    Form.Elements.Radio_buttons.list
      ~to_string:Fn.id
      (module String)
      ~equal:[%equal: String.t]
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
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          third
        </label>
      </li>
    </ul>
    |}];
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
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="false"
    +|             #checked="true"
                   onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="false"
                   onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    |}];
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
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="true"
    +|             #checked="false"
                   onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="false"
    +|             #checked="true"
                   onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="false"
                   onclick> </input>
            third
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "horizontal radio buttons render with correct styles applied" =
  let component =
    Form.Elements.Radio_buttons.list
      ~to_string:Fn.id
      (module String)
      ~equal:[%equal: String.t]
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
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          first
        </label>
      </li>
    </ul>
    |}]
;;

let%expect_test "clicking a set checklist" =
  let component =
    Form.Elements.Checkbox.set
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
        <label>
          <input type="checkbox" #checked="false" onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" onclick> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" onclick> </input>
          third
        </label>
      </li>
    </ul>
    |}];
  Handle.click_on handle ~get_vdom ~selector:"li:nth-child(1) input";
  Handle.recompute_view handle;
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
          <label>
    -|      <input type="checkbox" #checked="false" onclick> </input>
    +|      <input type="checkbox" #checked="true" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    -|      <input type="checkbox" #checked="false" onclick> </input>
    +|      <input type="checkbox" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul>
    |}];
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
          <label>
    -|      <input type="checkbox" #checked="true" onclick> </input>
    +|      <input type="checkbox" #checked="false" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "setting into a set checklist" =
  let component =
    Form.Elements.Checkbox.set
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
        <label>
          <input type="checkbox" #checked="false" onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" onclick> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" onclick> </input>
          third
        </label>
      </li>
    </ul>
    |}];
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
          <label>
    -|      <input type="checkbox" #checked="false" onclick> </input>
    +|      <input type="checkbox" #checked="true" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    -|      <input type="checkbox" #checked="false" onclick> </input>
    +|      <input type="checkbox" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul>
    |}];
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
          <label>
    -|      <input type="checkbox" #checked="true" onclick> </input>
    +|      <input type="checkbox" #checked="false" onclick> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="true" onclick> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
            third
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "form of nested record of int and float" =
  let module T = struct
    module Nested = struct
      type t =
        { age : int
        ; height : float
        }
      [@@deriving fields ~iterators:make_creator, sexp]

      let form =
        let%sub age = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
        let%sub height =
          Form.Elements.Textbox.float ~allow_updates_when_focused:`Never ()
        in
        Form.Dynamic.Record_builder.(
          build_for_record (Fields.make_creator ~age:(field age) ~height:(field height)))
      ;;
    end

    type t =
      { unit : unit
      ; nested : Nested.t
      }
    [@@deriving fields ~iterators:make_creator, sexp]

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
      ("in field age" "Expected an integer")
      ("in field height" "Expected a floating point number")))

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
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
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=123
             oninput> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=123.456
             oninput> </input>
    </div>
    |}]
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

            let label_for_field = `Inferred

            let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
              = function
              | Age -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
              | Height ->
                Form.Elements.Textbox.float ~allow_updates_when_focused:`Never ()
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

          let label_for_field = `Inferred

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
      ("in field age" "Expected an integer")
      ("in field height" "Expected a floating point number")))

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
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
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=123
             oninput> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=123.456
             oninput> </input>
    </div>
    |}];
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
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=123
    +|         value:normalized=20
               oninput> </input>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=123.456
    +|         value:normalized=1.230
               oninput> </input>
      </div>
    |}]
;;

let%expect_test "typed records labelling overrides defaults" =
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

            let label_for_field = `Inferred

            let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
              = function
              | Age -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
              | Height ->
                Form.Elements.Textbox.float ~allow_updates_when_focused:`Never ()
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

          let field_to_string : type a. a Typed_field.t -> string = function
            | Unit -> "My awesome unit"
            | Nested -> "A complex nested record"
          ;;

          let label_for_field = `Computed field_to_string

          let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
            = function
            | Unit -> Bonsai.const (Form.return ())
            | Nested -> Nested.form
          ;;
        end)
    ;;
  end
  in
  let handle =
    Handle.create
      (form_result_spec
         ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
         ~get_vdom:get_vdom_verbose
         [%sexp_of: T.t])
      T.form
  in
  Handle.show handle;
  [%expect
    {|
    (Error (
      "in field A complex nested record"
      ("in field age" "Expected an integer")
      ("in field height" "Expected a floating point number")))

    ==============
    <table>
      <tbody>
        <tr>
          <td>
            <label> A complex nested record </label>
          </td>
          <td>
            <div>
              <div>
                <label>
                  <input> </input>
                  <span> ⚠ </span>
                  <div>
                    <div>
                      <div>
                        <strong> in field age </strong>
                      </div>
                      <div> Expected an integer </div>
                      <div>
                        <strong> in field height </strong>
                      </div>
                      <div> Expected a floating point number </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
        <tr>
          <td>
            <label> age </label>
          </td>
          <td>
            <input> </input>
          </td>
          <td>
            <div>
              <div>
                <label>
                  <input> </input>
                  <span> ⚠ </span>
                  <div>
                    <div>
                      <div> Expected an integer </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
        <tr>
          <td>
            <label> height </label>
          </td>
          <td>
            <input> </input>
          </td>
          <td>
            <div>
              <div>
                <label>
                  <input> </input>
                  <span> ⚠ </span>
                  <div>
                    <div>
                      <div> Expected a floating point number </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "typed records: dynamic labelling" =
  let module T = struct
    type t = { int : int } [@@deriving typed_fields, sexp]

    let form (int_label : string Value.t) =
      Form.Typed.Record.make
        (module struct
          module Typed_field = Typed_field

          let label_for_field =
            let%map int_label = int_label in
            fun ({ f = T field } : Typed_field.Packed.t) ->
              match field with
              | Int -> int_label
          ;;

          let label_for_field = `Dynamic label_for_field

          let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
            = function
            | Int ->
              Form.Elements.Number.int
                ~step:1
                ~default:0
                ~allow_updates_when_focused:`Never
                ()
          ;;
        end)
    ;;
  end
  in
  let label = Bonsai.Var.create "initial label" in
  let handle =
    Handle.create
      (form_result_spec
         ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
         ~get_vdom:get_vdom_verbose
         [%sexp_of: T.t])
      (T.form (Bonsai.Var.value label))
  in
  Handle.show handle;
  [%expect
    {|
    (Ok ((int 0)))

    ==============
    <table>
      <tbody>
        <tr>
          <td>
            <label> initial label </label>
          </td>
          <td>
            <input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Bonsai.Var.set label "updated label";
  Handle.show_diff handle;
  [%expect
    {|
      (Ok ((int 0)))

      ==============
      <table>
        <tbody>
          <tr>
            <td>
    -|        <label> initial label </label>
    +|        <label> updated label </label>
            </td>
            <td>
              <input> </input>
            </td>
          </tr>
        </tbody>
      </table>
    |}]
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

          let label_for_variant = `Inferred
          let initial_choice = `First_constructor

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | Nil -> Bonsai.const (Form.return ())
            | Cons ->
              let%map.Computation int =
                Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
              and me = (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (form ())) in
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
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true"> nil </option>
      <option value="1" #selected="false"> cons </option>
    </select>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"select" ~text:"1";
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false"> nil </option>
        <option value="1" #selected="true"> cons </option>
      </select>
      <input @key=bonsai_path_replaced_in_test
             type="text"
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
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok (Cons 123 Nil))

      ==============
      <div>
        <select id="bonsai_path_replaced_in_test"
                class="widget-dropdown"
                onchange
                style={
                  width: 100.00%;
                }>
          <option value="0" #selected="false"> nil </option>
          <option value="1" #selected="true"> cons </option>
        </select>
        <input @key=bonsai_path_replaced_in_test
               type="text"
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
      </div>
    |}]
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

          let label_for_variant = `Inferred
          let initial_choice = `First_constructor

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | Unit -> Bonsai.const (Form.return ())
            | Integer -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
            | Text -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
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
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true"> unit </option>
      <option value="1" #selected="false"> integer </option>
      <option value="2" #selected="false"> text </option>
    </select>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"select" ~text:"1";
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false"> unit </option>
        <option value="1" #selected="true"> integer </option>
        <option value="2" #selected="false"> text </option>
      </select>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok (Integer 123))

      ==============
      <div>
        <select id="bonsai_path_replaced_in_test"
                class="widget-dropdown"
                onchange
                style={
                  width: 100.00%;
                }>
          <option value="0" #selected="false"> unit </option>
          <option value="1" #selected="true"> integer </option>
          <option value="2" #selected="false"> text </option>
        </select>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized=123
               oninput> </input>
      </div>
    |}];
  Handle.do_actions handle [ Unit ];
  Handle.show handle;
  [%expect
    {|
    (Ok Unit)

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true"> unit </option>
      <option value="1" #selected="false"> integer </option>
      <option value="2" #selected="false"> text </option>
    </select>
    |}];
  Handle.do_actions handle [ Text "abc" ];
  Handle.show handle;
  [%expect
    {|
    (Ok (Text abc))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false"> unit </option>
        <option value="1" #selected="false"> integer </option>
        <option value="2" #selected="true"> text </option>
      </select>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=abc
             oninput> </input>
    </div>
    |}]
;;

let%expect_test "typed variants: dynamic labelling" =
  let module T = struct
    type t = Integer of int [@@deriving typed_variants, sexp]

    let form int_label =
      Form.Typed.Variant.make
        (module struct
          module Typed_variant = Typed_variant

          let label_for_variant =
            let%map int_label = int_label in
            fun ({ f = T variant } : Typed_variant.Packed.t) ->
              match variant with
              | Integer -> int_label
          ;;

          let label_for_variant = `Dynamic label_for_variant
          let initial_choice = `First_constructor

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | Integer ->
              Form.Elements.Number.int
                ~default:0
                ~step:1
                ~allow_updates_when_focused:`Never
                ()
          ;;
        end)
    ;;
  end
  in
  let label = Bonsai.Var.create "initial label" in
  let handle =
    Handle.create (form_result_spec [%sexp_of: T.t]) (T.form (Bonsai.Var.value label))
  in
  Handle.show handle;
  [%expect
    {|
    (Ok (Integer 0))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="true"> initial label </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=0
             oninput> </input>
    </div>
    |}];
  Bonsai.Var.set label "updated label";
  Handle.show_diff handle;
  [%expect
    {|
      (Ok (Integer 0))

      ==============
      <div>
        <select id="bonsai_path_replaced_in_test"
                class="widget-dropdown"
                onchange
                style={
                  width: 100.00%;
                }>
    -|    <option value="0" #selected="true"> initial label </option>
    +|    <option value="0" #selected="true"> updated label </option>
        </select>
        <input type="number"
               step="1"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
               value:normalized=0
               oninput> </input>
      </div>
    |}]
;;

let%expect_test "typed optional variants" =
  let module Nested = struct
    type t =
      | Unit
      | Integer of int
      | Text of string
    [@@deriving typed_variants, sexp]
  end
  in
  let module T = struct
    type t = Nested.t option [@@deriving sexp]

    let form =
      Form.Typed.Variant.make_optional
        (module struct
          module Typed_variant = Nested.Typed_variant

          let label_for_variant = `Inferred
          let initial_choice = `First_constructor

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | Unit -> Bonsai.const (Form.return ())
            | Integer -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
            | Text -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true"> (none) </option>
      <option value="1" #selected="false"> unit </option>
      <option value="2" #selected="false"> integer </option>
      <option value="3" #selected="false"> text </option>
    </select>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"select" ~text:"2";
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false"> (none) </option>
        <option value="1" #selected="false"> unit </option>
        <option value="2" #selected="true"> integer </option>
        <option value="3" #selected="false"> text </option>
      </select>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok ((Integer 123)))

      ==============
      <div>
        <select id="bonsai_path_replaced_in_test"
                class="widget-dropdown"
                onchange
                style={
                  width: 100.00%;
                }>
          <option value="0" #selected="false"> (none) </option>
          <option value="1" #selected="false"> unit </option>
          <option value="2" #selected="true"> integer </option>
          <option value="3" #selected="false"> text </option>
        </select>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
               id="bonsai_path_replaced_in_test"
    -|         value:normalized=""
    +|         value:normalized=123
               oninput> </input>
      </div>
    |}];
  Handle.do_actions handle [ Some Unit ];
  Handle.show handle;
  [%expect
    {|
    (Ok (Unit))

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false"> (none) </option>
      <option value="1" #selected="true"> unit </option>
      <option value="2" #selected="false"> integer </option>
      <option value="3" #selected="false"> text </option>
    </select>
    |}];
  Handle.do_actions handle [ Some (Text "abc") ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((Text abc)))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              onchange
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false"> (none) </option>
        <option value="1" #selected="false"> unit </option>
        <option value="2" #selected="false"> integer </option>
        <option value="3" #selected="true"> text </option>
      </select>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=abc
             oninput> </input>
    </div>
    |}];
  Handle.do_actions handle [ None ];
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true"> (none) </option>
      <option value="1" #selected="false"> unit </option>
      <option value="2" #selected="false"> integer </option>
      <option value="3" #selected="false"> text </option>
    </select>
    |}]
;;

let%expect_test "typed variants with custom labels" =
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

          let variant_to_string : type a. a Typed_variant.t -> string = function
            | Unit -> "My custom Unit"
            | Integer -> "This doesn't even mention integer"
            | Text -> "Text, please!"
          ;;

          let label_for_variant = `Computed variant_to_string
          let initial_choice = `First_constructor

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | Unit -> Bonsai.const (Form.return ())
            | Integer -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
            | Text -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
          ;;
        end)
    ;;
  end
  in
  let handle =
    Handle.create
      (form_result_spec
         ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
         [%sexp_of: T.t])
      T.form
  in
  Handle.show handle;
  [%expect
    {|
    (Ok Unit)

    ==============
    <select>
      <option> My custom Unit </option>
      <option> This doesn't even mention integer </option>
      <option> Text, please! </option>
    </select>
    |}]
;;

let%expect_test "typed variants: attr is applied to dropdown" =
  let module T = struct
    type t =
      | Unit
      | Integer of int
      | Text of string
    [@@deriving typed_variants, sexp]

    let form =
      Form.Typed.Variant.make
        ~picker_attr:(Value.return (Vdom.Attr.class_ "foo"))
        (module struct
          module Typed_variant = Typed_variant

          let label_for_variant = `Inferred
          let initial_choice = `First_constructor

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | Unit -> Bonsai.const (Form.return ())
            | Integer -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
            | Text -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
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
    <select id="bonsai_path_replaced_in_test"
            class="foo widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true"> unit </option>
      <option value="1" #selected="false"> integer </option>
      <option value="2" #selected="false"> text </option>
    </select>
    |}]
;;

let%expect_test "typed variants: radio vertical" =
  let module T = struct
    type t =
      | Unit
      | Integer of int
      | Text of string
    [@@deriving typed_variants, sexp]

    let form =
      Form.Typed.Variant.make
        ~picker:(`Radio `Vertical)
        ~picker_attr:(Value.return (Vdom.Attr.class_ "foo"))
        (module struct
          module Typed_variant = Typed_variant

          let label_for_variant = `Inferred
          let initial_choice = `Empty

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | Unit -> Bonsai.const (Form.return ())
            | Integer -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
            | Text -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <ul id="bonsai_path_replaced_in_test"
        class="foo radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          unit
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          integer
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          text
        </label>
      </li>
    </ul>
    |}]
;;

let%expect_test "typed variants: radio horizontal" =
  let module T = struct
    type t =
      | Unit
      | Integer of int
      | Text of string
    [@@deriving typed_variants, sexp]

    let form =
      Form.Typed.Variant.make
        ~picker:(`Radio `Horizontal)
        ~picker_attr:(Value.return (Vdom.Attr.class_ "foo"))
        (module struct
          module Typed_variant = Typed_variant

          let label_for_variant = `Inferred
          let initial_choice = `Empty

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | Unit -> Bonsai.const (Form.return ())
            | Integer -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
            | Text -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <ul id="bonsai_path_replaced_in_test"
        class="foo radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: inline-block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          unit
        </label>
      </li>
      <li style={ display: inline-block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          integer
        </label>
      </li>
      <li style={ display: inline-block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 onclick> </input>
          text
        </label>
      </li>
    </ul>
    |}]
;;

let%expect_test "typed variants: dropdown with initially empty picker" =
  let module T = struct
    type t = String of string [@@deriving typed_variants, sexp]

    let form =
      Form.Typed.Variant.make
        ~picker:`Dropdown
        ~picker_attr:(Value.return (Vdom.Attr.class_ "foo"))
        (module struct
          module Typed_variant = Typed_variant

          let label_for_variant = `Inferred
          let initial_choice = `Empty

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t =
            fun String ->
            Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="foo widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> string </option>
    </select>
    |}]
;;

let%expect_test "typed variants: dropdown with example default" =
  let module T = struct
    type t =
      | String of string
      | Other_thing
    [@@deriving typed_variants, sexp]

    let form =
      Form.Typed.Variant.make
        ~picker:`Dropdown
        (module struct
          module Typed_variant = Typed_variant

          let label_for_variant = `Inferred
          let initial_choice = `This Typed_variant.Packed.{ f = T Other_thing }

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | String -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
            | Other_thing -> Bonsai.const (Form.return ())
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Ok Other_thing)

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false"> string </option>
      <option value="1" #selected="true"> other thing </option>
    </select>
    |}]
;;

let%expect_test "optional typed-variant-form: dropdown with example default" =
  let module T = struct
    type t =
      | String of string
      | Other_thing
    [@@deriving typed_variants, sexp]

    let form =
      Form.Typed.Variant.make_optional
        ~picker:`Dropdown
        (module struct
          module Typed_variant = Typed_variant

          let label_for_variant = `Inferred
          let initial_choice = `This Typed_variant.Packed.{ f = T Other_thing }

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
            = function
            | String -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
            | Other_thing -> Bonsai.const (Form.return ())
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t option]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Ok (Other_thing))

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            onchange
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false"> (none) </option>
      <option value="1" #selected="false"> string </option>
      <option value="2" #selected="true"> other_thing </option>
    </select>
    |}]
;;

let%expect_test "file picker single" =
  let component =
    Form.Elements.File_select.single_opt
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
      <input type="file" accept=".txt,.png,image/jpeg" id="bonsai_path_replaced_in_test" oninput> </input>
    |}]
;;

let%expect_test "file picker list" =
  let component =
    Form.Elements.File_select.multiple ~accept:[ `Extension ".doc"; `Extension "docx" ] ()
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
      <input type="file" accept=".doc,.docx" multiple="" id="bonsai_path_replaced_in_test" oninput> </input>
    |}]
;;

let%expect_test "on_change handler should fire when input is changed" =
  let component =
    let%sub input = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
    let%sub () =
      Form.Dynamic.on_change
        ~sexp_of_model:[%sexp_of: String.t]
        ~equal:[%equal: String.t]
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
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           oninput> </input>

    ("the input changed to" (new_value ""))
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             oninput> </input>
    ("the input changed to" (new_value "hello world"))
    |}]
;;

let submit_test_attrs = function
  | "onsubmit" | "onclick" | "disabled" | "value:normalized" -> true
  | _ -> false
;;

let%expect_test "form with both submit button and form on-submit" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:(Form.Submit.create () ~f:(fun s -> Ui_effect.print_s (Sexp.Atom s)))
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:(fun ~key ~data:_ -> submit_test_attrs key)
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
          </tr>
          <tr>
            <td>
              <button onclick> submit </button>
            </td>
          </tr>
        </tbody>
      </table>
    </form>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.recompute_view handle;
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.recompute_view handle;
  [%expect {| "hello world" |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello there";
  Handle.recompute_view handle;
  Handle.submit_form handle ~get_vdom ~selector:"form";
  [%expect {| "hello there" |}]
;;

let%expect_test "form with on-submit with custom attrs" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create ~button_attr:(Vdom.Attr.class_ "my-class") () ~f:(fun s ->
           Ui_effect.print_s (Sexp.Atom s)))
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           submit_test_attrs key || String.equal key "class")
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
          </tr>
          <tr>
            <td>
              <button class="my-class" onclick> submit </button>
            </td>
          </tr>
        </tbody>
      </table>
    </form>
    |}]
;;

let%expect_test "form with on-submit with custom attrs and invalid form" =
  let component = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create ~button_attr:(Vdom.Attr.class_ "my-class") () ~f:(fun s ->
           Ui_effect.print_s ([%sexp_of: int] s)))
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: int]
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           submit_test_attrs key || String.equal key "class")
         ~get_vdom)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <form onsubmit>
      <table>
        <tbody>
          <tr>
            <td>  </td>
            <td>
              <input value:normalized=""> </input>
            </td>
          </tr>
          <tr>
            <td>
              <button disabled="" class="my-class"> submit </button>
            </td>
          </tr>
        </tbody>
      </table>
    </form>
    |}]
;;

let%expect_test "both button and on-submit are disabled when the form doesn't validate" =
  let component = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
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
         ~filter_printed_attributes:(fun ~key ~data:_ -> submit_test_attrs key)
         ~get_vdom)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <form onsubmit>
      <table>
        <tbody>
          <tr>
            <td>  </td>
            <td>
              <input value:normalized=""> </input>
            </td>
          </tr>
          <tr>
            <td>
              <button disabled=""> submit </button>
            </td>
          </tr>
        </tbody>
      </table>
    </form>
    |}];
  (* This form submission does nothing.  Well, it does prevent-default and
     stop-propagation, but the callback certainly doesn't fire. *)
  Handle.submit_form handle ~get_vdom ~selector:"form";
  Handle.recompute_view handle;
  [%expect {| |}];
  (* Once we put a valid number in, the form will be clickable, and
     submittable *)
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"123";
  Handle.recompute_view handle;
  Handle.submit_form handle ~get_vdom ~selector:"form";
  Handle.recompute_view handle;
  [%expect {| 123 |}];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
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
            </tr>
            <tr>
              <td>
    -|          <button disabled=""> submit </button>
    +|          <button onclick> submit </button>
              </td>
            </tr>
          </tbody>
        </table>
      </form>
    |}]
;;

let%expect_test "form with just button" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
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
         ~filter_printed_attributes:(fun ~key ~data:_ -> submit_test_attrs key)
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
        </tr>
        <tr>
          <td>
            <button onclick> submit </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello world";
  Handle.recompute_view handle;
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.recompute_view handle;
  [%expect {| "hello world" |}]
;;

let%expect_test "form with just enter" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create () ~f:(fun s -> Ui_effect.print_s (Sexp.Atom s)) ~button:None)
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:(fun ~key ~data:_ -> submit_test_attrs key)
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
          </tr>
        </tbody>
      </table>
      <input> </input>
    </form>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"hello there";
  Handle.recompute_view handle;
  Handle.submit_form handle ~get_vdom ~selector:"form";
  [%expect {| "hello there" |}]
;;

let%expect_test "form projected witha an effect" =
  let module Q = Effect.For_testing.Query_response_tracker in
  let tracker = Q.create () in
  let print_queued () = print_s [%sexp (Q.queries_pending_response tracker : int list)] in
  let not_positive = Error (Error.of_string "not positive") in
  let parse =
    tracker |> Effect.For_testing.of_query_response_tracker |> Bonsai.Value.return
  in
  let component =
    let%sub textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    Form.Dynamic.project_via_effect
      ~sexp_of_input:[%sexp_of: int]
      ~sexp_of_result:[%sexp_of: string]
      ~equal_input:[%equal: int]
      ~equal_result:[%equal: string]
      textbox
      ~parse
      ~unparse:Int.of_string
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
         ~get_vdom)
      component
  in
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (2) |}];
  Q.maybe_respond tracker ~f:(fun i ->
    Respond (if Int.is_positive i then Ok (Int.to_string i) else not_positive));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Ok 2)

    ==============
    <input> </input>
    |}];
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
    <input> </input>
    |}];
  Q.maybe_respond tracker ~f:(function
    | 5 -> Respond (Ok "5")
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  Q.maybe_respond tracker ~f:(function
    | -3 -> Respond not_positive
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error "not positive")

    ==============
    <input> </input>
    |}]
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
    let%sub textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    Form.Dynamic.validate_via_effect
      ~sexp_of_model:[%sexp_of: Int.t]
      textbox
      ~f
      ~equal:[%equal: Int.t]
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: int]
         ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
         ~get_vdom)
      component
  in
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (2) |}];
  Q.maybe_respond tracker ~f:(fun i ->
    Respond (if Int.is_positive i then Ok () else not_positive));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Ok 2)

    ==============
    <input> </input>
    |}];
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
    <input> </input>
    |}];
  Q.maybe_respond tracker ~f:(function
    | 5 -> Respond (Ok ())
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  Q.maybe_respond tracker ~f:(function
    | -3 -> Respond not_positive
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error "not positive")

    ==============
    <input> </input>
    |}]
;;

let%expect_test "form validated with an effect with one_at_at_time" =
  let module Q = Effect.For_testing.Query_response_tracker in
  let tracker = Q.create () in
  let print_queued () = print_s [%sexp (Q.queries_pending_response tracker : int list)] in
  let not_positive = Error (Error.of_string "not positive") in
  let f =
    tracker |> Effect.For_testing.of_query_response_tracker |> Bonsai.Value.return
  in
  let component =
    let%sub textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    Form.Dynamic.validate_via_effect
      ~sexp_of_model:[%sexp_of: Int.t]
      textbox
      ~f
      ~one_at_a_time:true
      ~equal:[%equal: Int.t]
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: int]
         ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
         ~get_vdom)
      component
  in
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (2) |}];
  Q.maybe_respond tracker ~f:(fun i ->
    Respond (if Int.is_positive i then Ok () else not_positive));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Ok 2)

    ==============
    <input> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"5";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"20";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"-3";
  Handle.recompute_view_until_stable handle;
  print_queued ();
  [%expect {| (5) |}];
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  Q.maybe_respond tracker ~f:(function
    | 5 -> Respond (Ok ())
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (-3) |}];
  Q.maybe_respond tracker ~f:(function
    | -3 -> Respond not_positive
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error "not positive")

    ==============
    <input> </input>
    |}]
;;

let%expect_test "form validated with an effect and debounced" =
  let module Q = Effect.For_testing.Query_response_tracker in
  let tracker = Q.create () in
  let print_queued () = print_s [%sexp (Q.queries_pending_response tracker : int list)] in
  let not_positive = Error (Error.of_string "not positive") in
  let f =
    tracker |> Effect.For_testing.of_query_response_tracker |> Bonsai.Value.return
  in
  let component =
    let%sub textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    Form.Dynamic.validate_via_effect
      ~debounce_ui:(Time_ns.Span.of_sec 1.0)
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      textbox
      ~f
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: int]
         ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
         ~get_vdom)
      component
  in
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (2) |}];
  Q.maybe_respond tracker ~f:(fun i ->
    Respond (if Int.is_positive i then Ok () else not_positive));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  (* this should be Ok, but it isn't due to the debounce *)
  [%expect {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 2.0);
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  (* now it's good! *)
  [%expect {|
    (Ok 2)

    ==============
    <input> </input>
    |}]
;;

let%expect_test "slider input" =
  let component =
    Form.Elements.Range.int
      ~min:0
      ~max:100
      ~default:0
      ~step:1
      ~allow_updates_when_focused:`Never
      ()
  in
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
         ~filter_printed_attributes:(fun ~key ~data:_ -> submit_test_attrs key)
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
          </tr>
        </tbody>
      </table>
      <input> </input>
    </form>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"20";
  Handle.recompute_view handle;
  Handle.submit_form handle ~get_vdom ~selector:"form";
  [%expect {| 20 |}]
;;

let%expect_test "query box" =
  let var = Bonsai.Var.create (String.Map.of_alist_exn [ "abc", "abc"; "def", "def" ]) in
  let value = Bonsai.Var.value var in
  let component =
    Form.Elements.Query_box.create
      (module String)
      ~selection_to_string:(Value.return Fn.id)
      ~f:(fun query ->
        let%arr query = query
        and value = value in
        Map.filter_map value ~f:(fun data ->
          if String.is_prefix ~prefix:query data then Some (Vdom.Node.text data) else None))
      ()
  in
  let get_vdom =
    get_vdom_verbose
      ~on_submit:
        (Form.Submit.create () ~f:(fun x -> Ui_effect.print_s (Sexp.Atom x)) ~button:None)
  in
  let handle =
    Handle.create
      (form_result_spec
         [%sexp_of: string]
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           match key with
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
                <input> </input>
                <div>
                  <div> </div>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <input> </input>
    </form>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"a";
  Handle.recompute_view handle;
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
                  <input> </input>
                  <div>
                    <div> </div>
                  </div>
                </div>
              </td>
            </tr>
    |}];
  Bonsai.Var.update var ~f:(fun map -> Map.remove (map : _ String.Map.t) "abc");
  Handle.show_diff handle;
  [%expect {| |}]
;;

let%expect_test "add tooltip to form" =
  let component =
    let%map.Computation x =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
    in
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
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=""
                   oninput> </input>
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
                       }> tooltip </div>
                </label>
              </div>

            </div>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "Bonsai_form.Typed sets groups/labels correctly on nested records" =
  let module A = struct
    let checkbox = Form.Elements.Checkbox.bool ~default:false ()

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

            let label_for_field = `Inferred

            let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
              = function
              | B_1 -> checkbox
              | B_2 -> checkbox
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

          let label_for_field = `Inferred

          let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
            = function
            | A_1 -> checkbox
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
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           match key with
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
          <td style={ padding-left: 1em; }>
            <label> a_1 </label>
          </td>
          <td>
            <input> </input>
          </td>
        </tr>
        <tr>
          <td style={ padding-left: 1em; }>
            <label> a_2 </label>
          </td>
        </tr>
        <tr>
          <td style={ padding-left: 2em; }>
            <label> b_1 </label>
          </td>
          <td>
            <input> </input>
          </td>
        </tr>
        <tr>
          <td style={ padding-left: 2em; }>
            <label> b_2 </label>
          </td>
          <td>
            <input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "view_as_vdom editable:`Currently_yes" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
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
              <input @key=bonsai_path_replaced_in_test
                     type="text"
                     placeholder=""
                     spellcheck="false"
                     id="bonsai_path_replaced_in_test"
                     value:normalized=""
                     oninput> </input>
            </td>
          </tr>
        </tbody>
      </table>
    </fieldset>
    |}]
;;

let%expect_test "view_as_vdom editable:`Currently_no" =
  (* We can't programatically test when inputs aren't editable, because they can still be
     set using javascript events like onclick, so just validate that there is a disabled
     fieldset wrapping the textbox *)
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
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
              <input @key=bonsai_path_replaced_in_test
                     type="text"
                     placeholder=""
                     spellcheck="false"
                     id="bonsai_path_replaced_in_test"
                     value:normalized=""
                     oninput> </input>
            </td>
          </tr>
        </tbody>
      </table>
    </fieldset>
    |}]
;;

let%expect_test "view_as_vdom not editable, with on_submit" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
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
                <input @key=bonsai_path_replaced_in_test
                       type="text"
                       placeholder=""
                       spellcheck="false"
                       id="bonsai_path_replaced_in_test"
                       value:normalized=""
                       oninput> </input>
              </td>
            </tr>
            <tr>
              <td colspan="3" style={ padding-left: 0em; }>
                <button onclick> submit </button>
              </td>
            </tr>
          </tbody>
        </table>
      </fieldset>
    </form>
    |}]
;;

let%expect_test "Adding error hints to the top level of various views" =
  let module View = Form.View in
  let print_view_with_error ~compute_view =
    let component =
      let%sub form =
        let%map.Computation form =
          Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
        in
        Form.Expert.create
          ~value:(Form.value form)
          ~set:(Form.set form)
          ~view:(compute_view (Form.view form))
      in
      Form.Dynamic.error_hint form
    in
    let handle =
      Handle.create
        (form_result_spec
           ~get_vdom:get_vdom_verbose
           ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
           [%sexp_of: int])
        component
    in
    Handle.show handle
  in
  (* The error should be inline in a plain row *)
  print_view_with_error ~compute_view:Fn.id;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <table>
      <tbody>
        <tr>
          <td>  </td>
          <td>
            <input> </input>
          </td>
          <td>
            <div>
              <div>
                <label>
                  <input> </input>
                  <span> ⚠ </span>
                  <div>
                    <div>
                      <div> Expected an integer </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  (* The error should be on the row above the list of fields for a record *)
  print_view_with_error ~compute_view:(fun view ->
    View.record [ { field_name = "field"; field_view = view } ]);
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <table>
      <tbody>
        <tr>
          <td>  </td>
          <td>
            <div>
              <div>
                <label>
                  <input> </input>
                  <span> ⚠ </span>
                  <div>
                    <div>
                      <div> Expected an integer </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
        <tr>
          <td>
            <label> field </label>
          </td>
          <td>
            <input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  (* The error should be placed inline with the selector for variants *)
  print_view_with_error ~compute_view:(fun view ->
    View.variant
      ~clause_selector:(Vdom.Node.text "I'm the selector!")
      ~selected_clause:(Some { View.clause_name = "clause1"; clause_view = view }));
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <table>
      <tbody>
        <tr>
          <td>  </td>
          <td> I'm the selector! </td>
          <td>
            <div>
              <div>
                <label>
                  <input> </input>
                  <span> ⚠ </span>
                  <div>
                    <div>
                      <div> Expected an integer </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
        <tr>
          <td>  </td>
          <td>
            <input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  (* The error should be inline with the toggle view in an option *)
  print_view_with_error ~compute_view:(fun view ->
    View.option ~toggle:(Vdom.Node.text "I'm the toggle!") ~status:(Currently_some view));
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <table>
      <tbody>
        <tr>
          <td>  </td>
          <td> I'm the toggle! </td>
          <td>
            <div>
              <div>
                <label>
                  <input> </input>
                  <span> ⚠ </span>
                  <div>
                    <div>
                      <div> Expected an integer </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
        <tr>
          <td>  </td>
          <td>
            <input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  print_view_with_error ~compute_view:(fun view ->
    View.collapsible ~label:(Vdom.Node.text "I'm the dropdown") ~state:(Expanded view));
  (* The error should be on the same row as the collapsible text *)
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <table>
      <tbody>
        <tr>
          <td>
            <label> I'm the dropdown </label>
          </td>
          <td>
            <div>
              <div>
                <label>
                  <input> </input>
                  <span> ⚠ </span>
                  <div>
                    <div>
                      <div> Expected an integer </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
        <tr>
          <td>  </td>
          <td>
            <input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "difference between with_default and with_default_always" =
  let default = Bonsai.Var.create 10 in
  let should_show_form = Bonsai.Var.create true in
  let create_handle ~name with_default =
    let component =
      match%sub Bonsai.Var.value should_show_form with
      | true ->
        let%sub form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
        with_default (Bonsai.Var.value default) form
      | false -> Bonsai.const (Form.return 0)
    in
    Handle.create
      (Result_spec.string
         (module struct
           type t = int Form.t

           let to_string t =
             let sexp = [%sexp_of: int Or_error.t] (Form.value t) in
             [%string "%{name}: %{sexp#Sexp}"]
           ;;
         end))
      component
  in
  let with_default_handle =
    create_handle ~name:"with_default" Form.Dynamic.with_default
  in
  let with_default_always_handle =
    create_handle ~name:"with_default_always" Form.Dynamic.with_default_always
  in
  let show_both () =
    Handle.show with_default_handle;
    Handle.show with_default_always_handle
  in
  show_both ();
  [%expect
    {|
    with_default: (Error"Expected an integer")
    with_default_always: (Error"Expected an integer")
    |}];
  show_both ();
  [%expect {|
    with_default: (Ok 10)
    with_default_always: (Ok 10)
    |}];
  Bonsai.Var.set should_show_form false;
  show_both ();
  [%expect {|
    with_default: (Ok 0)
    with_default_always: (Ok 0)
    |}];
  Bonsai.Var.set default 15;
  Bonsai.Var.set should_show_form true;
  show_both ();
  [%expect {|
    with_default: (Ok 10)
    with_default_always: (Ok 10)
    |}];
  show_both ();
  [%expect {|
    with_default: (Ok 10)
    with_default_always: (Ok 15)
    |}];
  Bonsai.Var.set default 20;
  show_both ();
  [%expect {|
    with_default: (Ok 10)
    with_default_always: (Ok 15)
    |}];
  (* Observe that neither version updates form to match the new default value
     of 20, since they both only update the default on activate or model reset. *)
  show_both ();
  [%expect {|
    with_default: (Ok 10)
    with_default_always: (Ok 15)
    |}]
;;

let%expect_test "[Form.with_default] sets the form value after a model reset" =
  let default = Bonsai.Var.create 0 in
  let component =
    Bonsai.with_model_resetter
      (let%sub form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
       Form.Dynamic.with_default (Bonsai.Var.value default) form)
  in
  let handle =
    Handle.create
      (module struct
        type t = int Form.t * unit Effect.t
        type incoming = unit

        let incoming (_, effect) () = effect

        let view (form, _) =
          Sexp.to_string_hum ([%sexp_of: int Or_error.t] (Form.value form))
        ;;
      end)
      component
  in
  Handle.show handle;
  [%expect {| (Error "Expected an integer") |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| (Error "Expected an integer") |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}]
;;

let%expect_test "[Form.with_default_always] sets the form value after a model reset" =
  let default = Bonsai.Var.create 0 in
  let component =
    Bonsai.with_model_resetter
      (let%sub form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
       Form.Dynamic.with_default_always (Bonsai.Var.value default) form)
  in
  let handle =
    Handle.create
      (module struct
        type t = int Form.t * unit Effect.t
        type incoming = unit

        let incoming (_, effect) () = effect

        let view (form, _) =
          Sexp.to_string_hum ([%sexp_of: int Or_error.t] (Form.value form))
        ;;
      end)
      component
  in
  Handle.show handle;
  [%expect {| (Error "Expected an integer") |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| (Error "Expected an integer") |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}]
;;

let%expect_test "[Form.with_default_always] only sets the form once on first activation" =
  let default = Bonsai.Var.create 0 in
  let component =
    let%sub form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    let%sub form_with_printing =
      let%arr form = form in
      Form.Expert.create ~view:(Form.view form) ~value:(Form.value form) ~set:(fun i ->
        let%bind.Effect () = Effect.print_s [%message "Form.set called"] in
        Form.set form i)
    in
    Form.Dynamic.with_default_always (Bonsai.Var.value default) form_with_printing
  in
  let handle =
    Handle.create
      (module struct
        type t = int Form.t
        type incoming = Nothing.t

        let incoming _ = Nothing.unreachable_code
        let view form = Sexp.to_string_hum ([%sexp_of: int Or_error.t] (Form.value form))
      end)
      component
  in
  Handle.show handle;
  [%expect {|
    (Error "Expected an integer")
    "Form.set called"
    |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}]
;;

let%expect_test {| [Form.with_default] interacts fine with [Handle.recompute_view_until_stable] |}
  =
  let default = Bonsai.Var.create 0 in
  let component =
    let%sub form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
    Form.Dynamic.with_default (Bonsai.Var.value default) form
  in
  let handle =
    Handle.create
      (module struct
        type t = int Form.t
        type incoming = Nothing.t

        let incoming _ = Nothing.unreachable_code
        let view form = Sexp.to_string_hum ([%sexp_of: int Or_error.t] (Form.value form))
      end)
      component
  in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {| (Ok 0) |}]
;;

let%expect_test "[Form.return] is not settable" =
  let component = Form.return 5 |> Bonsai.const in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect {|
    (Ok 5)

    ==============
    <div> </div>
    |}];
  Handle.do_actions handle [ 10 ];
  Handle.show handle;
  [%expect
    {|
    "Form.return was set, but setting is ignored."
    (Ok 5)

    ==============
    <div> </div>
    |}]
;;

let%expect_test "[Form.return] is not settable, but can log the attempted set if a \
                 [sexp_of] function is supplied"
  =
  let component = Form.return ~sexp_of_t:[%sexp_of: int] 5 |> Bonsai.const in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect {|
    (Ok 5)

    ==============
    <div> </div>
    |}];
  Handle.do_actions handle [ 10 ];
  Handle.show handle;
  [%expect
    {|
    ("Form.return was set, but setting is ignored." (set_value 5))
    (Ok 5)

    ==============
    <div> </div>
    |}]
;;

let%expect_test "Partially settable form via [Form.return] and [Form.return_settable]" =
  let module Record = struct
    type t =
      { form_return : int
      ; form_return_settable : int
      ; another_form_return : int
      }
    [@@deriving typed_fields, sexp_of]

    let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
      | Form_return -> Bonsai.const (Form.return ~sexp_of_t:[%sexp_of: int] 1)
      | Form_return_settable -> Form.return_settable ~equal:[%equal: int] 2
      | Another_form_return -> Bonsai.const (Form.return ~sexp_of_t:[%sexp_of: int] 3)
    ;;

    let label_for_field = `Inferred
  end
  in
  let component = Form.Typed.Record.make (module Record) in
  let handle = Handle.create (form_result_spec [%sexp_of: Record.t]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (form_return          1)
      (form_return_settable 2)
      (another_form_return  3)))

    ==============
    <div> </div>
    |}];
  Handle.do_actions
    handle
    [ { form_return = 10; form_return_settable = 20; another_form_return = 30 } ];
  Handle.show handle;
  [%expect
    {|
    ("Form.return was set, but setting is ignored." (set_value 1))
    ("Form.return was set, but setting is ignored." (set_value 3))
    (Ok (
      (form_return          1)
      (form_return_settable 20)
      (another_form_return  3)))

    ==============
    <div> </div>
    |}]
;;

let%expect_test "return_settable" =
  let component =
    Form.return_settable ~sexp_of_model:[%sexp_of: Int.t] 5 ~equal:[%equal: Int.t]
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect {|
    (Ok 5)

    ==============
    <div> </div>
    |}];
  Handle.do_actions handle [ 10 ];
  Handle.show handle;
  [%expect {|
    (Ok 10)

    ==============
    <div> </div>
    |}]
;;

let%expect_test "Checkbox.set layout options" =
  let print_handle layout =
    let component =
      Form.Elements.Checkbox.set
        (module String)
        ~layout
        (Value.return [ "first"; "second" ])
    in
    let handle = Handle.create (form_result_spec [%sexp_of: String.Set.t]) component in
    Handle.show handle
  in
  print_handle `Vertical;
  let print_diff = Expect_test_patdiff.diff_printer (Some [%expect.output]) in
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
        <label>
          <input type="checkbox" #checked="false" onclick> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" onclick> </input>
          second
        </label>
      </li>
    </ul>
    |}];
  print_handle `Horizontal;
  unstage print_diff [%expect.output];
  [%expect
    {|
    -1,23 +1,23
      (Ok ())

      ==============
      <ul id="bonsai_path_replaced_in_test"
          class="checkbox-container widget-checklist"
          style={
            list-style: none;
            margin-left: 0px;
          }>
    -|  <li style={ display: block; }>
    +|  <li style={ display: inline-block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
            first
          </label>
        </li>
    -|  <li style={ display: block; }>
    +|  <li style={ display: inline-block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
            second
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "typed variant forms with radio buttons can be initialized to the first \
                 item"
  =
  let module T = struct
    type t = String of string [@@deriving typed_variants, sexp]

    let form =
      Form.Typed.Variant.make
        ~picker:(`Radio `Vertical)
        ~picker_attr:(Value.return (Vdom.Attr.class_ "foo"))
        (module struct
          module Typed_variant = Typed_variant

          let label_for_variant = `Inferred
          let initial_choice = `First_constructor

          let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t =
            fun String ->
            Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
          ;;
        end)
    ;;
  end
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t]) T.form in
  Handle.show handle;
  [%expect
    {|
    (Ok (String ""))

    ==============
    <div>
      <ul id="bonsai_path_replaced_in_test"
          class="foo radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="true"
                   onclick> </input>
            string
          </label>
        </li>
      </ul>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             oninput> </input>
    </div>
    |}]
;;

let%expect_test "labelling a range form" =
  let label text = Some (Vdom.Node.text text) in
  let no_label = None in
  let all_options =
    [ no_label, no_label, "No labels"
    ; no_label, label "right", "Right label only"
    ; label "left", no_label, "Left label only"
    ; label "left", label "right", "Both sides labelled"
    ]
  in
  List.iter all_options ~f:(fun (left_label, right_label, description) ->
    print_endline description;
    print_endline "###############";
    let range =
      Form.Elements.Range.int
        ?left_label
        ?right_label
        ~default:0
        ~step:1
        ~allow_updates_when_focused:`Never
        ()
    in
    let handle = Handle.create (form_result_spec [%sexp_of: int]) range in
    Handle.show handle);
  [%expect
    {|
    No labels
    ###############
    (Ok 0)

    ==============
    <input type="range"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           min="0"
           max="100"
           value:normalized=0
           oninput> </input>

    Right label only
    ###############
    (Ok 0)

    ==============
    <span style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
      <input type="range"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="0"
             max="100"
             value:normalized=0
             oninput> </input>
      right
    </span>

    Left label only
    ###############
    (Ok 0)

    ==============
    <span style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
      left
      <input type="range"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="0"
             max="100"
             value:normalized=0
             oninput> </input>
    </span>

    Both sides labelled
    ###############
    (Ok 0)

    ==============
    <span style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
      left
      <input type="range"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             min="0"
             max="100"
             value:normalized=0
             oninput> </input>
      right
    </span>
    |}]
;;

let%test_module "Typed fields monomorphization" =
  (module struct
    module Record = struct
      type ('a, 'b, 'c) t =
        { a : 'a
        ; b : 'b
        ; c : 'c
        }
      [@@deriving sexp, typed_fields]

      let form =
        Form.Typed.Record.make
          (module struct
            module Typed_field =
              Typed_fields_lib.S_of_S3 (Typed_field) (Int) (String) (Float)

            let label_for_field = `Inferred

            let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
              = function
              | A -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
              | B -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
              | C -> Form.Elements.Textbox.float ~allow_updates_when_focused:`Never ()
            ;;
          end)
      ;;
    end

    let%expect_test "record monomorphization" =
      let handle =
        Handle.create
          (form_result_spec
             ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
             ~get_vdom:get_vdom_verbose
             [%sexp_of: (int, string, float) Record.t])
          Record.form
      in
      Handle.show handle;
      [%expect
        {|
        (Error (
          ("in field a" "Expected an integer")
          ("in field c" "Expected a floating point number")))

        ==============
        <table>
          <tbody>
            <tr>
              <td>
                <label> a </label>
              </td>
              <td>
                <input> </input>
              </td>
              <td>
                <div>
                  <div>
                    <label>
                      <input> </input>
                      <span> ⚠ </span>
                      <div>
                        <div>
                          <div> Expected an integer </div>
                        </div>
                      </div>
                    </label>
                  </div>
                </div>
              </td>
            </tr>
            <tr>
              <td>
                <label> b </label>
              </td>
              <td>
                <input> </input>
              </td>
            </tr>
            <tr>
              <td>
                <label> c </label>
              </td>
              <td>
                <input> </input>
              </td>
              <td>
                <div>
                  <div>
                    <label>
                      <input> </input>
                      <span> ⚠ </span>
                      <div>
                        <div>
                          <div> Expected a floating point number </div>
                        </div>
                      </div>
                    </label>
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        |}]
    ;;

    module Variant = struct
      type ('a, 'b, 'c) t =
        | A of 'a
        | B of 'b
        | C of 'c
      [@@deriving sexp, typed_variants]

      let form =
        Form.Typed.Variant.make
          (module struct
            module Typed_variant =
              Typed_variants_lib.S_of_S3 (Typed_variant) (Int) (String) (Float)

            let label_for_variant = `Inferred
            let initial_choice = `First_constructor

            let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
              = function
              | A -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
              | B -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
              | C -> Form.Elements.Textbox.float ~allow_updates_when_focused:`Never ()
            ;;
          end)
      ;;
    end

    let%expect_test "variant monomorphization" =
      let handle =
        Handle.create
          (form_result_spec
             ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
             ~get_vdom:get_vdom_verbose
             [%sexp_of: (int, string, float) Variant.t])
          Variant.form
      in
      Handle.show handle;
      [%expect
        {|
        (Error "Expected an integer")

        ==============
        <table>
          <tbody>
            <tr>
              <td>  </td>
              <td>
                <select>
                  <option> a </option>
                  <option> b </option>
                  <option> c </option>
                </select>
              </td>
            </tr>
            <tr>
              <td>  </td>
              <td>
                <input> </input>
              </td>
            </tr>
          </tbody>
        </table>
        |}]
    ;;
  end)
;;

let%test_module "Querybox as typeahead" =
  (module struct
    module Data = struct
      module T = struct
        type t =
          | Option_A
          | Option_B
          | Option_C
        [@@deriving variants, enumerate, sexp, equal, compare]
      end

      include T

      let to_string = function
        | Option_A -> "Option A"
        | Option_B -> "Option B"
        | Option_C -> "Option C"
      ;;

      include Comparable.Make (T)
    end

    let shared_computation ?(to_string = Value.return Data.to_string) () =
      Form.Elements.Query_box.single_opt
        (module Data)
        ~all_options:(Value.return Data.all)
        ~to_string
    ;;

    let view_computation ?to_string () =
      let%sub form = shared_computation ?to_string () in
      let%arr form = form in
      Form.view_as_vdom form
    ;;

    let view_and_inject_computation =
      let%sub form = shared_computation () in
      let%arr form = form in
      Form.view_as_vdom form, Form.set form
    ;;

    let%expect_test "Initial typeahead state" =
      let handle = Handle.create (Result_spec.vdom Fn.id) (view_computation ()) in
      Handle.show handle;
      [%expect
        {|
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
                <div id="bonsai_path_replaced_in_test">
                  <input id="bonsai_path_replaced_in_test"
                         type="text"
                         class="input_hash_replaced_in_test"
                         #value=""
                         onblur
                         onfocus
                         oninput
                         onkeydown> </input>
                  <div data-test="query-box-item-container"
                       id="bonsai_path_replaced_in_test"
                       tabindex="-1"
                       onblur
                       onwheel
                       style={
                         position: relative;
                       }>
                    <div> </div>
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        |}]
    ;;

    let%expect_test "Change typeahead contents" =
      let handle = Handle.create (Result_spec.vdom Fn.id) (view_computation ()) in
      Handle.store_view handle;
      Handle.input_text
        handle
        ~get_vdom:Fn.id
        ~selector:"input"
        ~text:(Data.to_string Data.Option_C);
      Handle.show_diff handle;
      [%expect
        {|
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
                  <div id="bonsai_path_replaced_in_test">
                    <input id="bonsai_path_replaced_in_test"
                           type="text"
                           class="input_hash_replaced_in_test"
        -|                 #value=""
        +|                 #value="Option C"
                           onblur
                           onfocus
                           oninput
                           onkeydown> </input>
                    <div data-test="query-box-item-container"
                         id="bonsai_path_replaced_in_test"
                         tabindex="-1"
                         onblur
                         onwheel
                         style={
                           position: relative;
                         }>
        -|            <div> </div>
        +|            <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
        +|              <div class="selected_item_hash_replaced_in_test" onclick onmouseenter>
        +|                <span> Option C </span>
        +|              </div>
        +|            </div>
                    </div>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
        |}]
    ;;

    let%expect_test "use setter" =
      let handle =
        Handle.create
          (module struct
            type incoming = Data.t option
            type t = Vdom.Node.t * (Data.t option -> unit Ui_effect.t)

            let view (vdom, _) =
              let module V = (val Result_spec.vdom Fn.id) in
              V.view vdom
            ;;

            let incoming (_, inject) = inject
          end)
          view_and_inject_computation
      in
      Handle.store_view handle;
      Handle.do_actions handle [ Some Data.Option_A ];
      Handle.show_diff handle;
      [%expect
        {|
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
                  <div id="bonsai_path_replaced_in_test">
                    <input id="bonsai_path_replaced_in_test"
                           type="text"
        +|                 placeholder="Option A"
                           class="input_hash_replaced_in_test"
                           #value=""
                           onblur
                           onfocus
                           oninput
                           onkeydown> </input>
                    <div data-test="query-box-item-container"
                         id="bonsai_path_replaced_in_test"
                         tabindex="-1"
                         onblur
                         onwheel
                         style={
                           position: relative;
                         }>
                      <div> </div>
                    </div>
        |}];
      Handle.do_actions handle [ None ];
      Handle.show_diff handle;
      [%expect
        {|
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
                  <div id="bonsai_path_replaced_in_test">
                    <input id="bonsai_path_replaced_in_test"
                           type="text"
        -|                 placeholder="Option A"
                           class="input_hash_replaced_in_test"
                           #value=""
                           onblur
                           onfocus
                           oninput
                           onkeydown> </input>
                    <div data-test="query-box-item-container"
                         id="bonsai_path_replaced_in_test"
                         tabindex="-1"
                         onblur
                         onwheel
                         style={
                           position: relative;
                         }>
                      <div> </div>
                    </div>
        |}]
    ;;

    let%expect_test "Select element using partial input" =
      let handle = Handle.create (Result_spec.vdom Fn.id) (view_computation ()) in
      Handle.show handle;
      [%expect
        {|
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
                <div id="bonsai_path_replaced_in_test">
                  <input id="bonsai_path_replaced_in_test"
                         type="text"
                         class="input_hash_replaced_in_test"
                         #value=""
                         onblur
                         onfocus
                         oninput
                         onkeydown> </input>
                  <div data-test="query-box-item-container"
                       id="bonsai_path_replaced_in_test"
                       tabindex="-1"
                       onblur
                       onwheel
                       style={
                         position: relative;
                       }>
                    <div> </div>
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        |}];
      (* "O" is not unique, all options are matched. *)
      Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"O";
      Handle.show_diff handle;
      [%expect
        {|
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
                  <div id="bonsai_path_replaced_in_test">
                    <input id="bonsai_path_replaced_in_test"
                           type="text"
                           class="input_hash_replaced_in_test"
        -|                 #value=""
        +|                 #value="O"
                           onblur
                           onfocus
                           oninput
                           onkeydown> </input>
                    <div data-test="query-box-item-container"
                         id="bonsai_path_replaced_in_test"
                         tabindex="-1"
                         onblur
                         onwheel
                         style={
                           position: relative;
                         }>
        -|            <div> </div>
        +|            <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
        +|              <div class="selected_item_hash_replaced_in_test" onclick onmouseenter>
        +|                <span> Option A </span>
        +|              </div>
        +|              <div onclick onmouseenter>
        +|                <span> Option B </span>
        +|              </div>
        +|              <div onclick onmouseenter>
        +|                <span> Option C </span>
        +|              </div>
        +|            </div>
                    </div>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
        |}];
      Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"C";
      Handle.show_diff handle;
      [%expect
        {|
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
                  <div id="bonsai_path_replaced_in_test">
                    <input id="bonsai_path_replaced_in_test"
                           type="text"
                           class="input_hash_replaced_in_test"
        -|                 #value="O"
        +|                 #value="C"
                           onblur
                           onfocus
                           oninput
                           onkeydown> </input>
                    <div data-test="query-box-item-container"
                         id="bonsai_path_replaced_in_test"
                         tabindex="-1"
                         onblur
                         onwheel
                         style={
                           position: relative;
                         }>
                      <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
                        <div class="selected_item_hash_replaced_in_test" onclick onmouseenter>
        -|                <span> Option A </span>
        -|              </div>
        -|              <div onclick onmouseenter>
        -|                <span> Option B </span>
        -|              </div>
        -|              <div onclick onmouseenter>
                          <span> Option C </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
        |}]
    ;;

    let%expect_test "dynamic [to_string]." =
      let to_string_var = Bonsai.Var.create Data.to_string in
      let to_string = Bonsai.Var.value to_string_var in
      let handle =
        Handle.create (Result_spec.vdom Fn.id) (view_computation ~to_string ())
      in
      Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"";
      Handle.store_view handle;
      Bonsai.Var.set to_string_var (fun data -> Data.to_string data ^ "!");
      Handle.show_diff handle;
      [%expect
        {|
                           class="input_hash_replaced_in_test"
                           #value=""
                           onblur
                           onfocus
                           oninput
                           onkeydown> </input>
                    <div data-test="query-box-item-container"
                         id="bonsai_path_replaced_in_test"
                         tabindex="-1"
                         onblur
                         onwheel
                         style={
                           position: relative;
                         }>
                      <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
                        <div class="selected_item_hash_replaced_in_test" onclick onmouseenter>
        -|                <span> Option A </span>
        +|                <span> Option A! </span>
                        </div>
                        <div onclick onmouseenter>
        -|                <span> Option B </span>
        +|                <span> Option B! </span>
                        </div>
                        <div onclick onmouseenter>
        -|                <span> Option C </span>
        +|                <span> Option C! </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
        |}]
    ;;

    let%expect_test "Handle unknown option" =
      let computation =
        let%sub form =
          Form.Elements.Query_box.single_opt
            (module Data)
            ~all_options:(Value.return Data.all)
            ~handle_unknown_option:
              (Value.return (fun _ ->
                 print_endline "in handle_unknown_option";
                 Some Data.Option_A))
        in
        let%arr form = form in
        Form.view_as_vdom form
      in
      let handle = Handle.create (Result_spec.vdom Fn.id) computation in
      Handle.store_view handle;
      [%expect {| |}];
      Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"unknown option";
      Handle.show_diff handle;
      [%expect
        {|
        in handle_unknown_option

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
                  <div id="bonsai_path_replaced_in_test">
                    <input id="bonsai_path_replaced_in_test"
                           type="text"
                           class="input_hash_replaced_in_test"
        -|                 #value=""
        +|                 #value="unknown option"
                           onblur
                           onfocus
                           oninput
                           onkeydown> </input>
                    <div data-test="query-box-item-container"
                         id="bonsai_path_replaced_in_test"
                         tabindex="-1"
                         onblur
                         onwheel
                         style={
                           position: relative;
                         }>
        -|            <div> </div>
        +|            <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
        +|              <div class="selected_item_hash_replaced_in_test" onclick onmouseenter>
        +|                <span> Option_A </span>
        +|              </div>
        +|            </div>
                    </div>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
        |}]
    ;;
  end)
;;

(* This test demonstrates efficiency in the [Form.Elements.Multiple.list] combinator,
   where we need to stabilize more than once per nesting level. *)
let%expect_test "nested list form actions" =
  let component =
    Form.Elements.Multiple.list
      (Form.Elements.Multiple.list
         (Form.Elements.Multiple.list
            (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())))
  in
  let handle =
    Handle.create (form_result_spec [%sexp_of: string list list list]) component
  in
  Handle.print_stabilizations handle;
  Handle.do_actions
    handle
    [ [ [ [ "first"; "second" ]; [ "third"; "fourth" ] ]
      ; [ [ "fifth"; "sixth" ]; [ "seventh"; "eighth" ] ]
      ]
    ];
  Handle.recompute_view handle;
  [%expect
    {|
    skipped stabilization
    skipped stabilization
    stabilized
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    stabilized
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    stabilized
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    skipped stabilization
    |}]
;;

(* The main difference between the Form2 version and this one is the addition of the error
   hints. So, we only test the error messages in this library *)
let%expect_test "Form.Typed.Record.make_table adds appropriate error messages" =
  let module T = struct
    type t =
      { a : int
      ; b : string
      }
    [@@deriving typed_fields, sexp]
  end
  in
  let component =
    Form.Typed.Record.make_table
      (module struct
        module Typed_field = T.Typed_field

        let label_for_field : type a. a Typed_field.t -> string = function
          | A -> "Alpha"
          | B -> "Bravo"
        ;;

        let label_for_field = `Computed label_for_field

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
          | A -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never ()
          | B -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ()
        ;;
      end)
  in
  let handle = Handle.create (form_result_spec [%sexp_of: T.t list]) component in
  (* Click the + button to add a field that fails to validate *)
  Handle.click_on handle ~get_vdom ~selector:"button";
  Handle.show handle;
  (* Note the existence of the error checkbox below *)
  [%expect
    {|
    (Error ("in field Alpha" "Expected an integer"))

    ==============
    <div style={ display: flex; flex-direction: column; }>
      <table class="table_hash_replaced_in_test"
             custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <thead class="header_hash_replaced_in_test">
          <tr class="header_row_hash_replaced_in_test">
            <th colspan="1" class="header_cell_hash_replaced_in_test"> Alpha </th>
            <th colspan="1" class="header_cell_hash_replaced_in_test"> Bravo </th>
            <th colspan="1" class="header_cell_hash_replaced_in_test"> Remove </th>
          </tr>
        </thead>
        <tbody class="body_hash_replaced_in_test">
          <tr class="body_row_hash_replaced_in_test">
            <td class="body_cell_hash_replaced_in_test">
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
                      <input @key=bonsai_path_replaced_in_test
                             type="text"
                             placeholder=""
                             spellcheck="false"
                             id="bonsai_path_replaced_in_test"
                             value:normalized=""
                             oninput> </input>
                    </td>
                    <td>
                      <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
                        <div class="container_hash_replaced_in_test">
                          <label class="label_hash_replaced_in_test" style={ color: #f54646; }>
                            <input type="checkbox"
                                   tabindex="-1"
                                   class="checkbox_hash_replaced_in_test"> </input>
                            <span class="span_hash_replaced_in_test"> ⚠ </span>
                            <div class="right_hash_replaced_in_test text_hash_replaced_in_test"
                                 style={
                                   border: 1px solid red;
                                   color: black;
                                   background-color: pink;
                                 }>
                              <div>
                                <div> Expected an integer </div>
                              </div>
                            </div>
                          </label>
                        </div>
                      </div>
                    </td>
                  </tr>
                </tbody>
              </table>
            </td>
            <td class="body_cell_hash_replaced_in_test">
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
                      <input @key=bonsai_path_replaced_in_test
                             type="text"
                             placeholder=""
                             spellcheck="false"
                             id="bonsai_path_replaced_in_test"
                             value:normalized=""
                             oninput> </input>
                    </td>
                  </tr>
                </tbody>
              </table>
            </td>
            <td class="body_cell_hash_replaced_in_test">
              <div style={ display: flex; justify-content: center; }>
                <button onclick> X </button>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <button onclick> + </button>
    </div>
    |}]
;;

let%expect_test "Typed variant set form" =
  let module M = struct
    module Foo = struct
      type t =
        | A
        | B
      [@@deriving sexp_of, compare, enumerate]
    end

    module Bar = struct
      type t =
        | C
        | D
        | E
      [@@deriving sexp_of, compare, enumerate]
    end

    module T = struct
      type t =
        | Foo of Foo.t
        | Bar of Bar.t
      [@@deriving sexp_of, compare, typed_variants]
    end

    include T

    let label_for_variant = `Inferred
    let sexp_of_variant_argument = `Use_sexp_of_variant

    let form_for_variant
      : type a cmp.
        a Typed_variant.t
        -> (a, cmp) Bonsai.comparator
        -> (a, cmp) Set.t Form.t Computation.t
      =
      fun variant comparator ->
      match variant with
      | Foo ->
        Form.Elements.Checkbox.set
          comparator
          ~extra_container_attrs:(Value.return [ Vdom.Attr.class_ "foo" ])
          (Value.return Foo.all)
      | Bar ->
        Form.Elements.Checkbox.set
          comparator
          ~extra_container_attrs:(Value.return [ Vdom.Attr.class_ "bar" ])
          (Value.return Bar.all)
    ;;

    include Comparable.Make_plain (T)
  end
  in
  let component = Form.Typed.Variant.make_set (module M) in
  let result_spec = form_result_spec [%sexp_of: M.Set.t] in
  let handle = Handle.create result_spec component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <div>
      <ul id="bonsai_path_replaced_in_test"
          class="checkbox-container foo widget-checklist"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
             foo a
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
             foo b
          </label>
        </li>
      </ul>
      <ul id="bonsai_path_replaced_in_test"
          class="bar checkbox-container widget-checklist"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
             bar c
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
             bar d
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" onclick> </input>
             bar e
          </label>
        </li>
      </ul>
    </div>
    |}];
  Handle.do_actions handle M.[ Set.of_list [ Foo A; Bar C; Bar E ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (
    +|  (Foo A)
    +|  (Bar C)
    +|  (Bar E)))

      ==============
      <div>
        <ul id="bonsai_path_replaced_in_test"
            class="checkbox-container foo widget-checklist"
            style={
              list-style: none;
              margin-left: 0px;
            }>
          <li style={ display: block; }>
            <label>
    -|        <input type="checkbox" #checked="false" onclick> </input>
    +|        <input type="checkbox" #checked="true" onclick> </input>
               foo a
            </label>
          </li>
          <li style={ display: block; }>
            <label>
              <input type="checkbox" #checked="false" onclick> </input>
               foo b
            </label>
          </li>
        </ul>
        <ul id="bonsai_path_replaced_in_test"
            class="bar checkbox-container widget-checklist"
            style={
              list-style: none;
              margin-left: 0px;
            }>
          <li style={ display: block; }>
            <label>
    -|        <input type="checkbox" #checked="false" onclick> </input>
    +|        <input type="checkbox" #checked="true" onclick> </input>
               bar c
            </label>
          </li>
          <li style={ display: block; }>
            <label>
              <input type="checkbox" #checked="false" onclick> </input>
               bar d
            </label>
          </li>
          <li style={ display: block; }>
            <label>
    -|        <input type="checkbox" #checked="false" onclick> </input>
    +|        <input type="checkbox" #checked="true" onclick> </input>
               bar e
            </label>
          </li>
        </ul>
      </div>
    |}];
  Handle.set_checkbox
    handle
    ~get_vdom:Form.view_as_vdom
    ~selector:".foo li:nth-child(2) input"
    ~checked:true;
  Handle.show_diff handle;
  [%expect
    {|
      (Ok (
        (Foo A)
    +|  (Foo B)
        (Bar C)
        (Bar E)))

      ==============
      <div>
        <ul id="bonsai_path_replaced_in_test"
            class="checkbox-container foo widget-checklist"
            style={
              list-style: none;
              margin-left: 0px;
            }>
          <li style={ display: block; }>
            <label>
              <input type="checkbox" #checked="true" onclick> </input>
               foo a
            </label>
          </li>
          <li style={ display: block; }>
            <label>
    -|        <input type="checkbox" #checked="false" onclick> </input>
    +|        <input type="checkbox" #checked="true" onclick> </input>
               foo b
            </label>
          </li>
        </ul>
        <ul id="bonsai_path_replaced_in_test"
            class="bar checkbox-container widget-checklist"
            style={
              list-style: none;
              margin-left: 0px;
            }>
          <li style={ display: block; }>
            <label>
              <input type="checkbox" #checked="true" onclick> </input>
               bar c
            </label>
          </li>
    |}];
  Handle.set_checkbox
    handle
    ~get_vdom:Form.view_as_vdom
    ~selector:".bar li:nth-child(1) input"
    ~checked:false;
  Handle.show_diff handle;
  [%expect
    {|
      (Ok (
        (Foo A)
        (Foo B)
    -|  (Bar C)
        (Bar E)))

      ==============
      <div>
        <ul id="bonsai_path_replaced_in_test"
            class="checkbox-container foo widget-checklist"
            style={
              list-style: none;
              margin-left: 0px;
            }>
          <li style={ display: block; }>
            <label>
              <input type="checkbox" #checked="true" onclick> </input>
               foo a
            </label>
          </li>
          <li style={ display: block; }>
            <label>
              <input type="checkbox" #checked="true" onclick> </input>
               foo b
            </label>
          </li>
        </ul>
        <ul id="bonsai_path_replaced_in_test"
            class="bar checkbox-container widget-checklist"
            style={
              list-style: none;
              margin-left: 0px;
            }>
          <li style={ display: block; }>
            <label>
    -|        <input type="checkbox" #checked="true" onclick> </input>
    +|        <input type="checkbox" #checked="false" onclick> </input>
               bar c
            </label>
          </li>
          <li style={ display: block; }>
            <label>
              <input type="checkbox" #checked="false" onclick> </input>
               bar d
            </label>
          </li>
          <li style={ display: block; }>
            <label>
              <input type="checkbox" #checked="true" onclick> </input>
               bar e
            </label>
          </li>
        </ul>
    |}]
;;
