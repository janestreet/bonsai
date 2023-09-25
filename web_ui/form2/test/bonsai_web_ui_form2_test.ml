open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form2_experimental

let form_result_spec (type a) ?filter_printed_attributes ?censor_paths sexp_of_a
  : ((a, Vdom.Node.t) Form.t, a) Result_spec.t
  =
  (module struct
    type t = (a, Vdom.Node.t) Form.t
    type incoming = a

    let view form =
      let module V = (val Result_spec.vdom ?filter_printed_attributes ?censor_paths Fn.id)
      in
      let vdom = Form.view form in
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
    test |}];
  Handle.do_actions handle [ "not test" ];
  Handle.show_diff handle
;;

let%expect_test "typing into a string textbox" =
  let component = Form.Elements.Textbox.string () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
    -|<input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|<input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input> |}]
;;

let%expect_test "typing into a string password textbox" =
  let component = Form.Elements.Password.string () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input type="password" placeholder="" spellcheck="false" value:normalized="" oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
    -|<input type="password" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|<input type="password" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input> |}]
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
    <select class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="false"> world </option>
    </select> |}];
  Handle.change handle ~get_vdom:Form.view ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok hello)

      ==============
      <select class="widget-dropdown" onchange style={ width: 100.00%; }>
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
    <select class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="false"> hello </option>
      <option value="1" #selected="true"> world </option>
    </select> |}];
  Handle.change handle ~get_vdom:Form.view ~selector:"select" ~value:"0";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok world)
    +|(Ok hello)

      ==============
      <select class="widget-dropdown" onchange style={ width: 100.00%; }>
    -|  <option value="0" #selected="false"> hello </option>
    +|  <option value="0" #selected="true"> hello </option>
    -|  <option value="1" #selected="true"> world </option>
    +|  <option value="1" #selected="false"> world </option>
      </select> |}]
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
    <select class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="true"> world </option>
    </select> |}];
  Handle.change handle ~get_vdom:Form.view ~selector:"select" ~value:"0";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (world))
    +|(Ok ())

      ==============
      <select class="widget-dropdown" onchange style={ width: 100.00%; }>
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
    <select class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true"> hello </option>
      <option value="1" #selected="false"> world </option>
    </select> |}];
  Handle.change handle ~get_vdom:Form.view ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok hello)
    +|(Ok world)

      ==============
      <select class="widget-dropdown" onchange style={ width: 100.00%; }>
    -|  <option value="0" #selected="true"> hello </option>
    +|  <option value="0" #selected="false"> hello </option>
    -|  <option value="1" #selected="false"> world </option>
    +|  <option value="1" #selected="true"> world </option>
      </select> |}]
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
    <select class="widget-dropdown" onchange style={ width: 100.00%; }>
      <option value="0" #selected="true">  </option>
    </select> |}]
;;

let%expect_test "setting into a string textbox" =
  let component = Form.Elements.Textbox.string () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input> |}];
  Handle.do_actions handle [ "hello world" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
    -|<input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|<input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input> |}]
;;

let%expect_test "typing into a int textbox" =
  let component = Form.Elements.Textbox.int () in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok 123)

      ==============
    -|<input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|<input type="text" placeholder="" spellcheck="false" value:normalized=123 oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 123)
    +|(Error "Expected an integer")

      ==============
    -|<input type="text" placeholder="" spellcheck="false" value:normalized=123 oninput> </input>
    +|<input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input> |}]
;;

let%expect_test "setting into a int textbox" =
  let component = Form.Elements.Textbox.int () in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input> |}];
  Handle.do_actions handle [ 123 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok 123)

      ==============
    -|<input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|<input type="text" placeholder="" spellcheck="false" value:normalized=123 oninput> </input> |}]
;;

let%expect_test "typing into a paired string textbox * int textbox " =
  let component =
    let%sub string_form = Form.Elements.Textbox.string () in
    let%sub int_form = Form.Elements.Textbox.int () in
    let%arr string_form = string_form
    and int_form = int_form in
    Form.both string_form int_form
    |> Form.map_view ~f:(fun (a, b) -> Vdom.Node.div [ a; b ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string * int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    </div> |}];
  Handle.input_text
    handle
    ~get_vdom:Form.view
    ~selector:"input:nth-child(1)"
    ~text:"hello world";
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input:nth-child(2)" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok ("hello world" 123))

      ==============
      <div>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=123 oninput> </input>
      </div> |}]
;;

let%expect_test "setting into a paired string textbox * int textbox " =
  let component =
    let%sub string_form = Form.Elements.Textbox.string () in
    let%sub int_form = Form.Elements.Textbox.int () in
    let%arr string_form = string_form
    and int_form = int_form in
    Form.both string_form int_form
    |> Form.map_view ~f:(fun (a, b) -> Vdom.Node.div [ a; b ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string * int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    </div> |}];
  Handle.do_actions handle [ "hello world", 123 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok ("hello world" 123))

      ==============
      <div>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=123 oninput> </input>
      </div> |}]
;;

let%test_module "Form.all" =
  (module struct
    let make_handle () =
      let component =
        let%sub string_forms =
          List.init 3 ~f:(fun _ -> Form.Elements.Textbox.string ()) |> Computation.all
        in
        let%arr string_forms = string_forms in
        Form.all string_forms |> Form.map_view ~f:(fun l -> Vdom.Node.div l)
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
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    </div> |}];
      Handle.input_text
        handle
        ~get_vdom:Form.view
        ~selector:"input:nth-child(1)"
        ~text:"hello world";
      Handle.input_text
        handle
        ~get_vdom:Form.view
        ~selector:"input:nth-child(2)"
        ~text:"quack";
      Handle.show_diff handle;
      [%expect
        {|
    -|(Ok ("" "" ""))
    +|(Ok ("hello world" quack ""))

      ==============
      <div>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=quack oninput> </input>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      </div> |}]
    ;;

    let%expect_test "setting into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
    (Ok ("" "" ""))

    ==============
    <div>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    </div> |}];
      Handle.do_actions handle [ [ "hello world"; "quack"; "" ] ];
      Handle.show_diff handle;
      [%expect
        {|
    -|(Ok ("" "" ""))
    +|(Ok ("hello world" quack ""))

      ==============
      <div>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=quack oninput> </input>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      </div> |}]
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
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=quack oninput> </input>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      </div> |}]
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
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=quack oninput> </input>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      </div> |}]
    ;;
  end)
;;

let%test_module "Form.all_map" =
  (module struct
    let make_handle () =
      let component =
        let%sub string_forms =
          List.init 3 ~f:(fun i -> i, Form.Elements.Textbox.string ())
          |> Int.Map.of_alist_exn
          |> Computation.all_map
        in
        let%arr string_forms = string_forms in
        Form.all_map string_forms
        |> Form.map_view ~f:(fun map -> Vdom.Node.div (Map.data map))
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
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    </div> |}];
      Handle.input_text
        handle
        ~get_vdom:Form.view
        ~selector:"input:nth-child(1)"
        ~text:"hello world";
      Handle.input_text
        handle
        ~get_vdom:Form.view
        ~selector:"input:nth-child(2)"
        ~text:"quack";
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
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=quack oninput> </input>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      </div> |}]
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
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    </div> |}];
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
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=quack oninput> </input>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      </div> |}]
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
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=quack oninput> </input>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      </div> |}]
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
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=quack oninput> </input>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      </div> |}]
    ;;
  end)
;;

let%expect_test "typing into a time span textbox" =
  let component = Form.Elements.Date_time.time_span () in
  let handle = Handle.create (form_result_spec [%sexp_of: Time_ns.Span.t]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div> |}];
  Handle.input_text handle ~selector:"input" ~get_vdom:Form.view ~text:"24";
  Handle.show handle;
  [%expect
    {|
    (Ok 24s)

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=24 oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div> |}];
  Handle.change handle ~selector:"select" ~get_vdom:Form.view ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok 24m)

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=24 oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="false"> s </option>
        <option value="2" #selected="true"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div> |}]
;;

let%expect_test "setting into a time span textbox" =
  let component = Form.Elements.Date_time.time_span () in
  let handle = Handle.create (form_result_spec [%sexp_of: Time_ns.Span.t]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div> |}];
  Handle.do_actions handle [ Time_ns.Span.of_sec 24. ];
  Handle.show handle;
  [%expect
    {|
    (Ok 24s)

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=24 oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div> |}];
  Handle.do_actions handle [ Time_ns.Span.of_hr 24. ];
  Handle.show handle;
  [%expect
    {|
    (Ok 1d)

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=24 oninput> </input>
      <select class="widget-dropdown" onchange>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="false"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="true"> h </option>
      </select>
    </div> |}]
;;

let%expect_test "typing into a time range textbox, with strict inequality required" =
  let component = Form.Elements.Date_time.Range.time () in
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
    </div> |}];
  Handle.input_text
    handle
    ~get_vdom:Form.view
    ~text:"11:11 AM"
    ~selector:"input:nth-child(1)";
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
    </div> |}];
  Handle.input_text
    handle
    ~get_vdom:Form.view
    ~text:"10:00 AM"
    ~selector:"input:nth-child(2)";
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
    </div> |}];
  Handle.input_text
    handle
    ~get_vdom:Form.view
    ~text:"11:11 AM"
    ~selector:"input:nth-child(2)";
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
    </div> |}];
  Handle.input_text
    handle
    ~get_vdom:Form.view
    ~text:"11:12 AM"
    ~selector:"input:nth-child(2)";
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
    </div> |}]
;;

let%expect_test "typing into a time range textbox, with equality allowed" =
  let component = Form.Elements.Date_time.Range.time ~allow_equal:true () in
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
    </div> |}];
  Handle.input_text
    handle
    ~get_vdom:Form.view
    ~text:"11:11 AM"
    ~selector:"input:nth-child(1)";
  Handle.input_text
    handle
    ~get_vdom:Form.view
    ~text:"10:00 AM"
    ~selector:"input:nth-child(2)";
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
    </div> |}];
  Handle.input_text
    handle
    ~get_vdom:Form.view
    ~text:"11:11 AM"
    ~selector:"input:nth-child(2)";
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
    </div> |}]
;;

let%expect_test "setting into a date range, with strict inequality required" =
  let component = Form.Elements.Date_time.Range.time () in
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
    </div> |}];
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
    </div> |}];
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
    </div> |}];
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
    </div> |}]
;;

let%expect_test "setting into a date range, with equality allowed" =
  let component = Form.Elements.Date_time.Range.time ~allow_equal:true () in
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
    </div> |}];
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
    </div> |}];
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
    </div> |}];
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
    </div> |}]
;;

let%expect_test "using the same component twice" =
  let component =
    let%sub textbox = Form.Elements.Textbox.string () in
    let%arr textbox = textbox in
    Form.both textbox textbox |> Form.map_view ~f:(fun (a, b) -> Vdom.Node.div [ a; b ])
  in
  let handle =
    Handle.create
      (form_result_spec ~censor_paths:false [%sexp_of: string * string])
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
    <div>
      <input type="text" placeholder="" spellcheck="false" value:normalized=b oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized=b oninput> </input>
    </div> |}]
;;

let%expect_test "typing into an int number element (no default)" =
  let component = Form.Elements.Number.int ~step:1 ~min:(-1) ~max:10 () in
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
           min="-1"
           max="10"
           value:normalized=""
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"10";
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
             min="-1"
             max="10"
    -|       value:normalized=""
    +|       value:normalized=10
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"";
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
             min="-1"
             max="10"
    -|       value:normalized=10
    +|       value:normalized=""
             oninput> </input> |}]
;;

let%expect_test "typing into an int number element" =
  let component = Form.Elements.Number.int ~default:0 ~step:1 ~min:(-1) ~max:10 () in
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
           min="-1"
           max="10"
           value:normalized=0
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"10";
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
             min="-1"
             max="10"
    -|       value:normalized=0
    +|       value:normalized=10
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"-1";
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
             min="-1"
             max="10"
    -|       value:normalized=10
    +|       value:normalized=-1
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"11";
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
             min="-1"
             max="10"
    -|       value:normalized=-1
    +|       value:normalized=11
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"-2";
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
             min="-1"
             max="10"
    -|       value:normalized=11
    +|       value:normalized=-2
             oninput> </input> |}]
;;

let%expect_test "setting into an int number element (no default)" =
  let component = Form.Elements.Number.int ~step:1 ~min:(-1) ~max:10 () in
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
           min="-1"
           max="10"
           value:normalized=""
           oninput> </input> |}];
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
             min="-1"
             max="10"
    -|       value:normalized=""
    +|       value:normalized=10
             oninput> </input> |}]
;;

let%expect_test "setting into an int number element" =
  let component = Form.Elements.Number.int ~default:0 ~step:1 ~min:(-1) ~max:10 () in
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
             min="-1"
             max="10"
    -|       value:normalized=11
    +|       value:normalized=-2
             oninput> </input> |}]
;;

let%expect_test "typing into a float number element" =
  let component =
    Form.Elements.Number.float ~default:0. ~step:1. ~min:(-1.) ~max:10.1 ()
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
           min="-1"
           max="10.1"
           value:normalized=0
           oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"10.1";
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
             min="-1"
             max="10.1"
    -|       value:normalized=0
    +|       value:normalized=10.1
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"-1";
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
             min="-1"
             max="10.1"
    -|       value:normalized=10.1
    +|       value:normalized=-1
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"10.2";
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
             min="-1"
             max="10.1"
    -|       value:normalized=-1
    +|       value:normalized=10.2
             oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"-1.1";
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
             min="-1"
             max="10.1"
    -|       value:normalized=10.2
    +|       value:normalized=-1.1
             oninput> </input> |}]
;;

let%expect_test "setting into an int number element" =
  let component =
    Form.Elements.Number.float ~default:0. ~step:1. ~min:(-1.) ~max:10.1 ()
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
             min="-1"
             max="10.1"
    -|       value:normalized=10.2
    +|       value:normalized=-1.1
             oninput> </input> |}]
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
    <ul class="radio-button-container widget-radio-buttons"
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
    </ul> |}];
  Handle.click_on handle ~get_vdom:Form.view ~selector:"label:nth-child(1) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok first)

      ==============
      <ul class="radio-button-container widget-radio-buttons"
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
          <label> |}];
  Handle.click_on handle ~get_vdom:Form.view ~selector:"li:nth-child(2) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok first)
    +|(Ok second)

      ==============
      <ul class="radio-button-container widget-radio-buttons"
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
      </ul> |}]
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
    <ul class="radio-button-container widget-radio-buttons"
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
    </ul> |}];
  Handle.do_actions handle [ "first" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok first)

      ==============
      <ul class="radio-button-container widget-radio-buttons"
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
          <label> |}];
  Handle.do_actions handle [ "second" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok first)
    +|(Ok second)

      ==============
      <ul class="radio-button-container widget-radio-buttons"
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
      </ul> |}]
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
    <ul class="radio-button-container widget-radio-buttons"
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
    </ul> |}]
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
    <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
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
    </ul> |}];
  Handle.click_on handle ~get_vdom:Form.view ~selector:"li:nth-child(1) input";
  Handle.recompute_view handle;
  Handle.click_on handle ~get_vdom:Form.view ~selector:"li:nth-child(2) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (first second))

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
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
      </ul> |}];
  Handle.click_on handle ~get_vdom:Form.view ~selector:"li:nth-child(1) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (first second))
    +|(Ok (second))

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
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
      </ul> |}]
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
    <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
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
    </ul> |}];
  Handle.do_actions handle [ Set.of_list (module String) [ "first"; "second" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (first second))

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
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
      </ul> |}];
  Handle.do_actions handle [ Set.of_list (module String) [ "second" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (first second))
    +|(Ok (second))

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
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
      </ul> |}]
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
    <input type="file" accept=".txt,.png,image/jpeg" oninput> </input>

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
    <input type="file" accept=".txt,.png,image/jpeg" oninput> </input> |}]
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
    <input type="file" accept=".doc,.docx" multiple="" oninput> </input>

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
      <input type="file" accept=".doc,.docx" multiple="" oninput> </input> |}]
;;

let%expect_test "on_change handler should fire when input is changed" =
  let component =
    let%sub input = Form.Elements.Textbox.string () in
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
    <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>

    ("the input changed to" (new_value "")) |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
    -|<input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|<input type="text" placeholder="" spellcheck="false" value:normalized="hello world" oninput> </input>
    ("the input changed to" (new_value "hello world")) |}]
;;

let submit_test_attrs = function
  | "onsubmit" | "onclick" | "disabled" | "value:normalized" -> true
  | _ -> false
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
    let%sub textbox = Form.Elements.Textbox.int () in
    Form.Dynamic.validate_via_effect
      ~sexp_of_model:[%sexp_of: Int.t]
      textbox
      ~f
      ~equal:[%equal: Int.t]
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: int] ~filter_printed_attributes:(fun ~key:_ ~data:_ ->
         false))
      component
  in
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"2";
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
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"5";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"-3";
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

let%expect_test "form validated with an effect with one_at_at_time" =
  let module Q = Effect.For_testing.Query_response_tracker in
  let tracker = Q.create () in
  let print_queued () = print_s [%sexp (Q.queries_pending_response tracker : int list)] in
  let not_positive = Error (Error.of_string "not positive") in
  let f =
    tracker |> Effect.For_testing.of_query_response_tracker |> Bonsai.Value.return
  in
  let component =
    let%sub textbox = Form.Elements.Textbox.int () in
    Form.Dynamic.validate_via_effect
      ~sexp_of_model:[%sexp_of: Int.t]
      textbox
      ~f
      ~one_at_a_time:true
      ~equal:[%equal: Int.t]
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: int] ~filter_printed_attributes:(fun ~key:_ ~data:_ ->
         false))
      component
  in
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"2";
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
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"5";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"20";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"-3";
  Handle.recompute_view_until_stable handle;
  print_queued ();
  [%expect {| (5) |}];
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
  print_queued ();
  [%expect {| (-3) |}];
  Q.maybe_respond tracker ~f:(function
    | -3 -> Respond not_positive
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect {|
    (Error "not positive")

    ==============
    <input> </input> |}]
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
    let%sub textbox = Form.Elements.Textbox.int () in
    Form.Dynamic.validate_via_effect
      ~debounce_ui:(Time_ns.Span.of_sec 1.0)
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      textbox
      ~f
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: int] ~filter_printed_attributes:(fun ~key:_ ~data:_ ->
         false))
      component
  in
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"2";
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
  (* this should be Ok, but it isn't due to the debounce *)
  [%expect {|
    (Error validating...)

    ==============
    <input> </input> |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 2.0);
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  (* now it's good! *)
  [%expect {|
    (Ok 2)

    ==============
    <input> </input> |}]
;;

let%expect_test "extending a projection with an error" =
  let component =
    let%sub textbox = Form.Elements.Textbox.string () in
    let%arr textbox = textbox in
    Form.project
      textbox
      ~parse_exn:Int.of_string
      ~unparse:Int.to_string
      ~extend_view_with_error:(fun _ error ->
      Vdom.Node.sexp_for_debugging
        [%message "An error! Hide the textbox" (error : Error.t)])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error (Failure "Int.of_string: \"\""))

    ==============
    <pre> ("An error! Hide the textbox" (error (Failure "Int.of_string: \"\""))) </pre> |}];
  Handle.do_actions handle [ 1 ];
  Handle.show handle;
  [%expect
    {|
    (Ok 1)

    ==============
    <input type="text" placeholder="" spellcheck="false" value:normalized=1 oninput> </input> |}];
  Handle.input_text handle ~get_vdom:Form.view ~selector:"input" ~text:"not an int";
  Handle.show handle;
  [%expect
    {|
    (Error (Failure "Int.of_string: \"not an int\""))

    ==============
    <pre>
      ("An error! Hide the textbox"
     (error (Failure "Int.of_string: \"not an int\"")))
    </pre> |}]
;;

let%expect_test _ =
  let module Foo = struct
    type t =
      { x : int
      ; y : string
      }
    [@@deriving sexp_of, typed_fields]
  end
  in
  let form =
    Form.Typed.Record.make
      (module struct
        include Foo

        type field_view = Vdom.Node.t
        type resulting_view = Vdom.Node.t

        let form_for_field
          : type a. a Typed_field.t -> (a, field_view) Form.t Computation.t
          = function
          | X -> Form.Elements.Number.int ~step:1 ()
          | Y -> Form.Elements.Textbox.string ()
        ;;

        type value_and_view_of_field_fn =
          { f : 'a. 'a Typed_field.t -> 'a Or_error.t * field_view }

        let finalize_view { f } = Vdom.Node.div [ snd (f X); snd (f Y) ]
      end)
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Foo.t]) form in
  Handle.show handle;
  [%expect
    {|
    (Error "value not specified")

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    </div> |}];
  Handle.do_actions handle [ { x = 1; y = "hello" } ];
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (x 1)
      (y hello)))

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=1 oninput> </input>
      <input type="text" placeholder="" spellcheck="false" value:normalized=hello oninput> </input>
    </div> |}]
;;
