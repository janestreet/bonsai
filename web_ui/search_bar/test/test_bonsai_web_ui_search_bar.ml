open! Core
open Virtual_dom
open Bonsai.Let_syntax
module Item = Int

type t =
  { handle : (Vdom.Node.t, Nothing.t) Bonsai_web_test.Handle.t
  ; choices_var : int list Bonsai.Var.t
  }

let initial_choices = [ 1; 2; 3; 40; 100; 1000 ]

let create
  ?max_query_results
  ?additional_query_results_on_click
  ?autocomplete_item
  ?filter_choice
  ?score_choice
  ?of_string
  ?initial_query
  ()
  =
  let choices_var = Bonsai.Var.create initial_choices in
  let component =
    Bonsai_web_ui_search_bar.create
      (module Item)
      ~placeholder:"Pick your favourite number"
      ~extra_textbox_attr:(Vdom.Attr.class_ "my-test-class")
      ~wrap_search_bar:(fun node ->
        Vdom.Node.div ~attrs:[ Vdom.Attr.id "wrapper" ] [ node ])
      ?max_query_results
      ?additional_query_results_on_click
      ?autocomplete_item
      ?filter_choice
      ?score_choice
      ?of_string
      ?initial_query
      ()
      (let%map choices = Bonsai.Var.value choices_var in
       { Bonsai_web_ui_search_bar.Input.choices
       ; on_select =
           (fun i ->
             printf "%d chosen\n" i;
             Ui_effect.Ignore)
       })
  in
  let handle =
    Bonsai_web_test.Handle.create (Bonsai_web_test.Result_spec.vdom Fn.id) component
  in
  { handle; choices_var }
;;

let%expect_test "basics" =
  let t = create () in
  Bonsai_web_test.Handle.show t.handle;
  [%expect
    {|
    <div id="wrapper">
      <div class="wrapper" style={ width: 100.00%; }>
        <div class="search-input-container">
          <input placeholder="Pick your favourite number"
                 tabindex="1"
                 class="my-test-class"
                 #value=""
                 onblur
                 onchange
                 onfocus
                 oninput
                 onkeydown
                 onkeyup
                 style={
                   width: 100.00%;
                 }> </input>
        </div>
      </div>
    </div> |}];
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 2 </li>
    +|        <li onclick onmousedown onmouseover> 3 </li>
    +|        <li onclick onmousedown onmouseover> 40 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li onclick onmousedown onmouseover> 1000 </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"1";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value=""
    +|             #value="1"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
          <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
            <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
              <li onclick onmousedown onmouseover> 1 </li>
    -|        <li onclick onmousedown onmouseover> 2 </li>
    -|        <li onclick onmousedown onmouseover> 3 </li>
    -|        <li onclick onmousedown onmouseover> 40 </li>
              <li onclick onmousedown onmouseover> 100 </li>
              <li onclick onmousedown onmouseover> 1000 </li>
            </ul>
          </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.click_on t.handle ~get_vdom:Fn.id ~selector:"ul li:first-child";
  [%expect {| 1 chosen |}];
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value="1"
    +|             #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    -|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    -|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    -|        <li onclick onmousedown onmouseover> 1 </li>
    -|        <li onclick onmousedown onmouseover> 100 </li>
    -|        <li onclick onmousedown onmouseover> 1000 </li>
    -|      </ul>
    -|    </div>
        </div>
      </div> |}]
;;

let%expect_test "focus / blur" =
  let t = create () in
  Bonsai_web_test.Handle.store_view t.handle;
  Bonsai_web_test.Handle.focus t.handle ~get_vdom:Fn.id ~selector:"input";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 2 </li>
    +|        <li onclick onmousedown onmouseover> 3 </li>
    +|        <li onclick onmousedown onmouseover> 40 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li onclick onmousedown onmouseover> 1000 </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"1";
  Bonsai_web_test.Handle.blur t.handle ~get_vdom:Fn.id ~selector:"input";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value=""
    +|             #value="1"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    -|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    -|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    -|        <li onclick onmousedown onmouseover> 1 </li>
    -|        <li onclick onmousedown onmouseover> 2 </li>
    -|        <li onclick onmousedown onmouseover> 3 </li>
    -|        <li onclick onmousedown onmouseover> 40 </li>
    -|        <li onclick onmousedown onmouseover> 100 </li>
    -|        <li onclick onmousedown onmouseover> 1000 </li>
    -|      </ul>
    -|    </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.focus t.handle ~get_vdom:Fn.id ~selector:"input";
  Bonsai_web_test.Handle.show_diff t.handle;
  (* Note that the search contents are maintained *)
  [%expect
    {|
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value="1"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li onclick onmousedown onmouseover> 1000 </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}]
;;

let%expect_test "update choices" =
  let t = create () in
  Bonsai_web_test.Handle.store_view t.handle;
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 2 </li>
    +|        <li onclick onmousedown onmouseover> 3 </li>
    +|        <li onclick onmousedown onmouseover> 40 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li onclick onmousedown onmouseover> 1000 </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}];
  Bonsai.Var.set t.choices_var (1010 :: initial_choices);
  Bonsai_web_test.Handle.show_diff t.handle;
  (* No updates while the search box is focused *)
  [%expect {| |}];
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
          <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
            <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1010 </li>
              <li onclick onmousedown onmouseover> 1 </li>
              <li onclick onmousedown onmouseover> 2 </li>
              <li onclick onmousedown onmouseover> 3 </li>
              <li onclick onmousedown onmouseover> 40 </li>
              <li onclick onmousedown onmouseover> 100 </li>
              <li onclick onmousedown onmouseover> 1000 </li>
            </ul>
          </div>
        </div>
      </div> |}]
;;

let%expect_test "initial_query" =
  let t = create ~initial_query:"1" () in
  Bonsai_web_test.Handle.show t.handle;
  [%expect
    {|
    <div id="wrapper">
      <div class="wrapper" style={ width: 100.00%; }>
        <div class="search-input-container">
          <input placeholder="Pick your favourite number"
                 tabindex="1"
                 class="my-test-class"
                 #value="1"
                 onblur
                 onchange
                 onfocus
                 oninput
                 onkeydown
                 onkeyup
                 style={
                   width: 100.00%;
                 }> </input>
        </div>
      </div>
    </div> |}];
  Bonsai_web_test.Handle.focus t.handle ~get_vdom:Fn.id ~selector:"input.my-test-class";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value="1"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li onclick onmousedown onmouseover> 1000 </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}]
;;

let%expect_test "autocomplete_item" =
  let t =
    create
      ~autocomplete_item:(fun i ->
        let open Vdom in
        Node.span [ Node.text "Choice: "; Node.strong [ Node.text (Int.to_string i) ] ])
      ()
  in
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"";
  Bonsai_web_test.Handle.show t.handle;
  [%expect
    {|
    <div id="wrapper">
      <div class="wrapper" style={ width: 100.00%; }>
        <div class="search-input-container">
          <input placeholder="Pick your favourite number"
                 tabindex="1"
                 class="my-test-class"
                 #value=""
                 onblur
                 onchange
                 onfocus
                 oninput
                 onkeydown
                 onkeyup
                 style={
                   width: 100.00%;
                 }> </input>
        </div>
        <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
          <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
            <li onclick onmousedown onmouseover>
              <span>
                Choice:
                <strong> 1 </strong>
              </span>
            </li>
            <li onclick onmousedown onmouseover>
              <span>
                Choice:
                <strong> 2 </strong>
              </span>
            </li>
            <li onclick onmousedown onmouseover>
              <span>
                Choice:
                <strong> 3 </strong>
              </span>
            </li>
            <li onclick onmousedown onmouseover>
              <span>
                Choice:
                <strong> 40 </strong>
              </span>
            </li>
            <li onclick onmousedown onmouseover>
              <span>
                Choice:
                <strong> 100 </strong>
              </span>
            </li>
            <li onclick onmousedown onmouseover>
              <span>
                Choice:
                <strong> 1000 </strong>
              </span>
            </li>
          </ul>
        </div>
      </div>
    </div> |}]
;;

let%expect_test "filter_choice" =
  let t =
    create
      ~filter_choice:(fun ~input_text i ->
        match Int.of_string input_text with
        | exception _ -> false
        | input -> i % 2 = input % 2)
      ()
  in
  Bonsai_web_test.Handle.store_view t.handle;
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"99";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value=""
    +|             #value="99"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 3 </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"66";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value="99"
    +|             #value="66"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
          <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
            <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    -|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 2 </li>
    -|        <li onclick onmousedown onmouseover> 3 </li>
    +|        <li onclick onmousedown onmouseover> 40 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li onclick onmousedown onmouseover> 1000 </li>
            </ul>
          </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"foo";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value="66"
    +|             #value="foo"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    -|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    -|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    -|        <li onclick onmousedown onmouseover> 2 </li>
    -|        <li onclick onmousedown onmouseover> 40 </li>
    -|        <li onclick onmousedown onmouseover> 100 </li>
    -|        <li onclick onmousedown onmouseover> 1000 </li>
    -|      </ul>
    -|    </div>
        </div>
      </div> |}]
;;

let%expect_test "score_choice" =
  let t =
    create
      ~score_choice:(fun ~input_text i ->
        if String.is_substring (Int.to_string i) ~substring:input_text then -i else 0)
      ()
  in
  Bonsai_web_test.Handle.store_view t.handle;
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"1";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value=""
    +|             #value="1"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1000 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}]
;;

let%expect_test "of_string" =
  let t =
    create
      ~of_string:(fun input_text -> Option.try_with (fun () -> Int.of_string input_text))
      ()
  in
  Bonsai_web_test.Handle.store_view t.handle;
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"4";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value=""
    +|             #value="4"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 40 </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"40";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value="4"
    +|             #value="40"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
          <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
            <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
              <li onclick onmousedown onmouseover> 40 </li>
            </ul>
          </div>
        </div> |}]
;;

let%expect_test "max_query_results" =
  let t = create ~max_query_results:2 () in
  Bonsai_web_test.Handle.store_view t.handle;
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"1";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
      <div id="wrapper">
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
    -|             #value=""
    +|             #value="1"
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li class="no-cursor"
    +|            onclick
    +|            onmousedown
    +|            style={
    +|              background-color: var(--js-dark-snow-color);
    +|              width: 100.00%;
    +|            }> +1 more </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}]
;;

let%expect_test "show additional query results" =
  let t = create ~max_query_results:3 ~additional_query_results_on_click:2 () in
  Bonsai_web_test.Handle.show t.handle;
  [%expect
    {|
    <div id="wrapper">
      <div class="wrapper" style={ width: 100.00%; }>
        <div class="search-input-container">
          <input placeholder="Pick your favourite number"
                 tabindex="1"
                 class="my-test-class"
                 #value=""
                 onblur
                 onchange
                 onfocus
                 oninput
                 onkeydown
                 onkeyup
                 style={
                   width: 100.00%;
                 }> </input>
        </div>
      </div>
    </div> |}];
  Bonsai_web_test.Handle.input_text t.handle ~get_vdom:Fn.id ~selector:"input" ~text:"";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 2 </li>
    +|        <li onclick onmousedown onmouseover> 3 </li>
    +|        <li class="no-cursor"
    +|            onclick
    +|            onmousedown
    +|            style={
    +|              background-color: var(--js-dark-snow-color);
    +|              width: 100.00%;
    +|            }> +3 more </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.click_on
    t.handle
    ~get_vdom:Fn.id
    ~selector:"#autocomplete-items > li:last-child";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
          <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
            <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
              <li onclick onmousedown onmouseover> 1 </li>
              <li onclick onmousedown onmouseover> 2 </li>
              <li onclick onmousedown onmouseover> 3 </li>
    +|        <li onclick onmousedown onmouseover> 40 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
              <li class="no-cursor"
                  onclick
                  onmousedown
                  style={
                    background-color: var(--js-dark-snow-color);
                    width: 100.00%;
    -|            }> +3 more </li>
    +|            }> +1 more </li>
            </ul>
          </div>
        </div>
      </div> |}];
  List.init 5 ~f:(fun _ ->
    Bonsai_web_test.Handle.keydown
      t.handle
      ~get_vdom:Fn.id
      ~selector:"input"
      ~key:ArrowDown)
  |> List.iter ~f:(Fn.const ());
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
    ("default prevented" (key ArrowDown))
    ("default prevented" (key ArrowDown))
    ("default prevented" (key ArrowDown))
    ("default prevented" (key ArrowDown))
    ("default prevented" (key ArrowDown))

                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
          <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
            <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
              <li onclick onmousedown onmouseover> 1 </li>
              <li onclick onmousedown onmouseover> 2 </li>
              <li onclick onmousedown onmouseover> 3 </li>
              <li onclick onmousedown onmouseover> 40 </li>
    -|        <li onclick onmousedown onmouseover> 100 </li>
    +|        <li class="autocomplete-active" onclick onmousedown onmouseover> 100 </li>
              <li class="no-cursor"
                  onclick
                  onmousedown
                  style={
                    background-color: var(--js-dark-snow-color);
                    width: 100.00%;
                  }> +1 more </li>
            </ul>
          </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.keydown t.handle ~get_vdom:Fn.id ~selector:"input" ~key:ArrowDown;
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
    ("default prevented" (key ArrowDown))

            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
          <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
            <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    -|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li class="autocomplete-active" onclick onmousedown onmouseover> 1 </li>
              <li onclick onmousedown onmouseover> 2 </li>
              <li onclick onmousedown onmouseover> 3 </li>
              <li onclick onmousedown onmouseover> 40 </li>
    -|        <li class="autocomplete-active" onclick onmousedown onmouseover> 100 </li>
    +|        <li onclick onmousedown onmouseover> 100 </li>
              <li class="no-cursor"
                  onclick
                  onmousedown
                  style={
                    background-color: var(--js-dark-snow-color);
                    width: 100.00%;
                  }> +1 more </li>
            </ul>
          </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.click_on
    t.handle
    ~get_vdom:Fn.id
    ~selector:"#autocomplete-items > li:last-child";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
          <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
            <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
              <li class="autocomplete-active" onclick onmousedown onmouseover> 1 </li>
              <li onclick onmousedown onmouseover> 2 </li>
              <li onclick onmousedown onmouseover> 3 </li>
              <li onclick onmousedown onmouseover> 40 </li>
              <li onclick onmousedown onmouseover> 100 </li>
    -|        <li class="no-cursor"
    -|            onclick
    -|            onmousedown
    -|            style={
    -|              background-color: var(--js-dark-snow-color);
    -|              width: 100.00%;
    -|            }> +1 more </li>
    +|        <li onclick onmousedown onmouseover> 1000 </li>
            </ul>
          </div>
        </div>
      </div> |}];
  (* Selecting an item resets the number of query results to show. *)
  Bonsai_web_test.Handle.click_on
    t.handle
    ~get_vdom:Fn.id
    ~selector:"#autocomplete-items > li:first-child";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
    1 chosen

        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    -|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    -|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    -|        <li class="autocomplete-active" onclick onmousedown onmouseover> 1 </li>
    -|        <li onclick onmousedown onmouseover> 2 </li>
    -|        <li onclick onmousedown onmouseover> 3 </li>
    -|        <li onclick onmousedown onmouseover> 40 </li>
    -|        <li onclick onmousedown onmouseover> 100 </li>
    -|        <li onclick onmousedown onmouseover> 1000 </li>
    -|      </ul>
    -|    </div>
        </div>
      </div> |}];
  Bonsai_web_test.Handle.focus t.handle ~get_vdom:Fn.id ~selector:"input";
  Bonsai_web_test.Handle.show_diff t.handle;
  [%expect
    {|
        <div class="wrapper" style={ width: 100.00%; }>
          <div class="search-input-container">
            <input placeholder="Pick your favourite number"
                   tabindex="1"
                   class="my-test-class"
                   #value=""
                   onblur
                   onchange
                   onfocus
                   oninput
                   onkeydown
                   onkeyup
                   style={
                     width: 100.00%;
                   }> </input>
          </div>
    +|    <div class="autocomplete" style={ width: 100.00%; max-width: 100.00%; }>
    +|      <ul id="autocomplete-items" class="autocomplete-items" onmouseout>
    +|        <li onclick onmousedown onmouseover> 1 </li>
    +|        <li onclick onmousedown onmouseover> 2 </li>
    +|        <li onclick onmousedown onmouseover> 3 </li>
    +|        <li class="no-cursor"
    +|            onclick
    +|            onmousedown
    +|            style={
    +|              background-color: var(--js-dark-snow-color);
    +|              width: 100.00%;
    +|            }> +3 more </li>
    +|      </ul>
    +|    </div>
        </div>
      </div> |}]
;;
