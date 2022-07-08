open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
module Tabs = Bonsai_web_ui_tabs

module My_tabs = struct
  type t =
    | A
    | B
    | C
  [@@deriving sexp, compare, equal, enumerate]
end

open My_tabs

let basic_tab_component ?additional_button_attributes ?decorate ?f () =
  let f =
    Option.value f ~default:(fun ~change_tab:_ tab ->
      Bonsai.enum
        (module My_tabs)
        ~match_:tab
        ~with_:(function
          | A -> Bonsai.const (Vdom.Node.text "a")
          | B -> Bonsai.const (Vdom.Node.text "b")
          | C -> Bonsai.const (Vdom.Node.text "c")))
  in
  let%sub state = Tabs.tab_state (module My_tabs) ~initial:A in
  let%sub tab_result =
    Tabs.tab_ui
      ?additional_button_attributes
      ?decorate
      (module My_tabs)
      ~all_tabs:(Value.return My_tabs.all)
      state
      ~f
  in
  return (Value.map tab_result ~f:Tabs.Result.combine_trivially)
;;

let%expect_test "how is it printed" =
  let handle = Handle.create (Result_spec.vdom Fn.id) (basic_tab_component ()) in
  Handle.show handle;
  [%expect
    {|
    <div class="bonsai_ui_tab_container">
      <div class="bonsai_ui_tab_tabs">
        <button name="A" class="bonsai_ui_tab selected" onclick> A </button>
        <button name="B" class="bonsai_ui_tab" onclick> B </button>
        <button name="C" class="bonsai_ui_tab" onclick> C </button>
      </div>
      <div class="bonsai_ui_tab_body"> a </div>
    </div> |}]
;;

let%expect_test "you can click on a button to change the tab" =
  let handle = Handle.create (Result_spec.vdom Fn.id) (basic_tab_component ()) in
  Handle.show handle;
  let before = [%expect.output] in
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"[name=B]";
  Handle.show handle;
  let after = [%expect.output] in
  Expect_test_patdiff.print_patdiff before after;
  [%expect
    {|
    -1,8 +1,8
      <div class="bonsai_ui_tab_container">
        <div class="bonsai_ui_tab_tabs">
    -|    <button name="A" class="bonsai_ui_tab selected" onclick> A </button>
    +|    <button name="A" class="bonsai_ui_tab" onclick> A </button>
    -|    <button name="B" class="bonsai_ui_tab" onclick> B </button>
    +|    <button name="B" class="bonsai_ui_tab selected" onclick> B </button>
          <button name="C" class="bonsai_ui_tab" onclick> C </button>
        </div>
    -|  <div class="bonsai_ui_tab_body"> a </div>
    +|  <div class="bonsai_ui_tab_body"> b </div>
      </div> |}]
;;

let%expect_test "you can customize the display of the tabs" =
  let decorate = function
    | A -> Vdom.Node.text "GIMME AN A!"
    | B -> Vdom.Node.text "GIMME A B!"
    | C -> Vdom.Node.text "GIMME A C!"
  in
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (basic_tab_component ~decorate:(Value.return decorate) ())
  in
  Handle.show handle;
  [%expect
    {|
    <div class="bonsai_ui_tab_container">
      <div class="bonsai_ui_tab_tabs">
        <button name="A" class="bonsai_ui_tab selected" onclick> GIMME AN A! </button>
        <button name="B" class="bonsai_ui_tab" onclick> GIMME A B! </button>
        <button name="C" class="bonsai_ui_tab" onclick> GIMME A C! </button>
      </div>
      <div class="bonsai_ui_tab_body"> a </div>
    </div> |}]
;;

let%expect_test "you can add more attributes to the buttons" =
  let alt text = Vdom.Attr.create "alt" text in
  let additional_attrs ~is_selected:_ = function
    | A -> alt "click on a!"
    | B -> alt "click on b!"
    | C -> alt "click on c!"
  in
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (basic_tab_component
         ~additional_button_attributes:(Value.return additional_attrs)
         ())
  in
  Handle.show handle;
  [%expect
    {|
    <div class="bonsai_ui_tab_container">
      <div class="bonsai_ui_tab_tabs">
        <button name="A" alt="click on a!" class="bonsai_ui_tab selected" onclick> A </button>
        <button name="B" alt="click on b!" class="bonsai_ui_tab" onclick> B </button>
        <button name="C" alt="click on c!" class="bonsai_ui_tab" onclick> C </button>
      </div>
      <div class="bonsai_ui_tab_body"> a </div>
    </div> |}]
;;

let%expect_test "you can switch the tab from inside the inner component" =
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (basic_tab_component
         ~f:(fun ~change_tab _ ->
           let%arr change_tab = change_tab in
           Vdom.Node.button
             ~attr:
               (Vdom.Attr.many_without_merge
                  [ Vdom.Attr.id "my-button"; Vdom.Attr.on_click (fun _ -> change_tab C) ])
             [ Vdom.Node.text "click to move to tab c!" ])
         ())
  in
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"#my-button";
  Handle.show handle;
  [%expect
    {|
    <div class="bonsai_ui_tab_container">
      <div class="bonsai_ui_tab_tabs">
        <button name="A" class="bonsai_ui_tab" onclick> A </button>
        <button name="B" class="bonsai_ui_tab" onclick> B </button>
        <button name="C" class="bonsai_ui_tab selected" onclick> C </button>
      </div>
      <div class="bonsai_ui_tab_body">
        <button id="my-button" onclick> click to move to tab c! </button>
      </div>
    </div> |}]
;;
