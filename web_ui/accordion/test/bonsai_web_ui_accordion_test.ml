open! Core
open Bonsai_web
open Bonsai_web_test
module Accordion = Bonsai_web_ui_accordion

module Open_close = struct
  type t =
    | Open
    | Close
    | Toggle
end

let get_vdom { Accordion.view; open_ = _; close = _; toggle = _; is_open = _ } = view

module Accordion_result_spec = struct
  type t = Accordion.t
  type incoming = Open_close.t

  let view accordion =
    let module V =
      (val Result_spec.vdom
             ~path_censoring_message:""
             ~hash_censoring_message:""
             ~filter_printed_attributes:(fun ~key ~data:_ ->
               not
                 (String.is_substring key ~substring:"style"
                  || String.equal key "custom-css-vars"))
             Fn.id)
    in
    let vdom_string = V.view (get_vdom accordion) in
    [%string {|
is_open: %{accordion.is_open#Bool}
-----------------
%{vdom_string}|}]
  ;;

  let incoming { Accordion.open_; close; toggle; is_open = _; view = _ } = function
    | Open_close.Open -> open_
    | Close -> close
    | Toggle -> toggle
  ;;
end

let%expect_test "Clicking open and close" =
  let accordion =
    Accordion.component
      ~extra_title_attrs:(Value.return [ Vdom.Attr.create "data-test" "title" ])
      ~starts_open:false
      ~title:(Value.return (Vdom.Node.text "Title"))
      ~content:(Bonsai.const (Vdom.Node.text "Content"))
      ()
  in
  let handle = Handle.create (module Accordion_result_spec) accordion in
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <div class="container" onclick>
      <div data-test="title" class="title-bar title-text title_closed title" onclick>
        <div>
          <div class="icon-container">
            <div class="accordion-closed icon"> </div>
          </div>
          Title
        </div>
      </div>
      <div class="content_common no_padding"> </div>
    </div> |}];
  Handle.click_on handle ~selector:"div[data-test=title]" ~get_vdom;
  Handle.show handle;
  [%expect
    {|
    is_open: true
    -----------------
    <div class="container" onclick>
      <div data-test="title" class="title-bar title-text title title_open" onclick>
        <div>
          <div class="icon-container">
            <div class="accordion-open icon"> </div>
          </div>
          Title
        </div>
      </div>
      <div class="content_common"> Content </div>
    </div> |}];
  Handle.click_on handle ~selector:"div[data-test=title]" ~get_vdom;
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <div class="container" onclick>
      <div data-test="title" class="title-bar title-text title_closed title" onclick>
        <div>
          <div class="icon-container">
            <div class="accordion-closed icon"> </div>
          </div>
          Title
        </div>
      </div>
      <div class="content_common no_padding"> </div>
    </div> |}]
;;

let%expect_test "Opening and closing via effects" =
  let accordion =
    Accordion.component
      ~starts_open:false
      ~title:(Value.return (Vdom.Node.text "Title"))
      ~content:(Bonsai.const (Vdom.Node.text "Content"))
      ()
  in
  let handle = Handle.create (module Accordion_result_spec) accordion in
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <div class="container" onclick>
      <div class="title-bar title-text title_closed title" onclick>
        <div>
          <div class="icon-container">
            <div class="accordion-closed icon"> </div>
          </div>
          Title
        </div>
      </div>
      <div class="content_common no_padding"> </div>
    </div> |}];
  Handle.do_actions handle [ Open ];
  Handle.show_diff handle;
  [%expect
    {|
    -|is_open: false
    +|is_open: true
      -----------------
      <div class="container" onclick>
    -|  <div class="title-bar title-text title_closed title" onclick>
    +|  <div class="title-bar title-text title title_open" onclick>
          <div>
            <div class="icon-container">
    -|        <div class="accordion-closed icon"> </div>
    +|        <div class="accordion-open icon"> </div>
            </div>
            Title
          </div>
        </div>
    -|  <div class="content_common no_padding"> </div>
    +|  <div class="content_common"> Content </div>
      </div> |}];
  (* opening again should do nothing *)
  Handle.do_actions handle [ Open ];
  Handle.show_diff handle;
  [%expect {||}];
  Handle.do_actions handle [ Close ];
  Handle.show_diff handle;
  [%expect
    {|
    -|is_open: true
    +|is_open: false
      -----------------
      <div class="container" onclick>
    -|  <div class="title-bar title-text title title_open" onclick>
    +|  <div class="title-bar title-text title_closed title" onclick>
          <div>
            <div class="icon-container">
    -|        <div class="accordion-open icon"> </div>
    +|        <div class="accordion-closed icon"> </div>
            </div>
            Title
          </div>
        </div>
    -|  <div class="content_common"> Content </div>
    +|  <div class="content_common no_padding"> </div>
      </div> |}];
  (* closing again should do nothing *)
  Handle.do_actions handle [ Close ];
  Handle.show_diff handle;
  [%expect {||}];
  Handle.do_actions handle [ Toggle ];
  Handle.show_diff handle;
  [%expect
    {|
    -|is_open: false
    +|is_open: true
      -----------------
      <div class="container" onclick>
    -|  <div class="title-bar title-text title_closed title" onclick>
    +|  <div class="title-bar title-text title title_open" onclick>
          <div>
            <div class="icon-container">
    -|        <div class="accordion-closed icon"> </div>
    +|        <div class="accordion-open icon"> </div>
            </div>
            Title
          </div>
        </div>
    -|  <div class="content_common no_padding"> </div>
    +|  <div class="content_common"> Content </div>
      </div> |}];
  Handle.do_actions handle [ Toggle ];
  Handle.show_diff handle;
  [%expect
    {|
    -|is_open: true
    +|is_open: false
      -----------------
      <div class="container" onclick>
    -|  <div class="title-bar title-text title title_open" onclick>
    +|  <div class="title-bar title-text title_closed title" onclick>
          <div>
            <div class="icon-container">
    -|        <div class="accordion-open icon"> </div>
    +|        <div class="accordion-closed icon"> </div>
            </div>
            Title
          </div>
        </div>
    -|  <div class="content_common"> Content </div>
    +|  <div class="content_common no_padding"> </div>
      </div> |}]
;;
