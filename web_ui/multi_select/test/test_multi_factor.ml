open! Core
open! Import
open  Bonsai.Let_syntax
open  Bonsai_web_test

module Key = struct
  module T = struct
    type t =
      | Foo
      | Bar
      | Baz
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
  include Sexpable.To_stringable (T)

  let name_singular = "foo"
  let name_plural   = "foos"
end

module S = Bonsai_web_ui_multi_select.Multi_factor.Make (String) (Key)

let default_all_keys = Key.(Set.of_list [ Foo; Bar; Baz ])

let default_per_subwidget =
  let mk items =
    { S.all_items = String.Set.of_list items; default_selection_status = Selected }
  in
  Bonsai.Value.return
    (Key.Map.of_alist_exn
       [ Foo, mk [ "foo1"; "foo2"; "foo3" ]; Bar, mk []; Baz, mk [ "baz1"; "baz2" ] ])
;;

let make_handle
      ?initial_model_settings
      ?(all_keys = default_all_keys)
      ?(per_subwidget = default_per_subwidget)
      ?(id_prefix = Bonsai.Value.return "test-foo")
      which_display
  =
  let bonsai =
    let%sub result =
      S.bonsai ?initial_model_settings ~id_prefix ~all_keys per_subwidget
    in
    result
    |> Bonsai.Value.map ~f:(fun result ->
      let display =
        sprintf
          !"Selected items: %{sexp: String.Set.t Key.Map.t}\n"
          (S.Result.selection result)
        ^
        match which_display with
        | `Simple -> Lazy.force result.view_for_testing
        | `Html   ->
          Virtual_dom_test_helpers.Node_helpers.(
            unsafe_convert_exn result.view |> to_string_html)
      in
      display, result.inject)
    |> return
  in
  Handle.create
    (module struct
      type t        = string * (S.Action.t -> unit Ui_effect.t)
      type incoming = S.Action.t

      let view     (s, _     ) = s
      let incoming (_, inject) = inject
    end)
    bonsai
;;

let%expect_test "simple view" =
  let handle = make_handle `Simple in
  Handle.show handle;
  [%expect
    {|
  Selected items: ((Foo (foo1 foo2 foo3)) (Bar ()) (Baz (baz1 baz2)))
  ┌───────────────────┬───────────────────┬───────────────────┐
  │ * Foo             │   Bar             │   Baz             │
  ├───────────────────┼───────────────────┼───────────────────┤
  │ Search string: '' │ Search string: '' │ Search string: '' │
  │    * foo1         │                   │    * baz1         │
  │    * foo2         │                   │    * baz2         │
  │    * foo3         │                   │                   │
  └───────────────────┴───────────────────┴───────────────────┘ |}];
  Handle.do_actions
    handle
    [ Cycle_focused_subwidget `Next; Cycle_focused_subwidget `Next ];
  Handle.show handle;
  [%expect
    {|
  Selected items: ((Foo (foo1 foo2 foo3)) (Bar ()) (Baz (baz1 baz2)))
  ┌───────────────────┬───────────────────┬───────────────────┐
  │   Foo             │   Bar             │ * Baz             │
  ├───────────────────┼───────────────────┼───────────────────┤
  │ Search string: '' │ Search string: '' │ Search string: '' │
  │    * foo1         │                   │    * baz1         │
  │    * foo2         │                   │    * baz2         │
  │    * foo3         │                   │                   │
  └───────────────────┴───────────────────┴───────────────────┘ |}];
  Handle.do_actions handle [ Cycle_focused_subwidget `Next ];
  Handle.show       handle;
  [%expect
    {|
  Selected items: ((Foo (foo1 foo2 foo3)) (Bar ()) (Baz (baz1 baz2)))
  ┌───────────────────┬───────────────────┬───────────────────┐
  │ * Foo             │   Bar             │   Baz             │
  ├───────────────────┼───────────────────┼───────────────────┤
  │ Search string: '' │ Search string: '' │ Search string: '' │
  │    * foo1         │                   │    * baz1         │
  │    * foo2         │                   │    * baz2         │
  │    * foo3         │                   │                   │
  └───────────────────┴───────────────────┴───────────────────┘ |}];
  Handle.do_actions handle [ Select_on_all_subwidgets `None ];
  Handle.show       handle;
  [%expect
    {|
  Selected items: ((Foo ()) (Bar ()) (Baz ()))
  ┌───────────────────┬───────────────────┬───────────────────┐
  │ * Foo             │   Bar             │   Baz             │
  ├───────────────────┼───────────────────┼───────────────────┤
  │ Search string: '' │ Search string: '' │ Search string: '' │
  │      foo1         │                   │      baz1         │
  │      foo2         │                   │      baz2         │
  │      foo3         │                   │                   │
  └───────────────────┴───────────────────┴───────────────────┘ |}]
;;

let%expect_test "initial model settings" =
  let initial_model_settings =
    [ Key.Foo, S.Single_factor.Initial_model_settings.create ~search_string:"2" ()
    ; ( Key.Baz
      , S.Single_factor.Initial_model_settings.create
          ~selection_status:
            (String.Map.singleton "baz1" S.Single_factor.Selection_status.Unselected)
          ~focused_item:"baz2"
          () )
    ]
    |> Key.Map.of_alist_exn
  in
  let handle = make_handle ~initial_model_settings `Simple in
  Handle.show handle;
  [%expect
    {|
  Selected items: ((Foo (foo1 foo2 foo3)) (Bar ()) (Baz (baz2)))
  ┌────────────────────┬───────────────────┬───────────────────┐
  │ * Foo              │   Bar             │   Baz             │
  ├────────────────────┼───────────────────┼───────────────────┤
  │ Search string: '2' │ Search string: '' │ Search string: '' │
  │    * foo2          │                   │      baz1         │
  │                    │                   │ -> * baz2         │
  └────────────────────┴───────────────────┴───────────────────┘ |}]
;;

let%expect_test "html view" =
  let handle = make_handle `Html in
  Handle.show handle;
  [%expect
    {|
  Selected items: ((Foo (foo1 foo2 foo3)) (Bar ()) (Baz (baz1 baz2)))
  <div style={ display: flex; flex-direction: column; flex-wrap: nowrap; }>
    <div style={ flex-shrink: 0; }>
      Select on all foos:
      <a href="about:blank" onclick> all </a>
      ;
      <a href="about:blank" onclick> none </a>
    </div>
    <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; flex-shrink: 0; }>
      <div id="test-foo-Foo"
           class="multi-factor-focused-subwidget multi-factor-subwidget"
           onclick
           style={
             flex-shrink: 0;
           }>
        <div class="multi-select-container">
          <div class="multi-select-header"> Foo </div>
          <input type="text"
                 placeholder=""
                 spellcheck="false"
                 autofocus="false"
                 id="test-foo-search-box-Foo"
                 value:normalized=""
                 oninput> </input>
          <div class="multi-select-select-all-none">
            Select:
            <a href="about:blank" class="multi-select-select-all" onclick> all </a>
            ;
            <a href="about:blank" class="multi-select-select-none" onclick> none </a>
          </div>
          <div class="multi-select-checkboxes">
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> foo1 </label>
            </div>
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> foo2 </label>
            </div>
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> foo3 </label>
            </div>
          </div>
        </div>
      </div>
      <div id="test-foo-Bar"
           class="multi-factor-subwidget multi-factor-unfocused-subwidget"
           onclick
           style={
             flex-shrink: 0;
           }>
        <div class="multi-select-container">
          <div class="multi-select-header"> Bar </div>
          <input type="text"
                 placeholder=""
                 spellcheck="false"
                 autofocus="false"
                 id="test-foo-search-box-Bar"
                 value:normalized=""
                 oninput> </input>
          <div class="multi-select-select-all-none">
            Select:
            <a href="about:blank" class="multi-select-select-all" onclick> all </a>
            ;
            <a href="about:blank" class="multi-select-select-none" onclick> none </a>
          </div>
          <div class="multi-select-checkboxes"> </div>
        </div>
      </div>
      <div id="test-foo-Baz"
           class="multi-factor-subwidget multi-factor-unfocused-subwidget"
           onclick
           style={
             flex-shrink: 0;
           }>
        <div class="multi-select-container">
          <div class="multi-select-header"> Baz </div>
          <input type="text"
                 placeholder=""
                 spellcheck="false"
                 autofocus="false"
                 id="test-foo-search-box-Baz"
                 value:normalized=""
                 oninput> </input>
          <div class="multi-select-select-all-none">
            Select:
            <a href="about:blank" class="multi-select-select-all" onclick> all </a>
            ;
            <a href="about:blank" class="multi-select-select-none" onclick> none </a>
          </div>
          <div class="multi-select-checkboxes">
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> baz1 </label>
            </div>
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> baz2 </label>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div> |}];
  Handle.do_actions handle [ Cycle_focused_subwidget `Next ];
  Handle.show       handle;
  [%expect
    {|
  Selected items: ((Foo (foo1 foo2 foo3)) (Bar ()) (Baz (baz1 baz2)))
  <div style={ display: flex; flex-direction: column; flex-wrap: nowrap; }>
    <div style={ flex-shrink: 0; }>
      Select on all foos:
      <a href="about:blank" onclick> all </a>
      ;
      <a href="about:blank" onclick> none </a>
    </div>
    <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; flex-shrink: 0; }>
      <div id="test-foo-Foo"
           class="multi-factor-subwidget multi-factor-unfocused-subwidget"
           onclick
           style={
             flex-shrink: 0;
           }>
        <div class="multi-select-container">
          <div class="multi-select-header"> Foo </div>
          <input type="text"
                 placeholder=""
                 spellcheck="false"
                 autofocus="false"
                 id="test-foo-search-box-Foo"
                 value:normalized=""
                 oninput> </input>
          <div class="multi-select-select-all-none">
            Select:
            <a href="about:blank" class="multi-select-select-all" onclick> all </a>
            ;
            <a href="about:blank" class="multi-select-select-none" onclick> none </a>
          </div>
          <div class="multi-select-checkboxes">
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> foo1 </label>
            </div>
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> foo2 </label>
            </div>
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> foo3 </label>
            </div>
          </div>
        </div>
      </div>
      <div id="test-foo-Bar"
           class="multi-factor-focused-subwidget multi-factor-subwidget"
           onclick
           style={
             flex-shrink: 0;
           }>
        <div class="multi-select-container">
          <div class="multi-select-header"> Bar </div>
          <input type="text"
                 placeholder=""
                 spellcheck="false"
                 autofocus="false"
                 id="test-foo-search-box-Bar"
                 value:normalized=""
                 oninput> </input>
          <div class="multi-select-select-all-none">
            Select:
            <a href="about:blank" class="multi-select-select-all" onclick> all </a>
            ;
            <a href="about:blank" class="multi-select-select-none" onclick> none </a>
          </div>
          <div class="multi-select-checkboxes"> </div>
        </div>
      </div>
      <div id="test-foo-Baz"
           class="multi-factor-subwidget multi-factor-unfocused-subwidget"
           onclick
           style={
             flex-shrink: 0;
           }>
        <div class="multi-select-container">
          <div class="multi-select-header"> Baz </div>
          <input type="text"
                 placeholder=""
                 spellcheck="false"
                 autofocus="false"
                 id="test-foo-search-box-Baz"
                 value:normalized=""
                 oninput> </input>
          <div class="multi-select-select-all-none">
            Select:
            <a href="about:blank" class="multi-select-select-all" onclick> all </a>
            ;
            <a href="about:blank" class="multi-select-select-none" onclick> none </a>
          </div>
          <div class="multi-select-checkboxes">
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> baz1 </label>
            </div>
            <div class="multi-select-item" onclick>
              <input type="checkbox" checked="" #checked="true" onchange> </input>
              <label> baz2 </label>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div> |}]
;;
