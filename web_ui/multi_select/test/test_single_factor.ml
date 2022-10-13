open! Core
open! Import
module S = Bonsai_web_ui_multi_select.Make (String)
open Bonsai.Let_syntax
open Bonsai_web_test


let all_items = String.Set.of_list [ "foo"; "bar"; "baz" ]

let bonsai
      ?(which_display = `Simple)
      ?search_string
      ?(default_selection_status = Bonsai.Value.return S.Selection_status.Selected)
      ?selection_status
      ?focused_item
      ~view_config
      all_items
  =
  let%sub result =
    S.bonsai
      ~default_selection_status
      ~initial_model_settings:
        (S.Initial_model_settings.create
           ?search_string
           ?selection_status
           ?focused_item
           ())
      ~view_config
      all_items
  in
  let%arr result = result in
  let display =
    sprintf !"Selected items: %{sexp:String.Set.t}\n" result.selected_items
    ^
    match which_display with
    | `Simple -> Lazy.force result.view_for_testing
    | `Html   ->
      Virtual_dom_test_helpers.Node_helpers.(
        unsafe_convert_exn result.view |> to_string_html)
  in
  display, result.inject
;;

let default_view_config =
  Bonsai.Value.return
    (S.View_config.create
       ~header:(Vdom.Node.text "header")
       ~autofocus_search_box:false
       ())
;;

let handle
      ?which_display
      ?search_string
      ?selection_status
      ?focused_item
      ?default_selection_status
      ?(view_config = default_view_config)
      ?(all_items = Bonsai.Value.return all_items)
      ()
  =
  Bonsai_web_test.Handle.create
    (module struct
      type t        = string * (S.Action.t -> unit Vdom.Effect.t)
      type incoming = S.Action.t

      let view     (s, _     ) = s
      let incoming (_, inject) = inject
    end)
    (bonsai
       ?which_display
       ?search_string
       ?focused_item
       ?selection_status
       ?default_selection_status
       ~view_config
       all_items)
;;

let%expect_test "focus" =
  let handle = handle () in
  Handle.do_actions handle [ Select_all ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
       * foo |}];
  Handle.do_actions handle [ Move_focus `Next ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
    -> * bar
       * baz
       * foo |}];
  Handle.do_actions handle [ Move_focus `Next ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
    -> * baz
       * foo |}];
  Handle.do_actions handle [ Move_focus `Prev ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
    -> * bar
       * baz
       * foo |}];
  Handle.do_actions handle [ Move_focus `Prev ];
  Handle.show       handle;
  (* Note that we don't wrap around. *)
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
    -> * bar
       * baz
       * foo |}];
  Handle.do_actions handle [ Set_focus (Some "foo") ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
    -> * foo |}];
  Handle.do_actions handle [ Move_focus `Next ];
  Handle.show       handle;
  (* Note that we don't wrap around. *)
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
    -> * foo |}];
  Handle.do_actions handle [ Set_focus None ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
       * foo |}]
;;

let%expect_test "selections" =
  let handle = handle () in
  Handle.do_actions handle [ Set_item_selected { item = "bar"; status = Unselected } ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (baz foo)
    Search string: ''
         bar
       * baz
       * foo |}];
  Handle.do_actions handle [ Set_item_selected { item = "bar"; status = Selected } ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
       * foo |}];
  Handle.do_actions handle [ Move_focus `Prev ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
    -> * foo |}];
  Handle.do_actions handle [ Toggle_focused_item_selected ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz)
    Search string: ''
       * bar
       * baz
    ->   foo |}];
  Handle.do_actions handle [ Select_all ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
    -> * foo |}];
  Handle.do_actions handle [ Select_none ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: ()
    Search string: ''
         bar
         baz
    ->   foo |}];
  Handle.do_actions
    handle
    [ Set_all_selection_statuses
        (String.Map.of_alist_exn
           [ "bar", S.Selection_status.Selected; "baz", Unselected ])
    ];
  Handle.show handle;
  [%expect
    {|
    Selected items: (bar foo)
    Search string: ''
       * bar
         baz
    -> * foo |}]
;;

let%expect_test "search string vdom" =
  let handle =
    Bonsai_web_test.Handle.create
      (Bonsai_web_test.Result_spec.vdom (fun r -> r.S.Result.view))
      (S.bonsai ~view_config:default_view_config (Bonsai.Value.return all_items))
  in
  Handle.show handle;
  [%expect
    {|
    <div class="multi-select-container">
      <div class="multi-select-header"> header </div>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <div class="multi-select-select-all-none">
        Select:
        <a href="about:blank" class="multi-select-select-all" onclick> all </a>
        ;
        <a href="about:blank" class="multi-select-select-none" onclick> none </a>
      </div>
      <div class="multi-select-checkboxes">
        <div class="multi-select-item" onclick>
          <input type="checkbox" #checked="false" onchange> </input>
          <label> bar </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" #checked="false" onchange> </input>
          <label> baz </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" #checked="false" onchange> </input>
          <label> foo </label>
        </div>
      </div>
    </div> |}];
  Handle.input_text
    handle
    ~get_vdom:(fun result -> result.S.Result.view)
    ~selector:"input"
    ~text:"bar";
  Handle.show_diff handle;
  [%expect
    {|
      <div class="multi-select-container">
        <div class="multi-select-header"> header </div>
    -|  <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
    +|  <input type="text" placeholder="" spellcheck="false" value:normalized=bar oninput> </input>
        <div class="multi-select-select-all-none">
          Select:
          <a href="about:blank" class="multi-select-select-all" onclick> all </a>
          ;
          <a href="about:blank" class="multi-select-select-none" onclick> none </a>
        </div>
        <div class="multi-select-checkboxes">
          <div class="multi-select-item" onclick>
            <input type="checkbox" #checked="false" onchange> </input>
            <label> bar </label>
          </div>
    -|    <div class="multi-select-item" onclick>
    -|      <input type="checkbox" #checked="false" onchange> </input>
    -|      <label> baz </label>
    -|    </div>
    -|    <div class="multi-select-item" onclick>
    -|      <input type="checkbox" #checked="false" onchange> </input>
    -|      <label> foo </label>
    -|    </div>
        </div>
      </div> |}]
;;

let%expect_test "searching" =
  let handle = handle () in
  Handle.do_actions handle [ Select_none; Update_search_string "ba" ];
  Handle.show       handle;
  [%expect {|
    Selected items: ()
    Search string: 'ba'
         bar
         baz |}];
  Handle.do_actions handle [ Move_focus `Next ];
  Handle.show       handle;
  [%expect {|
    Selected items: ()
    Search string: 'ba'
    ->   bar
         baz |}];
  Handle.do_actions handle [ Move_focus `Next ];
  Handle.show       handle;
  [%expect {|
    Selected items: ()
    Search string: 'ba'
         bar
    ->   baz |}];
  Handle.do_actions handle [ Select_all ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz)
    Search string: 'ba'
       * bar
    -> * baz |}];
  Handle.do_actions handle [ Update_search_string "" ];
  Handle.show       handle;
  (* Note that "foo" is not selected as it was not in the search results when Select_all
     was run *)
  [%expect
    {|
    Selected items: (bar baz)
    Search string: ''
       * bar
    -> * baz
         foo |}];
  Handle.do_actions handle [ Select_all ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
    -> * baz
       * foo |}];
  Handle.do_actions handle [ Update_search_string "ba" ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: 'ba'
       * bar
    -> * baz |}];
  Handle.do_actions handle [ Select_none ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (foo)
    Search string: 'ba'
         bar
    ->   baz |}];
  Handle.do_actions handle [ Update_search_string "" ];
  Handle.show       handle;
  (* Note that foo is not deselected as it was not in the search results when Select_none
     was run. *)
  [%expect
    {|
    Selected items: (foo)
    Search string: ''
         bar
    ->   baz
       * foo |}]
;;

let%expect_test "changing items" =
  let all_items_var = Bonsai.Var.create all_items    in
  let all_items     = Bonsai.Var.value all_items_var in
  let handle        = handle ~all_items ()           in
  Bonsai.Var.update all_items_var ~f:(fun all_items -> Set.add all_items "quux");
  Handle.show handle;
  [%expect
    {|
    Selected items: (bar baz foo quux)
    Search string: ''
       * bar
       * baz
       * foo
       * quux |}];
  Handle.do_actions handle [ Update_search_string "" ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo quux)
    Search string: ''
       * bar
       * baz
       * foo
       * quux |}]
;;

let%expect_test "default_selection_status = Unselected" =
  let all_items_var                = Bonsai.Var.create all_items                     in
  let all_items                    = Bonsai.Var.value all_items_var                  in
  let default_selection_status_var = Bonsai.Var.create S.Selection_status.Unselected in
  let default_selection_status     = Bonsai.Var.value default_selection_status_var   in
  let handle                       = handle ~default_selection_status ~all_items ()  in
  Handle.show handle;
  [%expect
    {|
    Selected items: ()
    Search string: ''
         bar
         baz
         foo |}];
  Handle.do_actions handle [ Select_all ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
       * foo |}];
  Bonsai.Var.update all_items_var ~f:(fun all_items -> Set.add all_items "quux");
  Handle.show handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    Search string: ''
       * bar
       * baz
       * foo
         quux |}]
;;

let%expect_test "specifying arguments to S.Model.create" =
  let handle =
    handle
      ~selection_status:(String.Map.singleton "bar" S.Selection_status.Unselected)
      ~focused_item:"baz"
      ~search_string:"b"
      ()
  in
  Handle.show handle;
  [%expect
    {|
    Selected items: (baz foo)
    Search string: 'b'
         bar
    -> * baz |}]
;;

let%expect_test "html" =
  let all_items_var = Bonsai.Var.create all_items                                  in
  let all_items = Bonsai.Var.value all_items_var                                   in
  let default_selection_status_var = Bonsai.Var.create S.Selection_status.Selected in
  let default_selection_status = Bonsai.Var.value default_selection_status_var     in
  let handle = handle ~default_selection_status ~all_items ~which_display:`Html () in
  Handle.show handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    <div class="multi-select-container">
      <div class="multi-select-header"> header </div>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <div class="multi-select-select-all-none">
        Select:
        <a href="about:blank" class="multi-select-select-all" onclick> all </a>
        ;
        <a href="about:blank" class="multi-select-select-none" onclick> none </a>
      </div>
      <div class="multi-select-checkboxes">
        <div class="multi-select-item" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> bar </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> baz </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> foo </label>
        </div>
      </div>
    </div> |}];
  Handle.do_actions handle [ Move_focus `Next; Select_none ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: ()
    <div class="multi-select-container">
      <div class="multi-select-header"> header </div>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <div class="multi-select-select-all-none">
        Select:
        <a href="about:blank" class="multi-select-select-all" onclick> all </a>
        ;
        <a href="about:blank" class="multi-select-select-none" onclick> none </a>
      </div>
      <div class="multi-select-checkboxes">
        <div class="multi-select-item multi-select-item-focused" onclick>
          <input type="checkbox" #checked="false" onchange> </input>
          <label> bar </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" #checked="false" onchange> </input>
          <label> baz </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" #checked="false" onchange> </input>
          <label> foo </label>
        </div>
      </div>
    </div> |}];
  Handle.do_actions handle [ Select_all ];
  Handle.show       handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    <div class="multi-select-container">
      <div class="multi-select-header"> header </div>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <div class="multi-select-select-all-none">
        Select:
        <a href="about:blank" class="multi-select-select-all" onclick> all </a>
        ;
        <a href="about:blank" class="multi-select-select-none" onclick> none </a>
      </div>
      <div class="multi-select-checkboxes">
        <div class="multi-select-item multi-select-item-focused" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> bar </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> baz </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> foo </label>
        </div>
      </div>
    </div> |}];
  Bonsai.Var.update all_items_var ~f:(fun all_items -> Set.add all_items "quux");
  Bonsai.Var.set default_selection_status_var Unselected;
  Handle.show handle;
  [%expect
    {|
    Selected items: (bar baz foo)
    <div class="multi-select-container">
      <div class="multi-select-header"> header </div>
      <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
      <div class="multi-select-select-all-none">
        Select:
        <a href="about:blank" class="multi-select-select-all" onclick> all </a>
        ;
        <a href="about:blank" class="multi-select-select-none" onclick> none </a>
      </div>
      <div class="multi-select-checkboxes">
        <div class="multi-select-item multi-select-item-focused" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> bar </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> baz </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" checked="" #checked="true" onchange> </input>
          <label> foo </label>
        </div>
        <div class="multi-select-item" onclick>
          <input type="checkbox" #checked="false" onchange> </input>
          <label> quux </label>
        </div>
      </div>
    </div> |}]
;;

let%expect_test "html-custom-selected-attr" =
  let all_items_var                = Bonsai.Var.create all_items                   in
  let all_items                    = Bonsai.Var.value all_items_var                in
  let default_selection_status_var = Bonsai.Var.create S.Selection_status.Selected in
  let default_selection_status     = Bonsai.Var.value default_selection_status_var in
  let view_config =
    Bonsai.Value.return
      (S.View_config.create
         ~extra_row_attrs:(fun ~is_focused ->
           if is_focused
           then Vdom.Attr.class_ "another-focused-class"
           else Vdom.Attr.class_ "another-unfocused-class")
         ~header:(Vdom.Node.text "header")
         ~autofocus_search_box:false
         ())
  in
  let handle =
    handle ~view_config ~default_selection_status ~all_items ~which_display:`Html ()
  in
  Handle.store_view handle;
  Handle.do_actions handle [ Move_focus `Next ];
  Handle.show_diff  handle;
  [%expect
    {|
      Selected items: (bar baz foo)
      <div class="multi-select-container">
        <div class="multi-select-header"> header </div>
        <input type="text" placeholder="" spellcheck="false" value:normalized="" oninput> </input>
        <div class="multi-select-select-all-none">
          Select:
          <a href="about:blank" class="multi-select-select-all" onclick> all </a>
          ;
          <a href="about:blank" class="multi-select-select-none" onclick> none </a>
        </div>
        <div class="multi-select-checkboxes">
    -|    <div class="another-unfocused-class multi-select-item" onclick>
    +|    <div class="another-focused-class multi-select-item multi-select-item-focused" onclick>
            <input type="checkbox" checked="" #checked="true" onchange> </input>
            <label> bar </label>
          </div>
          <div class="another-unfocused-class multi-select-item" onclick>
            <input type="checkbox" checked="" #checked="true" onchange> </input>
            <label> baz </label>
          </div>
          <div class="another-unfocused-class multi-select-item" onclick>
            <input type="checkbox" checked="" #checked="true" onchange> </input>
            <label> foo </label>
          </div>
        </div>
      </div> |}]
;;
