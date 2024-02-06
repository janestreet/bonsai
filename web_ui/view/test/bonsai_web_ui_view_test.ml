open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax

(* Basic constructors *)

let themed_component ?filter_printed_attributes fs =
  (* add more themes to this list as they're built *)
  let themes = [ Bonsai_web.View.Expert.default_theme; Kado.theme ~version:V1 () ] in
  List.iter themes ~f:(fun theme ->
    print_endline ("# " ^ View.Theme.name theme);
    List.iter fs ~f:(fun (name, f) ->
      print_endline ("## " ^ name);
      let handle =
        Handle.create
          (Result_spec.vdom ?filter_printed_attributes Fn.id)
          (Bonsai.const (f theme))
      in
      print_endline "```html";
      Handle.show handle;
      print_endline "```\n"))
;;

let vdom_component vdoms =
  List.iter vdoms ~f:(fun (name, vdom) ->
    print_endline ("# " ^ name);
    let handle = Handle.create (Result_spec.vdom Fn.id) (Bonsai.const vdom) in
    print_endline "```html";
    Handle.show handle;
    print_endline "```\n")
;;

let bonsai_component c =
  let handle = Handle.create (Result_spec.vdom Fn.id) c in
  Handle.show handle
;;

let%expect_test "button" =
  let on_click = Effect.Ignore in
  let basic theme = View.button theme ~on_click "button label" in
  let disabled theme = View.button theme ~disabled:true ~on_click "button label" in
  let extra_attrs theme =
    View.button
      theme
      ~attrs:[ Vdom.Attr.on_double_click (fun _ -> on_click) ]
      ~on_click
      "button label"
  in
  themed_component [ "basic", basic; "disabled", disabled; "extra attrs", extra_attrs ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <button onclick> button label </button>
    ```

    ## disabled
    ```html
    <button disabled="" onclick> button label </button>
    ```

    ## extra attrs
    ```html
    <button onclick ondblclick> button label </button>
    ```

    # kado v1
    ## basic
    ```html
    <button class="btn_hash_replaced_in_test subtle_hash_replaced_in_test"
            custom-css-vars=((--disabled-stripes_hash_replaced_in_test #21242a)(--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #14181c))
            onclick
            style={
              background-color: #14181c;
              color: #e2e3e4;
            }> button label </button>
    ```

    ## disabled
    ```html
    <button disabled=""
            class="btn_hash_replaced_in_test subtle_hash_replaced_in_test"
            custom-css-vars=((--disabled-stripes_hash_replaced_in_test #21242a)(--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #14181c))
            onclick
            style={
              background-color: #14181c;
              color: #e2e3e4;
            }> button label </button>
    ```

    ## extra attrs
    ```html
    <button class="btn_hash_replaced_in_test subtle_hash_replaced_in_test"
            custom-css-vars=((--disabled-stripes_hash_replaced_in_test #21242a)(--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #14181c))
            onclick
            ondblclick
            style={
              background-color: #14181c;
              color: #e2e3e4;
            }> button label </button>
    ``` |}]
;;

let%expect_test "text" =
  let basic _ = View.text "basic text" in
  let with_attr _ = View.text ~attrs:[ Vdom.Attr.id "foo" ] "basic text" in
  let basic_f _ = View.textf "basic with %s format" "magic gadt" in
  themed_component
    [ "basic", basic; "disabled", with_attr; "basic with fomatting", basic_f ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <span> basic text </span>
    ```

    ## disabled
    ```html
    <span id="foo"> basic text </span>
    ```

    ## basic with fomatting
    ```html
    <span> basic with magic gadt format </span>
    ```

    # kado v1
    ## basic
    ```html
    <span> basic text </span>
    ```

    ## disabled
    ```html
    <span id="foo"> basic text </span>
    ```

    ## basic with fomatting
    ```html
    <span> basic with magic gadt format </span>
    ``` |}]
;;

let%expect_test "themed text" =
  let basic theme = View.themed_text theme "themed text" in
  let basic_f theme = View.themed_textf theme "themed text with %d format" 42 in
  let with_attr theme =
    View.themed_text theme ~attrs:[ Vdom.Attr.id "foo" ] "themed text"
  in
  let with_intents theme =
    View.Intent.all
    |> List.map ~f:(fun intent -> View.themed_text theme ~intent "themed text")
    |> View.vbox
  in
  themed_component
    [ "basic", basic
    ; "disabled", with_attr
    ; "with-intents", with_intents
    ; "basic with format", basic_f
    ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <span> themed text </span>
    ```

    ## disabled
    ```html
    <span id="foo"> themed text </span>
    ```

    ## with-intents
    ```html
    <div style={ display: flex; flex-direction: column; }>
      <span style={ text-decoration: underline dashed #0a90bf; text-underline-offset: 4px; }> themed text </span>
      <span style={ text-decoration: underline dashed #348203; text-underline-offset: 4px; }> themed text </span>
      <span style={
              text-decoration: underline dashed #6b6001;
              text-underline-offset: 4px;
              font-weight: bold;
              color: #6b6001;
            }> themed text </span>
      <span style={
              text-decoration: underline dashed #630100;
              text-underline-offset: 4px;
              font-weight: bold;
              color: #630100;
            }> themed text </span>
    </div>
    ```

    ## basic with format
    ```html
    <span> themed text with 42 format </span>
    ```

    # kado v1
    ## basic
    ```html
    <span> themed text </span>
    ```

    ## disabled
    ```html
    <span id="foo"> themed text </span>
    ```

    ## with-intents
    ```html
    <div style={ display: flex; flex-direction: column; }>
      <span style={ text-decoration: underline dashed #1BA1F2; text-underline-offset: 4px; }> themed text </span>
      <span style={ text-decoration: underline dashed #57c961; text-underline-offset: 4px; }> themed text </span>
      <span style={
              text-decoration: underline dashed #ffbe01;
              text-underline-offset: 4px;
              font-weight: bold;
              color: #ffbe01;
            }> themed text </span>
      <span style={
              text-decoration: underline dashed #f2581b;
              text-underline-offset: 4px;
              font-weight: bold;
              color: #f2581b;
            }> themed text </span>
    </div>
    ```

    ## basic with format
    ```html
    <span> themed text with 42 format </span>
    ``` |}]
;;

let%expect_test "button with tooltip" =
  let on_click = Effect.Ignore in
  let basic theme = View.button theme ~tooltip:"my tooltip" ~on_click "button label" in
  themed_component [ "basic-with-tooltip", basic ];
  [%expect
    {|
    # default theme
    ## basic-with-tooltip
    ```html
    <button title="my tooltip" onclick> button label </button>
    ```

    # kado v1
    ## basic-with-tooltip
    ```html
    <button title="my tooltip"
            class="btn_hash_replaced_in_test subtle_hash_replaced_in_test"
            custom-css-vars=((--disabled-stripes_hash_replaced_in_test #21242a)(--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #14181c))
            onclick
            style={
              background-color: #14181c;
              color: #e2e3e4;
            }> button label </button>
    ``` |}]
;;

let%expect_test "button w/ intents" =
  let on_click = Effect.Ignore in
  let all =
    List.concat_map View.Intent.all ~f:(fun intent ->
      let enabled theme = View.button theme ~intent ~on_click "button label" in
      let disabled theme =
        View.button theme ~intent ~disabled:true ~on_click "button label"
      in
      let name_of_intent = intent |> [%sexp_of: View.Intent.t] |> Sexp.to_string in
      [ sprintf "%s enabled" name_of_intent, enabled
      ; sprintf "%s disabled" name_of_intent, disabled
      ])
  in
  themed_component all;
  [%expect
    {|
    # default theme
    ## Info enabled
    ```html
    <button onclick style={ background-color: #e0f7ff; color: #0a90bf; }> button label </button>
    ```

    ## Info disabled
    ```html
    <button disabled=""
            onclick
            style={
              background-color: #e0f7ff;
              color: #0a90bf;
              opacity: 0.300000;
            }> button label </button>
    ```

    ## Success enabled
    ```html
    <button onclick style={ background-color: #ecffe0; color: #348203; }> button label </button>
    ```

    ## Success disabled
    ```html
    <button disabled=""
            onclick
            style={
              background-color: #ecffe0;
              color: #348203;
              opacity: 0.300000;
            }> button label </button>
    ```

    ## Warning enabled
    ```html
    <button onclick style={ background-color: #ffeb3b; color: #6b6001; }> button label </button>
    ```

    ## Warning disabled
    ```html
    <button disabled=""
            onclick
            style={
              background-color: #ffeb3b;
              color: #6b6001;
              opacity: 0.300000;
            }> button label </button>
    ```

    ## Error enabled
    ```html
    <button onclick style={ background-color: #ff2522; color: #630100; }> button label </button>
    ```

    ## Error disabled
    ```html
    <button disabled=""
            onclick
            style={
              background-color: #ff2522;
              color: #630100;
              opacity: 0.300000;
            }> button label </button>
    ```

    # kado v1
    ## Info enabled
    ```html
    <button class="btn_hash_replaced_in_test primary_hash_replaced_in_test"
            custom-css-vars=((--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #1BA1F2))
            onclick
            style={
              background-color: #1BA1F2;
              color: black;
            }> button label </button>
    ```

    ## Info disabled
    ```html
    <button disabled=""
            class="btn_hash_replaced_in_test primary_hash_replaced_in_test"
            custom-css-vars=((--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #1BA1F2))
            onclick
            style={
              background-color: #1BA1F2;
              color: black;
            }> button label </button>
    ```

    ## Success enabled
    ```html
    <button class="btn_hash_replaced_in_test safe_hash_replaced_in_test"
            custom-css-vars=((--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #57c961))
            onclick
            style={
              background-color: #57c961;
              color: black;
            }> button label </button>
    ```

    ## Success disabled
    ```html
    <button disabled=""
            class="btn_hash_replaced_in_test safe_hash_replaced_in_test"
            custom-css-vars=((--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #57c961))
            onclick
            style={
              background-color: #57c961;
              color: black;
            }> button label </button>
    ```

    ## Warning enabled
    ```html
    <button class="btn_hash_replaced_in_test warn_hash_replaced_in_test"
            custom-css-vars=((--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #ffbe01))
            onclick
            style={
              background-color: #ffbe01;
              color: black;
            }> button label </button>
    ```

    ## Warning disabled
    ```html
    <button disabled=""
            class="btn_hash_replaced_in_test warn_hash_replaced_in_test"
            custom-css-vars=((--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #ffbe01))
            onclick
            style={
              background-color: #ffbe01;
              color: black;
            }> button label </button>
    ```

    ## Error enabled
    ```html
    <button class="btn_hash_replaced_in_test danger_hash_replaced_in_test"
            custom-css-vars=((--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #f2581b))
            onclick
            style={
              background-color: #f2581b;
              color: white;
            }> button label </button>
    ```

    ## Error disabled
    ```html
    <button disabled=""
            class="btn_hash_replaced_in_test danger_hash_replaced_in_test"
            custom-css-vars=((--extreme_primary_border_hash_replaced_in_test #313943)(--btn-bg_hash_replaced_in_test #f2581b))
            onclick
            style={
              background-color: #f2581b;
              color: white;
            }> button label </button>
    ``` |}]
;;

let%expect_test "tooltip" =
  let basic theme = View.tooltip theme ~tooltip:"test" "hover me" in
  let arbitrary_content theme =
    View.tooltip' theme ~tooltip:(Vdom.Node.div []) (Vdom.Node.span [])
  in
  themed_component [ "basic", basic; "arbitrary_content", arbitrary_content ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <span class="tooltip_container_hash_replaced_in_test top_hash_replaced_in_test">
      hover me
      <div class="tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))> test </div>
    </span>
    ```

    ## arbitrary_content
    ```html
    <span class="tooltip_container_hash_replaced_in_test top_hash_replaced_in_test">
      <span> </span>
      <div class="tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <div> </div>
      </div>
    </span>
    ```

    # kado v1
    ## basic
    ```html
    <span class="tooltip_container_hash_replaced_in_test top_hash_replaced_in_test">
      hover me
      <div class="tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test #e2e3e4)(--border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #14181c))> test </div>
    </span>
    ```

    ## arbitrary_content
    ```html
    <span class="tooltip_container_hash_replaced_in_test top_hash_replaced_in_test">
      <span> </span>
      <div class="tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test #e2e3e4)(--border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #14181c))>
        <div> </div>
      </div>
    </span>
    ``` |}]
;;

let%expect_test "hbox_wrap" =
  let contents =
    [ Vdom.Node.div [ Vdom.Node.text "a" ]; Vdom.Node.div [ Vdom.Node.text "b" ] ]
  in
  let no_params = View.hbox_wrap contents in
  let just_gaps = View.hbox_wrap contents ~column_gap:(`Px 10) ~row_gap:(`Em 1) in
  let all_default_params =
    View.hbox_wrap
      ~align_content:Stretch
      ~direction:Left_to_right
      ~main_axis_alignment:Start
      ~cross_axis_alignment:Stretch
      contents
  in
  let all_different_params =
    View.hbox_wrap
      ~align_content:Center
      ~direction:Right_to_left
      ~main_axis_alignment:End
      ~cross_axis_alignment:Baseline
      contents
  in
  vdom_component
    [ "no params", no_params
    ; "all default params", all_default_params
    ; "all different params", all_different_params
    ; "just gaps", just_gaps
    ];
  [%expect
    {|
    # no params
    ```html
    <div style={ display: flex; flex-wrap: wrap; }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # all default params
    ```html
    <div style={
           display: flex;
           flex-direction: row;
           flex-wrap: wrap;
           align-items: stretch;
           align-content: stretch;
           justify-content: flex-start;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # all different params
    ```html
    <div style={
           display: flex;
           flex-direction: row-reverse;
           flex-wrap: wrap;
           align-items: baseline;
           align-content: center;
           justify-content: flex-end;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # just gaps
    ```html
    <div style={ display: flex; flex-wrap: wrap; column-gap: 10px; row-gap: 1em; }>
      <div> a </div>
      <div> b </div>
    </div>
    ``` |}]
;;

let%expect_test "vbox_wrap" =
  let contents =
    [ Vdom.Node.div [ Vdom.Node.text "a" ]; Vdom.Node.div [ Vdom.Node.text "b" ] ]
  in
  let no_params = View.vbox_wrap contents in
  let just_gaps = View.vbox_wrap contents ~column_gap:(`Px 10) ~row_gap:(`Em 1) in
  let all_default_params =
    View.vbox_wrap
      ~align_content:Stretch
      ~direction:Top_to_bottom
      ~main_axis_alignment:Start
      ~cross_axis_alignment:Stretch
      contents
  in
  let all_different_params =
    View.vbox_wrap
      ~align_content:Center
      ~direction:Bottom_to_top
      ~main_axis_alignment:End
      ~cross_axis_alignment:Baseline
      contents
  in
  vdom_component
    [ "no params", no_params
    ; "all default params", all_default_params
    ; "all different params", all_different_params
    ; "just gaps", just_gaps
    ];
  [%expect
    {|
    # no params
    ```html
    <div style={ display: flex; flex-direction: column; flex-wrap: wrap; }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # all default params
    ```html
    <div style={
           display: flex;
           flex-direction: column;
           flex-wrap: wrap;
           align-items: stretch;
           align-content: stretch;
           justify-content: flex-start;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # all different params
    ```html
    <div style={
           display: flex;
           flex-direction: column-reverse;
           flex-wrap: wrap;
           align-items: baseline;
           align-content: center;
           justify-content: flex-end;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # just gaps
    ```html
    <div style={
           display: flex;
           flex-direction: column;
           flex-wrap: wrap;
           column-gap: 10px;
           row-gap: 1em;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ``` |}]
;;

let%expect_test "hbox" =
  let contents =
    [ Vdom.Node.div [ Vdom.Node.text "a" ]; Vdom.Node.div [ Vdom.Node.text "b" ] ]
  in
  let no_params = View.hbox contents in
  let just_gaps = View.hbox contents ~gap:(`Px 10) in
  let all_default_params =
    View.hbox
      ~direction:Left_to_right
      ~main_axis_alignment:Start
      ~cross_axis_alignment:Stretch
      contents
  in
  let all_different_params =
    View.hbox
      ~direction:Right_to_left
      ~main_axis_alignment:End
      ~cross_axis_alignment:Baseline
      contents
  in
  vdom_component
    [ "no params", no_params
    ; "all default params", all_default_params
    ; "all different params", all_different_params
    ; "just gaps", just_gaps
    ];
  [%expect
    {|
    # no params
    ```html
    <div style={ display: flex; }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # all default params
    ```html
    <div style={
           display: flex;
           flex-direction: row;
           align-items: stretch;
           justify-content: flex-start;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # all different params
    ```html
    <div style={
           display: flex;
           flex-direction: row-reverse;
           align-items: baseline;
           justify-content: flex-end;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # just gaps
    ```html
    <div style={ display: flex; column-gap: 10px; }>
      <div> a </div>
      <div> b </div>
    </div>
    ``` |}]
;;

let%expect_test "vbox" =
  let contents =
    [ Vdom.Node.div [ Vdom.Node.text "a" ]; Vdom.Node.div [ Vdom.Node.text "b" ] ]
  in
  let just_gaps = View.vbox contents ~gap:(`Px 10) in
  let no_params = View.vbox contents in
  let all_default_params =
    View.vbox
      ~direction:Top_to_bottom
      ~main_axis_alignment:Start
      ~cross_axis_alignment:Stretch
      contents
  in
  let all_different_params =
    View.vbox
      ~direction:Bottom_to_top
      ~main_axis_alignment:End
      ~cross_axis_alignment:Baseline
      contents
  in
  vdom_component
    [ "no params", no_params
    ; "all default params", all_default_params
    ; "all different params", all_different_params
    ; "just gaps", just_gaps
    ];
  [%expect
    {|
    # no params
    ```html
    <div style={ display: flex; flex-direction: column; }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # all default params
    ```html
    <div style={
           display: flex;
           flex-direction: column;
           align-items: stretch;
           justify-content: flex-start;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # all different params
    ```html
    <div style={
           display: flex;
           flex-direction: column-reverse;
           align-items: baseline;
           justify-content: flex-end;
         }>
      <div> a </div>
      <div> b </div>
    </div>
    ```

    # just gaps
    ```html
    <div style={ display: flex; flex-direction: column; row-gap: 10px; }>
      <div> a </div>
      <div> b </div>
    </div>
    ``` |}]
;;

let%expect_test "current theme" =
  let c =
    let%sub theme = View.Theme.current in
    let%arr theme = theme in
    Vdom.Node.text (View.Theme.name theme)
  in
  bonsai_component c;
  [%expect {| default theme |}]
;;

let%expect_test "override constants" =
  let inside =
    let%sub theme = View.Theme.current in
    let%arr theme = theme in
    Vdom.Node.text ((View.primary_colors theme).background |> Css_gen.Color.to_string_css)
  in
  bonsai_component inside;
  [%expect {| white |}];
  bonsai_component
    (View.Theme.override_constants_for_computation inside ~f:(fun consts ->
       { consts with primary = { consts.primary with background = `Name "red" } }));
  [%expect {| red |}]
;;

let%expect_test "override whole theme" =
  let inside =
    let%sub theme = View.Theme.current in
    let%arr theme = theme in
    Vdom.Node.text (View.Theme.name theme)
  in
  bonsai_component inside;
  [%expect {| default theme |}];
  bonsai_component
    (View.Expert.override_theme_for_computation inside ~f:(fun (module M) ->
       (module struct
         class c =
           object
             inherit M.c as super

             method! theme_name =
               let super_name = super#theme_name in
               [%string "great new theme (super theme:  %{super_name})"]
           end
       end)));
  [%expect {| great new theme (super theme:  default theme) |}]
;;

let%expect_test "text w/ intents" =
  let all =
    List.map View.Intent.all ~f:(fun intent ->
      let view theme = View.themed_text theme ~intent "hello world" in
      let name_of_intent = intent |> [%sexp_of: View.Intent.t] |> Sexp.to_string in
      name_of_intent, view)
  in
  themed_component all;
  [%expect
    {|
    # default theme
    ## Info
    ```html
    <span style={ text-decoration: underline dashed #0a90bf; text-underline-offset: 4px; }> hello world </span>
    ```

    ## Success
    ```html
    <span style={ text-decoration: underline dashed #348203; text-underline-offset: 4px; }> hello world </span>
    ```

    ## Warning
    ```html
    <span style={
            text-decoration: underline dashed #6b6001;
            text-underline-offset: 4px;
            font-weight: bold;
            color: #6b6001;
          }> hello world </span>
    ```

    ## Error
    ```html
    <span style={
            text-decoration: underline dashed #630100;
            text-underline-offset: 4px;
            font-weight: bold;
            color: #630100;
          }> hello world </span>
    ```

    # kado v1
    ## Info
    ```html
    <span style={ text-decoration: underline dashed #1BA1F2; text-underline-offset: 4px; }> hello world </span>
    ```

    ## Success
    ```html
    <span style={ text-decoration: underline dashed #57c961; text-underline-offset: 4px; }> hello world </span>
    ```

    ## Warning
    ```html
    <span style={
            text-decoration: underline dashed #ffbe01;
            text-underline-offset: 4px;
            font-weight: bold;
            color: #ffbe01;
          }> hello world </span>
    ```

    ## Error
    ```html
    <span style={
            text-decoration: underline dashed #f2581b;
            text-underline-offset: 4px;
            font-weight: bold;
            color: #f2581b;
          }> hello world </span>
    ``` |}]
;;

let%expect_test "tabs" =
  let basic theme =
    View.tabs
      theme
      ~equal:[%equal: int]
      ~on_change:(fun ~from:_ ~to_:_ -> Effect.Ignore)
      ~active:0
      [ 0, Vdom.Node.text "home"
      ; 1, Vdom.Node.text "about"
      ; 2, Vdom.Node.text "user preferences"
      ]
  in
  let customization theme =
    let grey_background = Vdom.Attr.style (Css_gen.background_color (`Name "grey")) in
    let red_border =
      Vdom.Attr.style
        (Css_gen.border_bottom ~width:(`Px 3) ~color:(`Name "red") ~style:`Solid ())
    in
    View.tabs
      theme
      ~attrs:[ grey_background ]
      ~per_tab_attrs:(fun _i ~is_active -> if is_active then [ red_border ] else [])
      ~on_change:(fun ~from:_ ~to_:_ -> Effect.Ignore)
      ~equal:[%equal: int]
      ~active:0
      [ 0, Vdom.Node.text "home"
      ; 1, Vdom.Node.text "about"
      ; 2, Vdom.Node.text "user preferences"
      ]
  in
  themed_component [ "basic", basic; "customization", customization ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <div style={ display: flex; column-gap: 0.50em; }>
      <div style={ border-bottom: 3px solid black; cursor: pointer; }> home </div>
      <div onclick style={ border-bottom: 1px solid black; opacity: 0.600000; cursor: pointer; }> about </div>
      <div onclick style={ border-bottom: 1px solid black; opacity: 0.600000; cursor: pointer; }> user preferences </div>
    </div>
    ```

    ## customization
    ```html
    <div style={ display: flex; column-gap: 0.50em; background-color: grey; }>
      <div style={ border-bottom: 3px solid red; cursor: pointer; }> home </div>
      <div onclick style={ border-bottom: 1px solid black; opacity: 0.600000; cursor: pointer; }> about </div>
      <div onclick style={ border-bottom: 1px solid black; opacity: 0.600000; cursor: pointer; }> user preferences </div>
    </div>
    ```

    # kado v1
    ## basic
    ```html
    <div class="tab_container_hash_replaced_in_test" style={ display: flex; column-gap: 0.50em; }>
      <div class="selected_hash_replaced_in_test tab_button_hash_replaced_in_test" onclick> home </div>
      <div class="tab_button_hash_replaced_in_test" onclick> about </div>
      <div class="tab_button_hash_replaced_in_test" onclick> user preferences </div>
    </div>
    ```

    ## customization
    ```html
    <div class="tab_container_hash_replaced_in_test"
         style={
           display: flex;
           column-gap: 0.50em;
           background-color: grey;
         }>
      <div class="selected_hash_replaced_in_test tab_button_hash_replaced_in_test"
           onclick
           style={
             border-bottom: 3px solid red;
           }> home </div>
      <div class="tab_button_hash_replaced_in_test" onclick> about </div>
      <div class="tab_button_hash_replaced_in_test" onclick> user preferences </div>
    </div>
    ``` |}]
;;

let%expect_test "devbar" =
  let basic theme = View.devbar theme ~count:4 "dev" in
  let with_intent theme = View.devbar theme ~count:4 ~intent:Success "dev" in
  themed_component [ "basic", basic; "with_intent", with_intent ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <div style={ display: flex; max-width: 100.00%; overflow: hidden; }>
      <span style={
              color: white;
              background-color: black;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
      <span style={
              color: #630100;
              background-color: #ff2522;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
      <span style={
              color: white;
              background-color: black;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
      <span style={
              color: #630100;
              background-color: #ff2522;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
    </div>
    ```

    ## with_intent
    ```html
    <div style={ display: flex; max-width: 100.00%; overflow: hidden; }>
      <span style={
              color: white;
              background-color: black;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
      <span style={
              color: #348203;
              background-color: #ecffe0;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
      <span style={
              color: white;
              background-color: black;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
      <span style={
              color: #348203;
              background-color: #ecffe0;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
    </div>
    ```

    # kado v1
    ## basic
    ```html
    <div class="dark_hash_replaced_in_test devbar_hash_replaced_in_test">
      <div class="container_hash_replaced_in_test">
        <div class="background_hash_replaced_in_test"> </div>
        <span> dev </span>
        <span> dev </span>
        <span> dev </span>
        <span> dev </span>
      </div>
    </div>
    ```

    ## with_intent
    ```html
    <div class="dark_hash_replaced_in_test devbar_hash_replaced_in_test"
         custom-css-vars=((--snd_hash_replaced_in_test #57c961)(--fst_hash_replaced_in_test black))>
      <div class="container_hash_replaced_in_test">
        <div class="background_hash_replaced_in_test"> </div>
        <span> dev </span>
        <span> dev </span>
        <span> dev </span>
        <span> dev </span>
      </div>
    </div>
    ``` |}]
;;

let%expect_test "app" =
  let basic theme = Vdom.Node.div ~attrs:[ force (View.App.top_attr theme) ] [] in
  themed_component [ "basic", basic ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <div inline-css-dynamic="\n@layer bonsai_web_ui_view.app {\n  :root {\n    font-family: sans-serif;\n  }\n\n  :root *,\n  :root *::before,\n  :root *::after {\n    box-sizing: border-box;\n  }\n}\n"> </div>
    ```

    # kado v1
    ## basic
    ```html
    <div class="app_hash_replaced_in_test dark_hash_replaced_in_test"
         custom-css-vars=((--bg_hash_replaced_in_test #1a1d21))> </div>
    ``` |}]
;;

let%expect_test "app wrapper" =
  let component ~f =
    let theme = Value.return (Kado.theme ~version:Bleeding ()) in
    View.Theme.set_for_app
      theme
      (let%sub theme = View.Theme.current in
       let%arr theme = theme in
       f (View.text (View.Theme.name theme)))
  in
  bonsai_component (component ~f:Fn.id);
  [%expect
    {|
      <span class="app_hash_replaced_in_test dark_hash_replaced_in_test"
            custom-css-vars=((--bg_hash_replaced_in_test #1a1d21))> kado v1 </span> |}];
  bonsai_component (component ~f:(fun vdom -> Vdom.Node.div [ vdom ]));
  [%expect
    {|
    <div class="app_hash_replaced_in_test dark_hash_replaced_in_test"
         custom-css-vars=((--bg_hash_replaced_in_test #1a1d21))>
      <span> kado v1 </span>
    </div> |}];
  bonsai_component (component ~f:(fun vdom -> Vdom.Node.lazy_ (lazy vdom)));
  [%expect
    {|
      <span class="app_hash_replaced_in_test dark_hash_replaced_in_test"
            custom-css-vars=((--bg_hash_replaced_in_test #1a1d21))> kado v1 </span> |}];
  bonsai_component (component ~f:(fun _ -> Vdom.Node.none));
  [%expect
    {|
      <div class="app_hash_replaced_in_test dark_hash_replaced_in_test"
           custom-css-vars=((--bg_hash_replaced_in_test #1a1d21))> </div> |}];
  bonsai_component
    (component ~f:(fun _vdom ->
       Vdom.Node.inner_html
         ()
         ~tag:"foo"
         ~attrs:[]
         ~this_html_is_sanitized_and_is_totally_safe_trust_me:"content"));
  [%expect
    {|
    <div class="app_hash_replaced_in_test dark_hash_replaced_in_test"
         custom-css-vars=((--bg_hash_replaced_in_test #1a1d21))>
      <foo> content </foo>
    </div> |}]
;;

let%expect_test "colors" =
  let basic theme =
    let to_s c = Css_gen.Color.to_string_css c in
    let fg_bg { View.Fg_bg.foreground; background } = function
      | `Fg -> to_s foreground
      | `Bg -> to_s background
    in
    let intent intent which = fg_bg (View.intent_colors theme intent) which in
    Vdom.Node.text
      (String.strip
         [%string
           {|
primary:
  fg: %{fg_bg (View.primary_colors theme) `Fg}
  bg: %{fg_bg (View.primary_colors theme) `Bg}
extreme:
  fg: %{fg_bg (View.extreme_colors theme) `Fg}
  bg: %{fg_bg (View.extreme_colors theme) `Bg}
extreme-primary-border: %{to_s (View.extreme_primary_border_color theme) }
info:
  fg: %{intent Info `Fg}
  bg: %{intent Info `Bg}
success:
  fg: %{intent Success `Fg}
  bg: %{intent Success `Bg}
warning:
  fg: %{intent Warning `Fg}
  bg: %{intent Warning `Bg}
error:
  fg: %{intent Error `Fg}
  bg: %{intent Error `Bg}
    |}])
  in
  themed_component [ "basic", basic ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    primary:
      fg: black
      bg: white
    extreme:
      fg: black
      bg: white
    extreme-primary-border: grey
    info:
      fg: #0a90bf
      bg: #e0f7ff
    success:
      fg: #348203
      bg: #ecffe0
    warning:
      fg: #6b6001
      bg: #ffeb3b
    error:
      fg: #630100
      bg: #ff2522
    ```

    # kado v1
    ## basic
    ```html
    primary:
      fg: #d1d2d3
      bg: #1a1d21
    extreme:
      fg: #e2e3e4
      bg: #14181c
    extreme-primary-border: #313943
    info:
      fg: black
      bg: #1BA1F2
    success:
      fg: black
      bg: #57c961
    warning:
      fg: black
      bg: #ffbe01
    error:
      fg: white
      bg: #f2581b
    ``` |}]
;;

let%expect_test "codemirror theme" =
  let basic theme =
    let cm = View.For_components.Codemirror.theme theme in
    Vdom.Node.sexp_for_debugging
      ([%sexp_of: View.Expert.For_codemirror.Theme.t option] cm)
  in
  themed_component [ "basic", basic ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <pre> () </pre>
    ```

    # kado v1
    ## basic
    ```html
    <pre> (Nord) </pre>
    ``` |}]
;;

let%test_module "tables" =
  (module struct
    type t =
      { sym : string
      ; price : float
      ; trader : string option
      }
    [@@deriving fields ~getters]

    let data =
      [ { sym = "aapl"; price = 1.0; trader = None }
      ; { sym = "msft"; price = 8.2; trader = Some "ty" }
      ; { sym = "tsla"; price = 3.3; trader = None }
      ]
    ;;

    let%expect_test "table" =
      let basic theme =
        let columns =
          let render_text _ string = View.text string in
          let render_float _ = Vdom.Node.textf "%.3f" in
          [ View.Table.Col.make "symbol" ~get:sym ~render:render_text
          ; View.Table.Col.make "price" ~get:price ~render:render_float
          ; View.Table.Col.make_opt "trader" ~get:trader ~render:render_text
          ]
        in
        View.Table.render theme columns data
      in
      let column_groups theme =
        let columns =
          let render_text _ string = View.text string in
          let render_float _ = Vdom.Node.textf "%.3f" in
          [ View.Table.Col.make "sym" ~get:sym ~render:render_text
          ; View.Table.Col.group
              "prices"
              [ View.Table.Col.make "bid" ~get:price ~render:render_float
              ; View.Table.Col.make
                  "ask"
                  ~get:(fun x -> price x +. 5.0)
                  ~render:render_float
              ]
          ]
        in
        View.Table.render theme columns []
      in
      themed_component [ "basic", basic; "column groups", column_groups ];
      [%expect
        {|
    # default theme
    ## basic
    ```html
    <table class="table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
      <thead class="header_hash_replaced_in_test">
        <tr class="header_row_hash_replaced_in_test">
          <th colspan="1" class="header_cell_hash_replaced_in_test"> symbol </th>
          <th colspan="1" class="header_cell_hash_replaced_in_test"> price </th>
          <th colspan="1" class="header_cell_hash_replaced_in_test"> trader </th>
        </tr>
      </thead>
      <tbody class="body_hash_replaced_in_test">
        <tr class="body_row_hash_replaced_in_test">
          <td class="body_cell_hash_replaced_in_test">
            <span> aapl </span>
          </td>
          <td class="body_cell_hash_replaced_in_test"> 1.000 </td>
          <td class="body_cell_hash_replaced_in_test empty_hash_replaced_in_test"> </td>
        </tr>
        <tr class="body_row_hash_replaced_in_test">
          <td class="body_cell_hash_replaced_in_test">
            <span> msft </span>
          </td>
          <td class="body_cell_hash_replaced_in_test"> 8.200 </td>
          <td class="body_cell_hash_replaced_in_test">
            <span> ty </span>
          </td>
        </tr>
        <tr class="body_row_hash_replaced_in_test">
          <td class="body_cell_hash_replaced_in_test">
            <span> tsla </span>
          </td>
          <td class="body_cell_hash_replaced_in_test"> 3.300 </td>
          <td class="body_cell_hash_replaced_in_test empty_hash_replaced_in_test"> </td>
        </tr>
      </tbody>
    </table>
    ```

    ## column groups
    ```html
    <table class="table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
      <thead class="header_hash_replaced_in_test">
        <tr class="header_row_hash_replaced_in_test">
          <th colspan="1" class="header_cell_hash_replaced_in_test"> </th>
          <th colspan="2" class="header_cell_hash_replaced_in_test"> prices </th>
        </tr>
        <tr class="header_row_hash_replaced_in_test">
          <th colspan="1" class="header_cell_hash_replaced_in_test"> sym </th>
          <th colspan="1" class="header_cell_hash_replaced_in_test"> bid </th>
          <th colspan="1" class="header_cell_hash_replaced_in_test"> ask </th>
        </tr>
      </thead>
      <tbody class="body_hash_replaced_in_test"> </tbody>
    </table>
    ```

    # kado v1
    ## basic
    ```html
    <table class="table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test #d1d2d3)(--row-odd-bg_hash_replaced_in_test #1a1d21)(--row-even-fg_hash_replaced_in_test #d1d2d3)(--row-even-bg_hash_replaced_in_test #21242a)(--header-header-border_hash_replaced_in_test #313943)(--header-fg_hash_replaced_in_test #d1d2d3)(--header-body-border_hash_replaced_in_test #313943)(--header-bg_hash_replaced_in_test #0b0e11)(--fg_hash_replaced_in_test #d1d2d3)(--body-body-border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))>
      <thead class="header_hash_replaced_in_test">
        <tr class="header_row_hash_replaced_in_test">
          <th colspan="1" class="header_cell_hash_replaced_in_test"> symbol </th>
          <th colspan="1" class="header_cell_hash_replaced_in_test"> price </th>
          <th colspan="1" class="header_cell_hash_replaced_in_test"> trader </th>
        </tr>
      </thead>
      <tbody class="body_hash_replaced_in_test">
        <tr class="body_row_hash_replaced_in_test">
          <td class="body_cell_hash_replaced_in_test">
            <span> aapl </span>
          </td>
          <td class="body_cell_hash_replaced_in_test"> 1.000 </td>
          <td class="body_cell_hash_replaced_in_test empty_hash_replaced_in_test"> </td>
        </tr>
        <tr class="body_row_hash_replaced_in_test">
          <td class="body_cell_hash_replaced_in_test">
            <span> msft </span>
          </td>
          <td class="body_cell_hash_replaced_in_test"> 8.200 </td>
          <td class="body_cell_hash_replaced_in_test">
            <span> ty </span>
          </td>
        </tr>
        <tr class="body_row_hash_replaced_in_test">
          <td class="body_cell_hash_replaced_in_test">
            <span> tsla </span>
          </td>
          <td class="body_cell_hash_replaced_in_test"> 3.300 </td>
          <td class="body_cell_hash_replaced_in_test empty_hash_replaced_in_test"> </td>
        </tr>
      </tbody>
    </table>
    ```

    ## column groups
    ```html
    <table class="table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test #d1d2d3)(--row-odd-bg_hash_replaced_in_test #1a1d21)(--row-even-fg_hash_replaced_in_test #d1d2d3)(--row-even-bg_hash_replaced_in_test #21242a)(--header-header-border_hash_replaced_in_test #313943)(--header-fg_hash_replaced_in_test #d1d2d3)(--header-body-border_hash_replaced_in_test #313943)(--header-bg_hash_replaced_in_test #0b0e11)(--fg_hash_replaced_in_test #d1d2d3)(--body-body-border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))>
      <thead class="header_hash_replaced_in_test">
        <tr class="header_row_hash_replaced_in_test">
          <th colspan="1" class="header_cell_hash_replaced_in_test"> </th>
          <th colspan="2" class="header_cell_hash_replaced_in_test"> prices </th>
        </tr>
        <tr class="header_row_hash_replaced_in_test">
          <th colspan="1" class="header_cell_hash_replaced_in_test"> sym </th>
          <th colspan="1" class="header_cell_hash_replaced_in_test"> bid </th>
          <th colspan="1" class="header_cell_hash_replaced_in_test"> ask </th>
        </tr>
      </thead>
      <tbody class="body_hash_replaced_in_test"> </tbody>
    </table>
    ``` |}]
    ;;

    let%expect_test "table" =
      let empty_group theme =
        let columns =
          let render_text _ string = View.text string in
          [ View.Table.Col.group "oh no" []
          ; View.Table.Col.make_opt "trader" ~get:trader ~render:render_text
          ; View.Table.Col.group "oh no" []
          ]
        in
        View.Table.render theme columns data
      in
      let no_columns theme = View.Table.render theme [] data in
      themed_component [ "basic", empty_group; "no columns", no_columns ];
      [%expect
        {|
        # default theme
        ## basic
        ```html
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
          <thead class="header_hash_replaced_in_test">
            <tr class="header_row_hash_replaced_in_test">
              <th colspan="1" class="header_cell_hash_replaced_in_test"> trader </th>
            </tr>
          </thead>
          <tbody class="body_hash_replaced_in_test">
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test empty_hash_replaced_in_test"> </td>
            </tr>
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test">
                <span> ty </span>
              </td>
            </tr>
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test empty_hash_replaced_in_test"> </td>
            </tr>
          </tbody>
        </table>
        ```

        ## no columns
        ```html
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
          <thead class="header_hash_replaced_in_test"> </thead>
          <tbody class="body_hash_replaced_in_test">
            <tr class="body_row_hash_replaced_in_test"> </tr>
            <tr class="body_row_hash_replaced_in_test"> </tr>
            <tr class="body_row_hash_replaced_in_test"> </tr>
          </tbody>
        </table>
        ```

        # kado v1
        ## basic
        ```html
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test #d1d2d3)(--row-odd-bg_hash_replaced_in_test #1a1d21)(--row-even-fg_hash_replaced_in_test #d1d2d3)(--row-even-bg_hash_replaced_in_test #21242a)(--header-header-border_hash_replaced_in_test #313943)(--header-fg_hash_replaced_in_test #d1d2d3)(--header-body-border_hash_replaced_in_test #313943)(--header-bg_hash_replaced_in_test #0b0e11)(--fg_hash_replaced_in_test #d1d2d3)(--body-body-border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))>
          <thead class="header_hash_replaced_in_test">
            <tr class="header_row_hash_replaced_in_test">
              <th colspan="1" class="header_cell_hash_replaced_in_test"> trader </th>
            </tr>
          </thead>
          <tbody class="body_hash_replaced_in_test">
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test empty_hash_replaced_in_test"> </td>
            </tr>
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test">
                <span> ty </span>
              </td>
            </tr>
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test empty_hash_replaced_in_test"> </td>
            </tr>
          </tbody>
        </table>
        ```

        ## no columns
        ```html
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test #d1d2d3)(--row-odd-bg_hash_replaced_in_test #1a1d21)(--row-even-fg_hash_replaced_in_test #d1d2d3)(--row-even-bg_hash_replaced_in_test #21242a)(--header-header-border_hash_replaced_in_test #313943)(--header-fg_hash_replaced_in_test #d1d2d3)(--header-body-border_hash_replaced_in_test #313943)(--header-bg_hash_replaced_in_test #0b0e11)(--fg_hash_replaced_in_test #d1d2d3)(--body-body-border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))>
          <thead class="header_hash_replaced_in_test"> </thead>
          <tbody class="body_hash_replaced_in_test">
            <tr class="body_row_hash_replaced_in_test"> </tr>
            <tr class="body_row_hash_replaced_in_test"> </tr>
            <tr class="body_row_hash_replaced_in_test"> </tr>
          </tbody>
        </table>
        ``` |}]
    ;;

    let%expect_test "lift" =
      let empty_group theme =
        let columns =
          let render_float _ float = View.textf "%.4f" float in
          let price_col = View.Table.Col.make "price" ~get:price ~render:render_float in
          [ price_col
          ; View.Table.Col.lift
              (View.Table.Col.make "doubled price" ~get:Fn.id ~render:render_float)
              ~f:(fun x -> x.price *. 2.0)
          ]
        in
        View.Table.render theme columns data
      in
      let no_columns theme = View.Table.render theme [] data in
      themed_component [ "basic", empty_group; "no columns", no_columns ];
      [%expect
        {|
        # default theme
        ## basic
        ```html
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
          <thead class="header_hash_replaced_in_test">
            <tr class="header_row_hash_replaced_in_test">
              <th colspan="1" class="header_cell_hash_replaced_in_test"> price </th>
              <th colspan="1" class="header_cell_hash_replaced_in_test"> doubled price </th>
            </tr>
          </thead>
          <tbody class="body_hash_replaced_in_test">
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test">
                <span> 1.0000 </span>
              </td>
              <td class="body_cell_hash_replaced_in_test">
                <span> 2.0000 </span>
              </td>
            </tr>
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test">
                <span> 8.2000 </span>
              </td>
              <td class="body_cell_hash_replaced_in_test">
                <span> 16.4000 </span>
              </td>
            </tr>
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test">
                <span> 3.3000 </span>
              </td>
              <td class="body_cell_hash_replaced_in_test">
                <span> 6.6000 </span>
              </td>
            </tr>
          </tbody>
        </table>
        ```

        ## no columns
        ```html
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
          <thead class="header_hash_replaced_in_test"> </thead>
          <tbody class="body_hash_replaced_in_test">
            <tr class="body_row_hash_replaced_in_test"> </tr>
            <tr class="body_row_hash_replaced_in_test"> </tr>
            <tr class="body_row_hash_replaced_in_test"> </tr>
          </tbody>
        </table>
        ```

        # kado v1
        ## basic
        ```html
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test #d1d2d3)(--row-odd-bg_hash_replaced_in_test #1a1d21)(--row-even-fg_hash_replaced_in_test #d1d2d3)(--row-even-bg_hash_replaced_in_test #21242a)(--header-header-border_hash_replaced_in_test #313943)(--header-fg_hash_replaced_in_test #d1d2d3)(--header-body-border_hash_replaced_in_test #313943)(--header-bg_hash_replaced_in_test #0b0e11)(--fg_hash_replaced_in_test #d1d2d3)(--body-body-border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))>
          <thead class="header_hash_replaced_in_test">
            <tr class="header_row_hash_replaced_in_test">
              <th colspan="1" class="header_cell_hash_replaced_in_test"> price </th>
              <th colspan="1" class="header_cell_hash_replaced_in_test"> doubled price </th>
            </tr>
          </thead>
          <tbody class="body_hash_replaced_in_test">
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test">
                <span> 1.0000 </span>
              </td>
              <td class="body_cell_hash_replaced_in_test">
                <span> 2.0000 </span>
              </td>
            </tr>
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test">
                <span> 8.2000 </span>
              </td>
              <td class="body_cell_hash_replaced_in_test">
                <span> 16.4000 </span>
              </td>
            </tr>
            <tr class="body_row_hash_replaced_in_test">
              <td class="body_cell_hash_replaced_in_test">
                <span> 3.3000 </span>
              </td>
              <td class="body_cell_hash_replaced_in_test">
                <span> 6.6000 </span>
              </td>
            </tr>
          </tbody>
        </table>
        ```

        ## no columns
        ```html
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test #d1d2d3)(--row-odd-bg_hash_replaced_in_test #1a1d21)(--row-even-fg_hash_replaced_in_test #d1d2d3)(--row-even-bg_hash_replaced_in_test #21242a)(--header-header-border_hash_replaced_in_test #313943)(--header-fg_hash_replaced_in_test #d1d2d3)(--header-body-border_hash_replaced_in_test #313943)(--header-bg_hash_replaced_in_test #0b0e11)(--fg_hash_replaced_in_test #d1d2d3)(--body-body-border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))>
          <thead class="header_hash_replaced_in_test"> </thead>
          <tbody class="body_hash_replaced_in_test">
            <tr class="body_row_hash_replaced_in_test"> </tr>
            <tr class="body_row_hash_replaced_in_test"> </tr>
            <tr class="body_row_hash_replaced_in_test"> </tr>
          </tbody>
        </table>
        ``` |}]
    ;;

    let%expect_test "table with extras" =
      let test_attr name = [ Vdom.Attr.create "test" name ] in
      let test_attr' key value = [ Vdom.Attr.create ("test-" ^ key) value ] in
      let basic theme =
        let columns =
          let render_text _ string = View.text string in
          [ View.Table.Col.make
              "trader"
              ~cell_attrs:(test_attr' "cell")
              ~header_attrs:(test_attr "header")
              ~get:sym
              ~render:render_text
          ]
        in
        View.Table.render
          ~row_attrs:(fun { sym; _ } -> test_attr' "row" sym)
          ~table_attrs:(test_attr "table")
          theme
          columns
          data
      in
      themed_component
        ~filter_printed_attributes:(fun ~key:k ~data:_ ->
          String.is_prefix k ~prefix:"test")
        [ "basic", basic ];
      [%expect
        {|
        # default theme
        ## basic
        ```html
        <table test="table">
          <thead>
            <tr>
              <th test="header"> trader </th>
            </tr>
          </thead>
          <tbody>
            <tr test-row="aapl">
              <td test-cell="aapl">
                <span> aapl </span>
              </td>
            </tr>
            <tr test-row="msft">
              <td test-cell="msft">
                <span> msft </span>
              </td>
            </tr>
            <tr test-row="tsla">
              <td test-cell="tsla">
                <span> tsla </span>
              </td>
            </tr>
          </tbody>
        </table>
        ```

        # kado v1
        ## basic
        ```html
        <table test="table">
          <thead>
            <tr>
              <th test="header"> trader </th>
            </tr>
          </thead>
          <tbody>
            <tr test-row="aapl">
              <td test-cell="aapl">
                <span> aapl </span>
              </td>
            </tr>
            <tr test-row="msft">
              <td test-cell="msft">
                <span> msft </span>
              </td>
            </tr>
            <tr test-row="tsla">
              <td test-cell="tsla">
                <span> tsla </span>
              </td>
            </tr>
          </tbody>
        </table>
        ``` |}]
    ;;
  end)
;;

let%expect_test "card" =
  let basic theme =
    View.card theme ~on_click:Effect.Ignore ~title:"new message" "new message content"
  in
  let basic_no_title theme =
    View.card theme ~on_click:Effect.Ignore "new message content"
  in
  let with_intent theme =
    View.card
      theme
      ~on_click:Effect.Ignore
      ~intent:Success
      ~title:"title"
      "new message that happens to be happy"
  in
  let with_field_set theme =
    View.card theme ~on_click:Effect.Ignore ~title:"title" "Card from a fieldset"
  in
  themed_component
    [ "basic", basic
    ; "basic_no_title", basic_no_title
    ; "with_intent", with_intent
    ; "with_field_set", with_field_set
    ];
  [%expect
    {|
    # default theme
    ## basic
    ```html
    <div class="container_hash_replaced_in_test"
         custom-css-vars=((--intent-fg_hash_replaced_in_test black)(--intent-bg_hash_replaced_in_test white)(--extreme-fg_hash_replaced_in_test black)(--extreme-bg_hash_replaced_in_test white))
         onclick
         style={
           display: flex;
           flex-direction: column;
         }>
      <div class="title-bar_hash_replaced_in_test title-text_hash_replaced_in_test"
           style={
             display: flex;
           }> new message </div>
      <div class="content_common_hash_replaced_in_test"
           style={
             display: flex;
             flex-direction: column;
           }> new message content </div>
    </div>
    ```

    ## basic_no_title
    ```html
    <div class="container_hash_replaced_in_test"
         custom-css-vars=((--intent-fg_hash_replaced_in_test black)(--intent-bg_hash_replaced_in_test white)(--extreme-fg_hash_replaced_in_test black)(--extreme-bg_hash_replaced_in_test white))
         onclick
         style={
           display: flex;
           flex-direction: column;
         }>
      <div class="content_common_hash_replaced_in_test"
           style={
             display: flex;
             flex-direction: column;
           }> new message content </div>
    </div>
    ```

    ## with_intent
    ```html
    <div class="container_hash_replaced_in_test"
         custom-css-vars=((--intent-fg_hash_replaced_in_test #348203)(--intent-bg_hash_replaced_in_test #ecffe0)(--extreme-fg_hash_replaced_in_test black)(--extreme-bg_hash_replaced_in_test white))
         onclick
         style={
           display: flex;
           flex-direction: column;
         }>
      <div class="title-bar_hash_replaced_in_test title-text_hash_replaced_in_test"
           style={
             display: flex;
           }> title </div>
      <div class="content_common_hash_replaced_in_test"
           style={
             display: flex;
             flex-direction: column;
           }> new message that happens to be happy </div>
    </div>
    ```

    ## with_field_set
    ```html
    <div class="container_hash_replaced_in_test"
         custom-css-vars=((--intent-fg_hash_replaced_in_test black)(--intent-bg_hash_replaced_in_test white)(--extreme-fg_hash_replaced_in_test black)(--extreme-bg_hash_replaced_in_test white))
         onclick
         style={
           display: flex;
           flex-direction: column;
         }>
      <div class="title-bar_hash_replaced_in_test title-text_hash_replaced_in_test"
           style={
             display: flex;
           }> title </div>
      <div class="content_common_hash_replaced_in_test"
           style={
             display: flex;
             flex-direction: column;
           }> Card from a fieldset </div>
    </div>
    ```

    # kado v1
    ## basic
    ```html
    <div class="container_hash_replaced_in_test"
         custom-css-vars=((--title-fg_hash_replaced_in_test #e2e3e4)(--title-border_hash_replaced_in_test #313943)(--title-bg_hash_replaced_in_test #14181c)(--fg_hash_replaced_in_test #d1d2d3)(--contrasting-fg-intent-color_hash_replaced_in_test #d1d2d3)(--border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))
         onclick
         style={
           display: flex;
           flex-direction: column;
         }>
      <div class="title-bar_hash_replaced_in_test title-text_hash_replaced_in_test"
           style={
             display: flex;
           }> new message </div>
      <div class="content_prominent_hash_replaced_in_test yes-title_hash_replaced_in_test"
           style={
             display: flex;
             flex-direction: column;
           }> new message content </div>
    </div>
    ```

    ## basic_no_title
    ```html
    <div class="container_hash_replaced_in_test"
         custom-css-vars=((--title-fg_hash_replaced_in_test #e2e3e4)(--title-border_hash_replaced_in_test #313943)(--title-bg_hash_replaced_in_test #14181c)(--fg_hash_replaced_in_test #d1d2d3)(--contrasting-fg-intent-color_hash_replaced_in_test #d1d2d3)(--border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))
         onclick
         style={
           display: flex;
           flex-direction: column;
         }>
      <div class="content_prominent_hash_replaced_in_test no-title_hash_replaced_in_test"
           style={
             display: flex;
             flex-direction: column;
           }> new message content </div>
    </div>
    ```

    ## with_intent
    ```html
    <div class="container_hash_replaced_in_test"
         custom-css-vars=((--title-fg_hash_replaced_in_test black)(--title-border_hash_replaced_in_test"rgba(255,255,255,0.30)")(--title-bg_hash_replaced_in_test #57c961)(--fg_hash_replaced_in_test #d1d2d3)(--contrasting-fg-intent-color_hash_replaced_in_test #57c961)(--border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))
         onclick
         style={
           display: flex;
           flex-direction: column;
         }>
      <div class="title-bar_hash_replaced_in_test title-text_hash_replaced_in_test"
           style={
             display: flex;
           }> title </div>
      <div class="content_prominent_hash_replaced_in_test yes-title_hash_replaced_in_test"
           style={
             display: flex;
             flex-direction: column;
           }> new message that happens to be happy </div>
    </div>
    ```

    ## with_field_set
    ```html
    <div class="container_hash_replaced_in_test"
         custom-css-vars=((--title-fg_hash_replaced_in_test #e2e3e4)(--title-border_hash_replaced_in_test #313943)(--title-bg_hash_replaced_in_test #14181c)(--fg_hash_replaced_in_test #d1d2d3)(--contrasting-fg-intent-color_hash_replaced_in_test #d1d2d3)(--border_hash_replaced_in_test #313943)(--bg_hash_replaced_in_test #1a1d21))
         onclick
         style={
           display: flex;
           flex-direction: column;
         }>
      <div class="title-bar_hash_replaced_in_test title-text_hash_replaced_in_test"
           style={
             display: flex;
           }> title </div>
      <div class="content_prominent_hash_replaced_in_test yes-title_hash_replaced_in_test"
           style={
             display: flex;
             flex-direction: column;
           }> Card from a fieldset </div>
    </div>
    ``` |}]
;;
