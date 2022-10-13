open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax

(* Basic constructors *)

let themed_component fs =
  (* add more themes to this list as they're built *)
  let themes = [ Bonsai_web.View.Expert.default_theme ] in
  List.iter themes ~f:(fun theme ->
    print_endline ("# " ^ View.Theme.name theme);
    List.iter fs ~f:(fun (name, f) ->
      print_endline ("## " ^ name);
      let handle = Handle.create (Result_spec.vdom Fn.id) (Bonsai.const (f theme)) in
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
      ~attr:(Vdom.Attr.on_double_click (fun _ -> on_click))
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
    <button onclick style={ background-color: #e0f7ff; color: black; }> button label </button>
    ```

    ## Info disabled
    ```html
    <button disabled="" onclick style={ background-color: #e0f7ff; color: black; opacity: 0.300000; }> button label </button>
    ```

    ## Success enabled
    ```html
    <button onclick style={ background-color: #ecffe0; color: black; }> button label </button>
    ```

    ## Success disabled
    ```html
    <button disabled="" onclick style={ background-color: #ecffe0; color: black; opacity: 0.300000; }> button label </button>
    ```

    ## Warning enabled
    ```html
    <button onclick style={ background-color: #ffeb3b; color: black; }> button label </button>
    ```

    ## Warning disabled
    ```html
    <button disabled="" onclick style={ background-color: #ffeb3b; color: black; opacity: 0.300000; }> button label </button>
    ```

    ## Error enabled
    ```html
    <button onclick style={ background-color: #ff2522; color: white; }> button label </button>
    ```

    ## Error disabled
    ```html
    <button disabled="" onclick style={ background-color: #ff2522; color: white; opacity: 0.300000; }> button label </button>
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
    (View.Theme.override_constants_temporarily ~inside ~f:(fun consts ->
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
    (View.Expert.override_current_theme_temporarily ~inside ~f:(fun (module M) ->
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
      let view theme = View.text theme ~intent "hello world" in
      let name_of_intent = intent |> [%sexp_of: View.Intent.t] |> Sexp.to_string in
      name_of_intent, view)
  in
  themed_component all;
  [%expect
    {|
    # default theme
    ## Info
    ```html
    <span style={
            background-color: #e0f7ff;
            color: black;
            padding-left: 0.25em;
            padding-right: 0.25em;
          }> hello world </span>
    ```

    ## Success
    ```html
    <span style={
            background-color: #ecffe0;
            color: black;
            padding-left: 0.25em;
            padding-right: 0.25em;
          }> hello world </span>
    ```

    ## Warning
    ```html
    <span style={
            background-color: #ffeb3b;
            color: black;
            padding-left: 0.25em;
            padding-right: 0.25em;
            font-weight: bold;
          }> hello world </span>
    ```

    ## Error
    ```html
    <span style={
            background-color: #ff2522;
            color: white;
            padding-left: 0.25em;
            padding-right: 0.25em;
            font-weight: bold;
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
      ~attr:grey_background
      ~per_tab_attr:(fun _i ~is_active ->
        if is_active then red_border else Vdom.Attr.empty)
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
              color: white;
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
              color: white;
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
              color: black;
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
              color: black;
              background-color: #ecffe0;
              padding-left: 0.50em;
              padding-right: 0.50em;
            }> dev </span>
    </div>
    ``` |}]
;;
