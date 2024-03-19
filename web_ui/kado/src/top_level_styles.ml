open! Core
open Virtual_dom

let dynamic_attr ~testing_name s =
  (* NOTE: This exists so that we do not attach the entire contents of the
     css on each expect test output. *)
  match am_running_test with
  | true ->
    (* Creating a dummy class instead of using [Vdom.Attr.create] is used in order to
       avoid a warning on attribute merging. *)
    Vdom.Attr.class_ ("kado-top-level-for-testing-" ^ testing_name)
  | false -> Inline_css.Private.Dynamic.attr [%string {|@layer kado.app { %{s} }|}]
;;

let app =
  Vdom.Attr.combine App.app
  @@ dynamic_attr
       ~testing_name:"kado"
       {|
  :root *,
  :root *::before,
  :root *::after {
    box-sizing: border-box;
  }

  :root *::selection {
    background: #1ba1f2;
    color: white;
  }

  :root {
    /* https://rsms.me/inter/lab/?feat-cv09=1&feat-ss02=1&feat-tnum=1&feat-zero=1
       tabular numbers : enabled via "tnum"
       slashed zero    : enabled via "zero"
       disambiguation  : enabled via "ss02"
       flat-top-three  : enabled via "cv09" */
    font-family: 'Inter', sans-serif;
    /* font-feature-settings: "tnum", "zero", "ss02", "cv09"; */
    accent-color: #1ba1f2;
  }
|}
;;

let dark_bg = `Hex "#1a1d21"

let dark =
  Vdom.Attr.many
    [ app
    ; App.dark
    ; dynamic_attr
        ~testing_name:"dark"
        [%string
          {|
    :root {
      color-scheme:dark;
      background: %{Css_gen.Color.to_string_css dark_bg};
    }
  |}]
    ]
;;

let light_bg = `Hex "#e5e2de"

let light =
  Vdom.Attr.many
    [ app
    ; App.light
    ; dynamic_attr
        ~testing_name:"light"
        [%string
          {|
    :root {
      color-scheme:light;
      background: %{Css_gen.Color.to_string_css light_bg};
    }
  |}]
    ]
;;
