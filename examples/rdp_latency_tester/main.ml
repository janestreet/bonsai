open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Style =
  [%css
    stylesheet
      {|
  * {
    box-sizing: border-box;
    cursor: default;
    user-select:none;
  }

  :root, body, .container {
    width:100vw;
    height:100vh;
    margin:0;
    padding:0;
    overflow:hidden;
    display: flex;
    -webkit-font-smoothing: subpixel-antialiased;
  }

  body {
    font-size: 0.95em;
  }

  .target, .backer {
    position: absolute;
    top: 50%;
    left: 50%;
    margin:0;
    padding:0;
    z-index:2;

  }

  .target {
    width:50px;
    height:50px;
    background: black;
  }

  .backer {
    width:100px;
    height:100px;
    background: white;
    transform: translate(-25px, -25px);
  }

  p {
    margin:0;
  }

  .a {
    background: rgb(63, 81, 181);
    color: rgb(0, 188, 212);
  }

  .b {
    background: maroon;
    color:#ffc300;
    opacity: 0;
  }

  .target:hover + .b,
  .b.on {
    opacity: 1;
  }

  .a, .b {
    width:100vw;
    height:100vh;
    position: absolute;
    text-align: justify;
  }

  .backer {
    font-size: 16px;
    display: flex;
    justify-content: center;
  }

  .tipped {
    padding:5px 10px;
  }

  .tooltip {
    max-width:min(1000px, 70vw) !important;
  }
  .tooltip p {
    margin:0.5em;
  }

  .page {
    display: flex;
    flex-direction: column;
    width: fit-content;
    background: #1a1d21;
    border: 1px solid #727380;
    border-radius: 4px;
    position: absolute;
    z-index: 3;
    max-width: 50vw;
    left: 50%;
    transform: translateX(-50%);
    top: 25%;
    align-items: center;
    padding: 1em 3em;
    font-size: 18px;
    gap: 1em;
  }
|}]

let make_text ~drop =
  Lorem_ipsum.text
  |> String.uppercase
  |> String.filter ~f:(function
    | 'A' .. 'Z' | ' ' | '\n' -> true
    | _ -> false)
  |> String.split_lines
  |> Fn.flip List.drop drop
  |> String.concat ~sep:"\n"
  |> fun text -> Vdom.Node.p [ Vdom.Node.text text ]
;;

let description =
  Vdom.Node.div
    [ Vdom.Node.p
        [ Vdom.Node.text
            {|This is a test-bench for the RDP "whole screen replacement" scenario.
              When you hover your mouse over the black square, the content on the
              page is swapped, causing a worst case scenario for RDP diff transport
              and image compression.|}
        ]
    ; Vdom.Node.p
        [ Vdom.Node.text
            {|Although I've described this as a "worst case scenario", it actually
              occurs quite frequently in a typical developers workflow.  Examples
              include: |}
        ]
    ; Vdom.Node.ul
        [ Vdom.Node.li [ Vdom.Node.text "changing tabs in a browser" ]
        ; Vdom.Node.li [ Vdom.Node.text "switching active programs or workspaces" ]
        ; Vdom.Node.li
            [ Vdom.Node.text "hitting the 'page-down' key in a browser or editor" ]
        ; Vdom.Node.li [ Vdom.Node.text "opening, closing, or switching files" ]
        ]
    ; Vdom.Node.p [ Vdom.Node.text {|Here's how you use the benchmark: |} ]
    ; Vdom.Node.ol
        [ Vdom.Node.li
            [ Vdom.Node.text "start recording your screen using 'Is It Snappy?'" ]
        ; Vdom.Node.li
            [ Vdom.Node.text
                {|move your mouse from right to left onto the black square in the
                center of the screen|}
            ]
        ; Vdom.Node.li [ Vdom.Node.text "stop the recording" ]
        ; Vdom.Node.li
            [ Vdom.Node.text {|using 'Is It Snappy', measure the time between: |}
            ; Vdom.Node.ul
                [ Vdom.Node.li
                    [ Vdom.Node.text
                        "the first frame where the cursor occludes a row of black \
                         pixels, and"
                    ]
                ; Vdom.Node.li
                    [ Vdom.Node.text
                        "the first frame where the background starts to change color"
                    ]
                ]
            ]
        ]
    ; Vdom.Node.p
        [ Vdom.Node.text
            {|
      At first glance, this might seem like a useless benchmark; after all, shouldn't the
      drawing of the cursor always be done in sync with the drawing of everything else,
      causing both of these events to happen simulataneously?  On a physical computer,
      yes, but when using RDP, the cursor-drawing is done on the _client computer_!
      You can use this difference to precisely measure RDP round-trip-latency.|}
        ]
    ]
;;

module Mode = struct
  type t =
    | Manual
    | Automatic
    | Help
  [@@deriving sexp, equal, compare, enumerate]
end

let theme = Kado.theme ~version:Bleeding ()

let component =
  let%sub ((mode, _) as mode_state) =
    Bonsai.state Manual ~sexp_of_model:[%sexp_of: Mode.t] ~equal:[%equal: Mode.t]
  in
  let%sub last, set_last =
    Bonsai.state
      Time_ns.epoch
      ~sexp_of_model:[%sexp_of: Time_ns.Alternate_sexp.t]
      ~equal:[%equal: Time_ns.Alternate_sexp.t]
  in
  let%sub is_on, time =
    match%sub mode with
    | Automatic ->
      let%sub on, toggle = Bonsai.toggle ~default_model:false in
      let%sub now = Bonsai.Clock.now in
      let%sub () =
        Bonsai.Clock.every
          (Time_ns.Span.of_sec 5.0)
          ~when_to_start_next_effect:`Every_multiple_of_period_blocking
          ~trigger_on_activate:false
          (let%map toggle = toggle
           and now = now
           and set_last = set_last in
           Effect.Many [ toggle; set_last now ])
      in
      return (Value.both on now)
    | _ -> Bonsai.const (false, Time_ns.epoch)
  in
  let%arr mode, set_mode = mode_state
  and on = is_on
  and time = time
  and last = last in
  let tabs =
    View.tabs_enum
      theme
      (module Mode)
      ~active:mode
      ~on_change:(fun ~from:_ ~to_ -> set_mode to_)
  in
  let page =
    match mode with
    | Help -> description
    | Manual -> Vdom.Node.none
    | Automatic ->
      View.vbox
        [ View.text
            ~attrs:[ Vdom.Attr.style (Css_gen.font_family [ "monospace" ]) ]
            (Time_ns.to_string_utc time)
        ; View.text
            ~attrs:[ Vdom.Attr.style (Css_gen.font_family [ "monospace" ]) ]
            (Time_ns.to_string_utc last)
        ]
  in
  let extras =
    match mode with
    | Manual ->
      [ Vdom.Node.div ~attrs:[ Style.backer ] []
      ; Vdom.Node.div ~attrs:[ Style.target ] []
      ]
    | Help | Automatic -> []
  in
  let on_attr = if on then Style.on else Vdom.Attr.empty in
  Vdom.Node.div
    ~attrs:[ Style.container ]
    ([ Vdom.Node.div ~attrs:[ Style.a ] [ make_text ~drop:0 ]
     ; View.vbox ~attrs:[ Style.page ] [ tabs; page ]
     ]
     @ extras
     @ [ Vdom.Node.div ~attrs:[ Style.b; on_attr ] [ make_text ~drop:5 ] ])
;;

let () = Bonsai_web.Start.start (View.Theme.set_for_app (Value.return theme) component)
