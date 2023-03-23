open! Core
open! Bonsai_web

include
  [%css
    stylesheet
      {|
  html,
  body,
  .app {
    background:rgb(250, 250, 250);
  }

  * { box-sizing: border-box; }

  .app {
    padding:1em;
    font-size:20px;
    font-family: sans-serif;

    display: flex;
    align-items: flex-start;
  }

  .row {
    display:flex;
    justify-content: space-between;
    align-items: center;
    padding:0.2em;
    padding-right: 0.5em;
    margin: 0.2em;
    border-radius:3px;
    color:white;
  }

  .color_list {
    margin:1em;
    position:relative;
    border: 1px solid #a6a6a6;
    border-radius: 3px;
    background: white;
    user-select: none;
    min-width: 150px;
  }

  .results {
    min-width: unset;
  }

  .results .body {
      padding: 0.2em;
  }

  .header {
    padding: 0.5em;
    background: #2f2f2f;
    color:white;
    display:flex;
    justify-content: start;
    gap: 0.5em;
    align-items: center;
  }

  .debug {
    position:absolute;
    left: 50%;
    transform: translateX(-50%);
    min-width:850px;
    min-height:200px;
    z-index: 1;
    font-size:0.8em;
    border-radius:3px;
    outline:rgba(50, 50, 50, 0.2) solid 5px;
  }

  .row_remove,
  .regenerate {
    font-family: 0.7em;
    display: inline-block;
    margin-left: 0.2em;

    background-color: grey;
    border: 1px solid darkgrey;
    border-radius:3px;
    cursor: pointer;
    padding: 0.3em 0.5em;
    color:white;

    user-select: none;
  }

  .row_remove {
    opacity: 0.5;
  }


  .svg_button {
    padding:0.2em;
    border-radius:5px;

    cursor:pointer;

    transition: 0.1s ease-in background-color;
  }

  .packet {
    border: 1px solid #818181;
    background: hsl(200deg 10% 96%) ;
    margin: 0.5em;
    padding: 0.5em;
    border-radius: 2px;
    font-family:monospace;
  }

  .packet.executed {
    opacity: 0.5;
  }

  .packet.selected {
    border: 1px solid #01579b;
    background: #e1f5fe;
  }

  .diff {
    display: flex;
    align-items: center;
    justify-content: end;
  }

  .diff *:first-child {
    margin-right: auto;
  }

  .chip {
    min-width: 1em;
    min-height: 1em;
    aspect-ratio: 1;
    padding:0.5em;
    border-radius: 3px;
    display:inline-block;
    margin: 0.1em 0.2em;

    display:inline-flex;
    justify-content: center;
    align-items: center;
    color:white;
  }

  .svg_button.dark {
    background:black;
    color: white;
  }

  .svg_button.light {
    background: white;
    outline: 1px solid rgba(255,255,255,0.5);
    color: black;
  }

  .svg_button.dark:hover {
    background-color: #404040;
  }

  .svg_button.light:hover {
    background-color: #f0f0f0;
  }

  .comparison_container {
    display:flex;
  }

  .comparison_list > div {
    display:flex;
    flex-direction: column;
  }

  .button_container {
    display: inline-flex;
  }
|}]

let color_class color =
  match color with
  | `Light -> light
  | `Dark -> dark
;;

let icon_button icon (color : [ `Light | `Dark ]) ~on_click =
  let color_class = color_class color in
  Vdom.Node.div
    ~attrs:[ button_container; Vdom.Attr.on_click (fun _ -> on_click) ]
    [ Feather_icon.svg ~extra_attrs:[ svg_button; color_class ] icon ]
;;

let refresh_button = icon_button Repeat
let x_button = icon_button X
let arrow_right = icon_button Arrow_right
let fast_forward = icon_button Fast_forward
let help = icon_button Help_circle

let color_of_mult ?alpha mult =
  Css_gen.Color.HSLA.create
    ~h:(mult *. 360.0 |> Float.to_int)
    ~s:(Percent.of_mult 0.4)
    ~l:(Percent.of_mult 0.6)
    ?a:alpha
    ()
;;

let darker_color_of_mult mult =
  Css_gen.Color.HSLA.create
    ~h:(mult *. 360.0 |> Float.to_int)
    ~s:(Percent.of_mult 0.6)
    ~l:(Percent.of_mult 0.4)
    ()
;;

let chip ?key mult content =
  let background = color_of_mult mult in
  let border = darker_color_of_mult mult in
  Vdom.Node.div
    ?key
    ~attrs:
      [ chip
      ; Vdom.Attr.style (Css_gen.background_color (`HSLA background))
      ; Vdom.Attr.style
          (Css_gen.border ~width:(`Px 3) ~style:`Solid ~color:(`HSLA border) ())
      ]
    [ content ]
;;
