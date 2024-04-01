open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery
module Snips = Bonsai_experimental_snips
open! Snips.Infix

module Shared_code = struct
  module Style =
  [%css
  stylesheet
    {|
    .container {
      width: 400px;
      height: 200px;
      max-height: 400px;
      max-width: 600px;
      overflow:auto;
      resize: both;
    }

    .box {
      display: inline-block;
      width:100%;
      min-height:100%;
      padding: 0.25em 0.5em;
    }

    .x2_grid {
      display: grid;
      grid-template-columns: minmax(50%, auto) 1fr;
      column-gap: 1em;
    }

    p {
        margin: 0.5em 0 0.3em 0;
    }

    code {
      background: var(--code-bg);
      color: var(--code-fg);
      border: 1px solid var(--code-border);
      border-radius: 2px;
      margin: -2px;
      padding: 2px;
    }

    .header {
      background: var(--header-bg);
      border-bottom: 1px solid var(--header-border);
      display:flex;
      justify-content: space-between;
      align-items:center;
    }

    .header > h1 {
      margin:0;
      line-height:1em;
      margin-left: 0.5em;
    }

    h2 > a {
      color: inherit;
      text-decoration:none;
      position:relative;
    }

    h2 > a::before {
      content: '#';
      display: inline-block;
      position: absolute;
      opacity: 0;
      transform: translate(-100%, 0);
      transition:
        opacity 0.15s linear,
        transform 0.15s ease-out;
    }

    h2:hover > a::before {
      opacity: 50%;
      transform: translate(-120%, 0);
    }

  |}]

  type t =
    { red : string -> Vdom.Node.t
    ; blue : string -> Vdom.Node.t
    ; green : string -> Vdom.Node.t
    ; orange : string -> Vdom.Node.t
    ; normal : string -> Vdom.Node.t
    ; container : Vdom.Node.t -> Vdom.Node.t
    }

  let prepare graph =
    let theme = View.Theme.current graph in
    let%arr theme = theme in
    let { View.Constants.intent = { info; error; warning; success }
        ; primary
        ; extreme_primary_border
        ; _
        }
      =
      View.constants theme
    in
    let colored { View.Constants.Fg_bg.background; foreground } s =
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.style Css_gen.(background_color background @> color foreground)
          ; Style.box
          ]
        [ Vdom.Node.text s ]
    in
    let container n =
      Vdom.Node.div
        ~attrs:
          [ Style.container
          ; Vdom.Attr.style
              (Css_gen.outline
                 ~width:(`Px 2)
                 ~color:extreme_primary_border
                 ~style:`Dashed
                 ())
          ]
        [ n ]
    in
    { red = colored error
    ; blue = colored info
    ; orange = colored warning
    ; green = colored success
    ; normal = colored primary
    ; container
    }
  ;;
end

let lorem_ipsum =
  {|Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec convallis velit id turpis sagittis fermentum. Donec suscipit augue id scelerisque efficitur. Nullam vel dui id augue interdum lobortis. Mauris dictum pretium arcu, ac tristique leo congue sed. Donec justo ante, tempor ac ante at, maximus faucibus purus. Etiam volutpat urna a nulla luctus consequat. In eleifend ex et gravida finibus. Nullam a volutpat nunc, et venenatis sapien. Sed ut ornare augue, ut volutpat enim. Maecenas vel fringilla arcu, nec finibus dolor. Vivamus ultricies convallis velit vitae elementum. In sodales viverra ex, vel ornare erat pulvinar eu. Duis vel tortor at lacus dapibus laoreet at a nunc. Etiam vitae rutrum velit. Nunc a massa mauris. Integer imperdiet, erat quis tincidunt auctor, ligula nunc suscipit velit, elementum aliquam ex dui ut nisl. Donec placerat aliquet porta. Donec eget massa interdum, convallis mauris id, tincidunt sem. Fusce suscipit vitae tortor et finibus. Suspendisse pulvinar efficitur lorem sit amet rutrum. Maecenas lacus ipsum, fringilla eu rutrum id, lobortis eu tellus. |}
;;

module Markup = struct
  type t =
    | H of string
    | P of string
    | D of (Bonsai.graph -> (Vdom.Node.t * string) Bonsai.t)
    | Ds of (string option * (Bonsai.graph -> (Vdom.Node.t * string) Bonsai.t)) list

  let remove_layout_comments code _graph =
    let%arr code = code in
    String.split_lines code
    |> List.filter ~f:(Fn.non (String.is_substring ~substring:"remove-this-line"))
    |> String.concat ~sep:"\n"
  ;;

  let to_component = function
    | H s ->
      let id =
        s
        |> String.lowercase
        |> String.map ~f:(function
             | ('a' .. 'z' | '0' .. '1') as alphanum -> alphanum
             | _ -> '_')
      in
      let link = Vdom.Node.a ~attrs:[ Vdom.Attr.href ("#" ^ id) ] [ Vdom.Node.text s ] in
      fun _graph -> Bonsai.return (Vdom.Node.h2 ~attrs:[ Vdom.Attr.id id ] [ link ])
    | P s ->
      fun _graph ->
        Bonsai.return
          (Vdom.Node.inner_html
             ()
             ~attrs:[ Vdom.Attr.empty ]
             ~tag:"p"
             ~this_html_is_sanitized_and_is_totally_safe_trust_me:s)
    | D c ->
      fun graph ->
        let%sub demo, code = c graph in
        let code = remove_layout_comments code graph in
        let gallery =
          Gallery.make_demo' ~hide_html:true ~ocaml_label:None ~demo ~code () graph
        in
        let%arr { Gallery.demo; code } = gallery in
        Vdom.Node.div ~attrs:[ Shared_code.Style.x2_grid ] [ code; demo ]
    | Ds a ->
      fun graph ->
        a
        |> List.map ~f:(fun (ocaml_label, c) ->
             let%sub demo, code = c graph in
             let code = remove_layout_comments code graph in
             let r =
               Gallery.make_demo' ~hide_html:true ~ocaml_label:None ~demo ~code () graph
             in
             let%arr r = r in
             ocaml_label, r)
        |> Bonsai.all
        |> Bonsai.map ~f:(fun demos ->
             demos
             |> List.concat_map ~f:(fun (label, { code; demo }) ->
                  let pre =
                    match label with
                    | None -> []
                    | Some label -> [ Vdom.Node.text label; Vdom.Node.div [] ]
                  in
                  pre @ [ code; demo ])
             |> Vdom.Node.div ~attrs:[ Shared_code.Style.x2_grid ])
  ;;
end

let intro : Markup.t list =
  [ P
      {| Snips is a library for describing the layout of elements within a "contained"
      region. Contained regions are sections of a page whose size is not allowed to grow
      without bound. |}
  ; P
      {| One readily available example of a contained region is the browser window itself.
      Content inside the window can be bigger than the window, but instead of making the
      window bigger, usually scrollbars are added to the page instead. |}
  ; P
      {| Indeed, the primary intended use Snips is for "application layout" at the top
      level of the page, for adding things like headers, sidebars, and footers and for
      splitting up the remaining space via some constraints. |}
  ]
;;

module Basic = struct
  let boilerplate =
    {|(* code that we'll use in the rest of these examples *)
module Snips = Bonsai_experimental_snips
open Snips.Infix

let normal s = Vdom.Node.div [ Vdom.Node.text s ]
let red s = Vdom.Node.div ~attr:Style.red [ Vdom.Node.text s ]
let blue, green, orange = ... (* more of the same *)

(* the most basic "layout" *)
|}
  ;;

  let view graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; normal; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout = Snips.body (normal lorem_ipsum) in
        Snips.render layout]
    in
    container vdom, boilerplate ^ demo
  ;;

  let content : Markup.t list =
    [ P
        {| The simplest Snips layout is one where all of the available space is devoted
        to a single element.  This is a pretty boring and unrealistic example (for
        something this simple, don't use Snips), so we'll use this intro example to also
        define some helper functions for the rest of the demos. |}
    ; D view
    ]
  ;;
end

module Your_first_snip = struct
  let view graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; normal; red; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          (* remove-this-line (for line breaking purposes) *)
          |+| Snips.body (normal lorem_ipsum)
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let content : Markup.t list =
    [ H "Basic snips"
    ; P
        {| The basis of the Snips API is the idea that you start with a container, and
        progressively "snip" pieces off of it to fill with content, leaving you with a
        smaller container that you continue to snip until you're left with a container to
        fill with content. |}
    ; P
        {| Every Snips layout starts with composing "continuation" layouts, and ends with
        a call to a "terminator" layout. You've already seen Snips.body, which is the most
        basic terminator, and now we'll use <code>Snips.top</code> (a continuation layout)
        and the <code>|+|</code> operator, which composes layouts. |}
    ; D view
    ; P
        {| When <code>Snips.top</code> is composed with <code>Snips.body</code> using
        <code>|+|</code>, it takes the available space, snips the top off, fills the
        snipped region with the vdom node provided to <code>Snips.top</code>, and then
        proceeds to make the rest of the area available to subsequent layouts (in this
        case, the call to <code>Snips.body</code>). |}
    ]
  ;;
end

module Composed_snips = struct
  let view graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; normal; red; blue; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.top (blue "header 2 ")
          |+| Snips.body (normal lorem_ipsum)
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let content : Markup.t list =
    [ P
        {| Let's see what happens when we compose more snips, starting with another
        <code>Snips.top</code>. |}
    ; D view
    ; P
        {| Because the blue <code>Snip.top</code> is after the red <code>Snip.top</code> the blue region is
        <i>inside</i> the area that remains after red snipping is done.  Now let's add
        sideways snips! |}
    ]
  ;;
end

module Sideways_snips = struct
  let view1 graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; normal; red; blue; green; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.top (blue "header 2 ")
          |+| Snips.left (green "sidebar")
          |+| Snips.body (normal lorem_ipsum)
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let alt_1 graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; normal; red; blue; green; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.top (blue "header 2 ")
          |+| Snips.left (green "sidebar")
          |+| Snips.body (normal lorem_ipsum)
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let alt_2 graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; normal; red; blue; green; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.left (green "sidebar")
          |+| Snips.top (blue "header 2 ")
          |+| Snips.body (normal lorem_ipsum)
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let alt_3 graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; normal; red; blue; green; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.left (green "sidebar")
          |+| Snips.top (red "header")
          |+| Snips.top (blue "header 2 ")
          |+| Snips.body (normal lorem_ipsum)
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let content : Markup.t list =
    [ H "Sideways snips"
    ; P {| Now let's add sideways snips! |}
    ; D view1
    ; P
        {| By moving the position of the call to <code>Snips.left</code> we can see how it impacts the layout |}
    ; Ds
        [ Some {| "left" snip is last |}, alt_1
        ; Some {| "left" snip is in the middle |}, alt_2
        ; Some {| "left" snip is first |}, alt_3
        ]
    ]
  ;;
end

module All_the_sides = struct
  let view graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; normal; red; blue; green; orange } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.left (green "left")
          |+| Snips.right (blue "right")
          |+| Snips.bottom (orange "footer")
          |+| Snips.body (normal lorem_ipsum)
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let content : Markup.t list =
    [ P
        {| For completenes' sake, let's use the last two 'side' operators, <code>Snips.bottom</code>
        and <code>Snips.right</code>.|}
    ; D view
    ]
  ;;
end

module Splits = struct
  let view graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; red; blue; green; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.split_h
                [ Snips.body (blue lorem_ipsum)
                  (* remove-this-line  (for line breaking purposes) *)
                ; Snips.body (green lorem_ipsum)
                ]
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let view2 graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; red; green; orange; blue; normal; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.split_h
                [ Snips.top (orange "left header")
                  |+| Snips.left (green "L")
                  |+| Snips.body (normal lorem_ipsum)
                ; Snips.right (green "R")
                  |+| Snips.bottom (blue "right footer ")
                  |+| Snips.body (normal lorem_ipsum)
                ]
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let content : Markup.t list =
    [ H "Doing the splits"
    ; P
        {|
   So far, we've used <code>Snips.body</code> as our "layout terminator" to fill all the remaining
   space in a layout, but it isn't the only terminal; we can also partition the space
   with subdivisions using <code>Snips.split_h</code> and <code>Snips.split_v</code> |}
    ; D view
    ; P
        {|
   You'll notice that <code>split_h</code> takes a list of <code>Snip.t</code> which means that we can use
   the snipping combinators on these sub-splits!
   |}
    ; D view2
    ]
  ;;
end

module Splits_on_splits = struct
  let view graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; red; normal; orange; blue; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.split_h
                [ Snips.split_v
                    [ Snips.body (orange "Left 1")
                    ; Snips.body (red "Left 2")
                    ; Snips.body (blue "Left 3")
                    ]
                ; Snips.body (normal lorem_ipsum)
                ]
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let view2 graph =
    let prepared = Shared_code.prepare graph in
    let%arr { container; red; normal; orange; blue; _ } = prepared in
    let vdom, demo =
      [%demo
        let layout =
          Snips.top (red "header")
          |+| Snips.split_h
                [ Snips.split_v
                    [ Snips.body (orange "Left 1")
                    ; Snips.body (red "Left 2")
                    ; Snips.body (blue "Left 3")
                    ]
                ; Snips.right (blue "R")
                  (* remove-this-line  (for line breaking purposes) *)
                  |+| Snips.body (normal lorem_ipsum)
                ]
        in
        Snips.render layout]
    in
    container vdom, demo
  ;;

  let content : Markup.t list =
    [ P "Splits can also nest"; D view; P "And they compose well with snips"; D view2 ]
  ;;
end

let component graph =
  let%sub theme, theme_picker =
    Gallery.Theme_picker.component ~default:Kado_light ~standalone:false () graph
  in
  let%sub () =
    Bonsai_extra.exactly_once
      (Bonsai.return
         (Effect.of_sync_fun
            (fun () ->
              let open Js_of_ocaml in
              match Js.to_string Dom_html.window##.location##.hash with
              | "" -> ()
              | other ->
                Dom_html.window##.location##.hash := Js.string "";
                Dom_html.window##.location##.hash := Js.string other)
            ()))
      graph
  in
  View.Theme.set_for_app
    theme
    (fun graph ->
      let nodes =
        Bonsai.all
          ([ intro
           ; Basic.content
           ; Your_first_snip.content
           ; Composed_snips.content
           ; Sideways_snips.content
           ; All_the_sides.content
           ; Splits.content
           ; Splits_on_splits.content
           ]
           |> List.concat
           |> List.map ~f:Markup.to_component
           |> List.map ~f:(fun x -> x graph))
      in
      let attr =
        let%arr theme = theme in
        let border =
          View.extreme_primary_border_color theme |> Css_gen.Color.to_string_css
        in
        Shared_code.Style.Variables.set
          ~header_border:border
          ~code_border:border
          ~header_bg:
            ((View.extreme_colors theme).background |> Css_gen.Color.to_string_css)
          ~code_bg:((View.extreme_colors theme).background |> Css_gen.Color.to_string_css)
          ~code_fg:((View.extreme_colors theme).foreground |> Css_gen.Color.to_string_css)
          ()
      in
      let body =
        Gallery.wrap_application ~theme_picker:(Bonsai.return Vdom.Node.none) nodes graph
      in
      let%arr theme_picker = theme_picker
      and attr = attr
      and body = body in
      let header =
        Vdom.Node.div
          ~attrs:[ Shared_code.Style.header ]
          [ Vdom.Node.h1 [ Vdom.Node.text "snips" ]; theme_picker ]
      in
      Snips.top header |+| Snips.body body |> Snips.render ~container_attr:attr)
    graph
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component
;;
