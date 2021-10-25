open! Core
open Bonsai_web

let widget uri : Vdom.Node.t =
  (* [init] is called whenever [uri] changes, updating the favicon. The DOM element
     produced by the widget is unused. *)
  Vdom.Node.widget
    ~id:(Type_equal.Id.create ~name:"favicon" (const [%sexp "favicon"]))
    ~init:(fun () ->
      let open Js_of_ocaml in
      let icon_node = Dom_html.document##querySelector (Js.string {|link[rel="icon"]|}) in
      let href_value = uri |> Uri.to_string in
      (match Js.Opt.to_option icon_node with
       | Some icon_node ->
         icon_node##setAttribute (Js.string "href") (Js.string href_value)
       | None ->
         let head = Dom_html.document##querySelector (Js.string "head") in
         let link =
           let open Vdom in
           Node.create
             "link"
             ~attr:
               (Attr.many
                  [ Attr.type_ "image/svg+xml"
                  ; Attr.create "rel" "icon"
                  ; Attr.href href_value
                  ])
             []
           |> Node.to_dom
         in
         let link = (link :> Dom.node Js.t) in
         Js.Opt.iter head (fun head -> ignore (head##appendChild link : Dom.node Js.t)));
      (), Vdom.Node.to_dom Vdom.Node.none)
    ()
;;

let slider ~min ~max ~value ~inject =
  let open Vdom in
  Node.input
    ~attr:
      (Attr.many
         [ Attr.type_ "range"
         ; Attr.min min
         ; Attr.max max
         ; Attr.value (value |> string_of_int)
         ; Attr.on_input (fun _ev value -> inject (int_of_string value))
         ])
    []
;;

let component =
  let open Bonsai.Let_syntax in
  let%sub text = Bonsai.state_opt [%here] (module String) ~default_model:"🤯" in
  let%sub size = Bonsai.state [%here] (module Int) ~default_model:80 in
  let%sub pos_x = Bonsai.state [%here] (module Int) ~default_model:50 in
  let%sub pos_y = Bonsai.state [%here] (module Int) ~default_model:50 in
  let%sub fg_color = Bonsai.state [%here] (module String) ~default_model:"#000000" in
  let%sub bg_color = Bonsai.state [%here] (module String) ~default_model:"#ffffff" in
  return
  @@ let%pattern_map text, inject_text = text
  and size, inject_size = size
  and pos_x, inject_pos_x = pos_x
  and pos_y, inject_pos_y = pos_y
  and fg_color, inject_fg_color = fg_color
  and bg_color, inject_bg_color = bg_color in
  let open Vdom in
  let text_box = Vdom_input_widgets.Entry.text ~value:text ~on_input:inject_text () in
  let size_slider = slider ~min:1. ~max:200. ~value:size ~inject:inject_size in
  let x_slider = slider ~min:0. ~max:100. ~value:pos_x ~inject:inject_pos_x in
  let y_slider = slider ~min:0. ~max:100. ~value:pos_y ~inject:inject_pos_y in
  let background_color =
    if String.equal "#ffffff" bg_color then None else Some (`Hex bg_color)
  in
  let favicon =
    Favicon_svg.of_unicode
      ~font_size:(Percent.of_percentage (float_of_int size))
      ~pos_x:(Percent.of_percentage (float_of_int pos_x))
      ~pos_y:(Percent.of_percentage (float_of_int pos_y))
      ?background_color
      ~font_color:(`Hex fg_color)
      (Option.value text ~default:"")
  in
  let uri = Favicon_svg.to_embedded_url favicon in
  let image =
    Node.create
      "img"
      ~attr:(Attr.many [ Attr.src (Uri.to_string uri); Attr.class_ "svg-preview" ])
      []
  in
  let code_section =
    match text with
    | None -> []
    | Some text ->
      let attr fmt cond value = Option.some_if (cond value) (fmt value ^ "\n  ") in
      let non eq x y = not (eq x y) in
      let attrs =
        [ attr
            (sprintf "~font_size:(Percent.of_percentage %.1f)")
            (Fn.const true)
            (float_of_int size)
        ; attr
            (sprintf "~pos_x:(Percent.of_percentage %.1f)")
            (non Float.equal 50.)
            (float_of_int pos_x)
        ; attr
            (sprintf "~pos_y:(Percent.of_percentage %.1f)")
            (non Float.equal 50.)
            (float_of_int pos_y)
        ; attr
            (sprintf {|~font_color:(`Hex "%s")|})
            (non String.equal "#000000")
            fg_color
        ; attr
            (sprintf {|~background_color:(`Hex "%s")|})
            (non String.equal "#ffffff")
            bg_color
        ]
      in
      let attrs = List.filter_opt attrs |> String.concat in
      [ Node.div ~attr:(Attr.class_ "section-header") [ Node.text "Code:" ]
      ; Node.pre
          [ [%string {|
  Favicon_svg.of_unicode
  %{attrs}"%{text}" |}]
            |> String.strip
            |> Node.text
          ]
      ]
  in
  let spacer x = Node.div ~attr:(Attr.style (Css_gen.height (`Px x))) [] in
  Node.div
    ~attr:(Attr.class_ "container")
    ([ widget uri
     ; Node.h3
         [ Node.text "What would you like "
         ; Node.create "i" [ Node.text "your" ]
         ; Node.text " favicon to say?"
         ]
     ; spacer 10
     ; text_box
     ; spacer 20
     ; Node.div ~attr:(Attr.class_ "section-header") [ Node.text "Preview:" ]
     ; spacer 10
     ; image
     ; spacer 20
     ; Node.div ~attr:(Attr.class_ "section-header") [ Node.text "Fine tuning:" ]
     ; spacer 5
     ; Node.div [ Node.text "Size: "; size_slider ]
     ; Node.div [ Node.text "Pos x: "; x_slider ]
     ; Node.div [ Node.text "Pos y: "; y_slider ]
     ; Node.div
         [ Node.text "Text color: "
         ; Node.input
             ~attr:
               (Attr.many
                  [ Attr.type_ "color"
                  ; Attr.value fg_color
                  ; Attr.on_input (fun _ev value -> inject_fg_color value)
                  ])
             []
         ]
     ; spacer 5
     ; Node.div
         [ Node.text "Background color: "
         ; Node.input
             ~attr:
               (Attr.many
                  [ Attr.type_ "color"
                  ; Attr.value bg_color
                  ; Attr.on_input (fun _ev value -> inject_bg_color value)
                  ])
             []
         ]
     ; spacer 20
     ]
     @ code_section)
;;

let run () =
  Async_js.init ();
  let (_ : (unit, Nothing.t) Start.Handle.t) =
    Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
  in
  ()
;;

let () = run ()
