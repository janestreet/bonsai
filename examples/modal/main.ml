open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Modal = Bonsai_web_ui_modal
module Native_modal = Draft_modal

(* original modal *)

let center = Vdom.Attr.style (Css_gen.text_align `Center)

let modal_1_contents =
  Bonsai.const (Vdom.Node.div ~attrs:[ center ] [ Vdom.Node.text "Surprise!" ])
;;

let modal_2_contents n =
  let%sub got_ya =
    match%arr n with
    | 1 -> "Got ya!"
    | n -> sprintf "Got ya %d times!" n
  in
  let%arr got_ya = got_ya in
  Vdom.Node.div
    ~attrs:[ center ]
    [ Vdom.Node.text "Surprise!"; Vdom.Node.br (); Vdom.Node.text got_ya ]
;;

let original_app =
  let%sub modal_1 =
    Modal.create
      (module Unit)
      (fun _ ~hide_self:_ -> modal_1_contents)
      ~equal:[%equal: Unit.t]
  in
  let%sub modal_2 =
    Modal.create
      (module Int)
      (fun n ~hide_self:_ -> modal_2_contents n)
      ~equal:[%equal: Int.t]
  in
  let%sub state, set_state =
    Bonsai.state 1 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
  in
  let%arr state = state
  and set_state = set_state
  and modal_1 = modal_1
  and modal_2 = modal_2 in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> modal_1.show ()) ]
        [ Vdom.Node.text "Click me!" ]
    ; modal_1.view
    ; Vdom.Node.br ()
    ; Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.on_click (fun _ ->
              let%bind.Ui_effect () = set_state (state + 1) in
              modal_2.show state)
          ; Vdom.Attr.style (Css_gen.margin_top (`Px 10))
          ]
        [ Vdom.Node.text "Click me multiple times!" ]
    ; modal_2.view
    ]
;;

(* New modal using <dialog> *)

let dialog_title text =
  Vdom.Node.h3
    ~attrs:
      [ Vdom.Attr.style
          (Css_gen.concat
             [ Css_gen.margin_top (`Rem 0.); Css_gen.margin_bottom (`Rem 0.5) ])
      ]
    [ Vdom.Node.text text ]
;;

let dialog_contents ?title ?close_button:close_button_on_cancel contents =
  let title_markup = Option.value_map title ~default:Vdom.Node.none ~f:dialog_title in
  let close_button_markup =
    match close_button_on_cancel with
    | None -> Vdom.Node.none
    | Some on_click ->
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.style (Css_gen.position ~right:(`Px 12) ~top:(`Px 12) `Absolute) ]
        [ Native_modal.close_button ~on_click () ]
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.style (Css_gen.uniform_padding (`Px 20)) ]
    [ title_markup; close_button_markup; contents ]
;;

(* A button that adds lots of content to the page. Used to see/test that we disabled body
   scrolling. *)
let add_lots_of_content_markup =
  let%sub show, toggle = Bonsai.toggle ~default_model:false in
  let%arr show = show
  and toggle = toggle in
  let content =
    if show
    then
      Vdom.Node.div
        (List.init 200 ~f:(fun _ -> Vdom.Node.div [ Vdom.Node.text "more text" ]))
    else Vdom.Node.none
  in
  let button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> toggle) ]
      [ Vdom.Node.text
          (if not show
           then "Add lots of content (to test scrolling)"
           else "Remove lots of content")
      ]
  in
  [ button; content ]
;;

let stacking_example =
  (* Note: there's probably a way to write this such that the outer modal takes a bonsai
     component for it's contents. For now it just demonstrates that the vdom's can stack
  *)
  (* Creates the button to show/hide the modal and has state management. creator should
     be fun on_cancel -> modal *)
  let%sub show_outer, toggle_outer = Bonsai.toggle ~default_model:false in
  let%sub show_inner, toggle_inner = Bonsai.toggle ~default_model:false in
  let toggle_button text toggler =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> toggler) ]
      [ Vdom.Node.text text ]
  in
  let%arr show_outer = show_outer
  and toggle_outer = toggle_outer
  and show_inner = show_inner
  and toggle_inner = toggle_inner in
  let inner_modal =
    let contents = dialog_contents (Vdom.Node.text "inner modal") in
    Native_modal.view ~on_cancel:(fun _ -> toggle_inner) contents
  in
  let outer_modal =
    let contents =
      dialog_contents
        (Vdom.Node.div
           [ toggle_button "Show inner modal" toggle_inner
           ; (if not show_inner then Vdom.Node.none else inner_modal)
           ])
    in
    Native_modal.view ~on_cancel:(fun _ -> toggle_outer) contents
  in
  [ (if not show_outer then Vdom.Node.none else outer_modal)
  ; Vdom.Node.div [ toggle_button "Show stacking modal" toggle_outer ]
  ]
;;

let native_app =
  (* I have a simple code example here of creating a modal, but then for later examples
     I have a slightly non-idiomatic helper function to cut down on the boilerplate
     of creating several different similar variations. *)
  (* Creates the button to show/hide the modal and has state management. creator should
     be fun on_cancel -> modal *)
  let create_modal_example creator button_text ?desc _ =
    let%sub show, toggle = Bonsai.toggle ~default_model:false in
    let%arr show = show
    and toggle = toggle in
    let toggle_button text toggler =
      Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> toggler) ]
        [ Vdom.Node.text text ]
    in
    let extra_desc_markup =
      Option.value_map
        ~default:Vdom.Node.none
        ~f:(fun text ->
          Vdom.Node.div
            ~attrs:
              [ Vdom.Attr.style
                  Css_gen.(concat [ font_style `Italic; margin_top (`Px 3) ])
              ]
            [ Vdom.Node.text text ])
        desc
    in
    let modal = creator ~toggle in
    [ (if not show then Vdom.Node.none else modal)
    ; Vdom.Node.div [ toggle_button button_text toggle; extra_desc_markup ]
    ]
  in
  let%sub simple_modal =
    (* I have many examples so I factored out the logic that adds a button and manages show/hide
       with bonsai state, but you can just write that directly.
    *)
    let creator ~toggle =
      let contents =
        dialog_contents
          ~title:"Hello!"
          ~close_button:toggle
          (Vdom.Node.div
             [ Vdom.Node.text "I'm a simple modal dialog (or dialog modal.)" ])
      in
      Native_modal.view ~on_cancel:(fun _ -> toggle) contents
    in
    create_modal_example creator "Simple modal" ()
  in
  let%sub side_sheet_modal =
    let creator ~toggle =
      let contents =
        dialog_contents
          ~title:"Heya!"
          ~close_button:toggle
          (Vdom.Node.div [ Vdom.Node.text "I'm a side sheet modal." ])
      in
      Native_modal.view ~layout:`Right_side_sheet ~on_cancel:(fun _ -> toggle) contents
    in
    create_modal_example creator "Side sheet" ()
  in
  let%sub transparent_modal =
    let creator ~toggle =
      let contents =
        dialog_contents
          (Vdom.Node.text
             "This can create a more subtle effect (think vs code's navigation popup)")
      in
      Native_modal.view
        ~transparent_overlay:true
        ~animated:false
        ~disable_body_scroll:false
        ~on_cancel:(fun _ -> toggle)
        contents
    in
    create_modal_example creator "Transparent backdrop, no animation" ()
  in
  let intro_text =
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.style Css_gen.(concat [ font_style `Italic; margin_top (`Px 3) ]) ]
      [ Vdom.Node.text
          "We also have a modal implementation using the native <dialog /> tag. Features \
           (can be individually enabled/disabled): \n\
          \            disable body scrolling, \n\
          \            close with Esc or overlay click, \n\
          \            animated transitions,\n\
          \            tabbing excludes webpage elements outside of dialog (always on).\n\
          \            Can customize overlay and frame styling if desired."
      ]
  in
  let%sub add_lots_of_content_markup = add_lots_of_content_markup in
  let%sub stacking_example = stacking_example in
  let%arr simple_modal = simple_modal
  and side_sheet_modal = side_sheet_modal
  and transparent_modal = transparent_modal
  and stacking_example = stacking_example
  and add_lots_of_content_markup = add_lots_of_content_markup in
  Vdom.Node.div
    (List.map
       ~f:(fun n ->
         Vdom.Node.div ~attrs:[ Vdom.Attr.style (Css_gen.margin_top (`Px 10)) ] [ n ])
       (List.join
          [ [ intro_text ]
          ; simple_modal
          ; side_sheet_modal
          ; transparent_modal
          ; stacking_example
          ; add_lots_of_content_markup
          ]))
;;

let combined_app =
  let%sub original_app = original_app in
  let%sub native_app = native_app in
  let%arr original_app = original_app
  and native_app = native_app in
  Vdom.Node.div
    [ Vdom.Node.h2
        ~attrs:[ Vdom.Attr.style (Css_gen.margin_bottom (`Rem 0.5)) ]
        [ Vdom.Node.text "Original Modal" ]
    ; original_app
    ; Vdom.Node.h2
        ~attrs:[ Vdom.Attr.style (Css_gen.margin_bottom (`Rem 0.5)) ]
        [ Vdom.Node.text "New Native Dialog vdom component" ]
    ; native_app
    ]
;;

let () = Bonsai_web.Start.start combined_app
