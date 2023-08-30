open! Core
open! Async_kernel
open! Bonsai_web
open! Bonsai.Let_syntax

(* $MDX part-begin=politician_table *)
type row =
  { id : int
  ; name : string
  ; age : int
  }

let basic_table rows =
  let open Vdom.Node in
  let thead = thead [ td [ text "id" ]; td [ text "name" ]; td [ text "age" ] ] in
  let tbody =
    rows
    |> List.map ~f:(fun { id; name; age } ->
         tr [ td [ textf "%d" id ]; td [ text name ]; td [ textf "%d" age ] ])
    |> tbody
  in
  table [ thead; tbody ]
;;

let politicians =
  basic_table
    [ { id = 0; name = "George Washington"; age = 67 }
    ; { id = 1; name = "Alexander Hamilton"; age = 47 }
    ; { id = 2; name = "Abraham Lincoln"; age = 56 }
    ]
;;

(* $MDX part-end *)

let () =
  Util.run
    (Bonsai.const
       (Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "politicians" ] [ politicians ]))
    ~id:"politician-table"
;;

(* $MDX part-begin=attr_table *)

type row2 =
  { id : int
  ; name : string
  ; age : int
  }

let table_styles =
  let open Css_gen in
  border_collapse `Collapse
  @> border ~style:`Solid ~color:(`Name "brown") ~width:(`Px 1) ()
;;

let thead_styles =
  let open Css_gen in
  text_align `Center
  @> background_color (`Name "brown")
  @> color (`Name "antiquewhite")
  @> font_weight `Bold
;;

let tr_odd = Css_gen.background_color (`Name "antiquewhite")
let tr_even = Css_gen.background_color (`Name "wheat")

let td_styles =
  Css_gen.padding ~top:(`Px 4) ~bottom:(`Px 4) ~left:(`Px 4) ~right:(`Px 4) ()
;;

let basic_table_attr rows =
  let open Vdom.Node in
  let thead =
    thead
      ~attrs:[ Vdom.Attr.style thead_styles ]
      [ td [ text "id" ]; td [ text "name" ]; td [ text "age" ] ]
  in
  let tbody =
    rows
    |> List.mapi ~f:(fun i { id; name; age } ->
         let tr_style = if Int.( % ) i 2 = 0 then tr_even else tr_odd in
         tr
           ~attrs:[ Vdom.Attr.style tr_style ]
           [ td ~attrs:[ Vdom.Attr.style td_styles ] [ textf "%d" id ]
           ; td ~attrs:[ Vdom.Attr.style td_styles ] [ text name ]
           ; td ~attrs:[ Vdom.Attr.style td_styles ] [ textf "%d" age ]
           ])
    |> tbody
  in
  table ~attrs:[ Vdom.Attr.style table_styles ] [ thead; tbody ]
;;

let politicians =
  basic_table_attr
    [ { id = 0; name = "George Washington"; age = 67 }
    ; { id = 1; name = "Alexander Hamilton"; age = 47 }
    ; { id = 2; name = "Abraham Lincoln"; age = 56 }
    ]
;;

(* $MDX part-end *)

let () =
  Util.run (Bonsai.const (Vdom.Node.div [ politicians ])) ~id:"politician-table-attr"
;;

(* $MDX part-begin=inline_css *)
module Style =
[%css
stylesheet
  {|
table.politicians {
  border-collapse: collapse;
  border: 1px solid brown;
}

table.politicians td {
  padding: 4px;
}

table.politicians thead {
  text-align: center;
  background: brown;
  color: antiquewhite;
  font-weight: bold;
}

table.politicians tr {
  background: antiquewhite;
}

table.politicians tr:nth-child(even) {
  background: wheat;
}
    |}]

(* $MDX part-end *)

(* $MDX part-begin=table_with_ppx_css *)
let table_with_ppx_css rows =
  let open Vdom.Node in
  let thead = thead [ td [ text "id" ]; td [ text "name" ]; td [ text "age" ] ] in
  let tbody =
    rows
    |> List.map ~f:(fun { id; name; age } ->
         tr [ td [ textf "%d" id ]; td [ text name ]; td [ textf "%d" age ] ])
    |> tbody
  in
  table ~attrs:[ Style.politicians ] [ thead; tbody ]
;;

(* $MDX part-end *)

let politicians =
  table_with_ppx_css
    [ { id = 0; name = "George Washington"; age = 67 }
    ; { id = 1; name = "Alexander Hamilton"; age = 47 }
    ; { id = 2; name = "Abraham Lincoln"; age = 56 }
    ]
;;

let () = Util.run (Bonsai.const (Vdom.Node.div [ politicians ])) ~id:"politician-table"

let themeable_table ?(theme = Style.default) rows =
  let module Style = (val theme) in
  let open Vdom.Node in
  let thead = thead [ td [ text "id" ]; td [ text "name" ]; td [ text "age" ] ] in
  let tbody =
    rows
    |> List.map ~f:(fun { id; name; age } ->
         tr [ td [ textf "%d" id ]; td [ text name ]; td [ textf "%d" age ] ])
    |> tbody
  in
  table ~attrs:[ Style.politicians ] [ thead; tbody ]
;;

(* $MDX part-begin=my_theme *)
module My_theme =
[%css
stylesheet
  {|
table.politicians {
  border-collapse: collapse;
  border: 1px solid black;
}

table.politicians td {
  padding: 4px;
}

table.politicians thead {
  text-align: center;
  background: black;
  color: white;
  font-weight: bold;
}

table.politicians td {
  border: 1px solid black;
}

    |}]

let table =
  themeable_table
    ~theme:(module My_theme)
    [ { id = 0; name = "George Washington"; age = 67 }
    ; { id = 1; name = "Alexander Hamilton"; age = 47 }
    ; { id = 2; name = "Abraham Lincoln"; age = 56 }
    ]
;;

(* $MDX part-end *)

let () = Util.run (Bonsai.const (Vdom.Node.div [ table ])) ~id:"themeable-table"

module _ = struct
  (* $MDX part-begin=tomato-square-ppx-css *)
  module Style =
  [%css
  stylesheet
    {|
.square {
  background-color: tomato;
  height: 100px;
  width: 100px;
  }
|}]
  (* $MDX part-end *)

  (* $MDX part-begin=tomato-square *)
  let square = Vdom.Node.div ~attrs:[ Style.square ] []
  (* $MDX part-end *)

  let () = Util.run (Bonsai.const (Vdom.Node.div [ square ])) ~id:"tomato-square"
end

module _ = struct
  module Style =
  [%css
  stylesheet
    {|
:root {
  --red: #ff5a5a;
  --green: #5aff5a;
  --blue: #5a5aff;
}

.button {
  /* Add common styles */
}

.button.red-button {
  background-color: var(--red);
}

.button.green-button {
  background-color: var(--green);
}

.button.blue-button {
  background-color: var(--blue);
}
|}]

  type theme =
    | Light
    | Dark

  let _ = Light
  let theme = Dark

  (* $MDX part-begin=css-variables *)
  let square =
    Vdom.Node.div
      ~attrs:
        [ Style.Variables.set
            ~red:
              (match theme with
               | Dark -> "tomato"
               | Light -> "#ff5a5a")
            ()
        ]
      [ Vdom.Node.button ~attrs:[ Style.red_button ] [ View.text "red" ]
      ; Vdom.Node.button ~attrs:[ Style.blue_button ] [ View.text "blue" ]
      ; Vdom.Node.button ~attrs:[ Style.green_button ] [ View.text "green" ]
      ]
  ;;

  (* $MDX part-end *)

  let () = Util.run (Bonsai.const (Vdom.Node.div [ square ])) ~id:"variables"
end
