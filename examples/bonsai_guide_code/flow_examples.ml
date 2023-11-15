open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

let textbox ~placeholder =
  let%sub state, set_state =
    Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%arr state = state
  and set_state = set_state
  and placeholder = placeholder in
  let view =
    Vdom.Node.input
      ~attrs:
        [ Vdom.Attr.value_prop state
        ; Vdom.Attr.on_input (fun _ new_text -> set_state new_text)
        ; Vdom.Attr.placeholder placeholder
        ]
      ()
  in
  state, view
;;

(* $MDX part-begin=textbox_with_placeholder *)
let textbox_with_placeholder = textbox ~placeholder:(Value.return "the placeholder")

(* $MDX part-end *)

let () =
  Util.run
    (textbox_with_placeholder |> Computation.map ~f:snd)
    ~id:"textbox_with_placeholder"
;;

(* $MDX part-begin=textbox_chaining *)
let textbox_chaining =
  let%sub a_contents, a_view = textbox ~placeholder:(Value.return "") in
  let%sub _, b_view = textbox ~placeholder:a_contents in
  let%arr a_view = a_view
  and b_view = b_view in
  let style = Vdom.Attr.style (Css_gen.display `Inline_grid) in
  Vdom.Node.div ~attrs:[ style ] [ a_view; b_view ]
;;

(* $MDX part-end *)

let () = Util.run textbox_chaining ~id:"textbox_chaining"

(* $MDX part-begin=textbox_chaining_match *)

let textbox_matching =
  let%sub a_contents, a_view = textbox ~placeholder:(Value.return "") in
  let%sub a_contents =
    let%arr s = a_contents in
    let s = String.strip s in
    if String.is_empty s then None else Some s
  in
  match%sub a_contents with
  | None ->
    let%arr a_view = a_view in
    let message = Vdom.Node.div [ Vdom.Node.text "<a is empty>" ] in
    Vdom.Node.div [ a_view; message ]
  | Some placeholder ->
    let%sub _, b_view = textbox ~placeholder in
    let%arr a_view = a_view
    and b_view = b_view in
    let style = Vdom.Attr.style (Css_gen.display `Inline_grid) in
    Vdom.Node.div ~attrs:[ style ] [ a_view; b_view ]
;;

(* $MDX part-end *)

let () = Util.run textbox_matching ~id:"textbox_chaining_match"

(* $MDX part-begin=multiple_counters *)

let multiple_counters (input : unit String.Map.t Value.t) =
  let%sub counters =
    Bonsai.assoc
      (module String)
      input
      ~f:(fun _key (_ : unit Value.t) -> State_examples.counter_state_machine)
  in
  let%arr counters = counters in
  Vdom.Node.table
    (counters
     |> Map.to_alist
     |> List.map ~f:(fun (key, vdom) ->
          let open Vdom.Node in
          let name = td [ Vdom.Node.text key ] in
          let counter = td [ vdom ] in
          Vdom.Node.tr [ name; counter ]))
;;

(* $MDX part-end *)

(* $MDX part-begin=multiple_counters_constant_map *)

let multiple_counters_constant =
  multiple_counters
    ([ "hello", (); "there", () ] |> Map.of_alist_exn (module String) |> Value.return)
;;

(* $MDX part-end *)

let () = Util.run multiple_counters_constant ~id:"multiple_counters_constant"

(* $MDX part-begin=kudo_tracker *)
module Model = struct
  type t = unit String.Map.t [@@deriving sexp, equal]

  let default = String.Map.of_alist_exn [ "Dave", (); "Jill", () ]
end

module Action = struct
  type t =
    | Add of string
    | Remove of string
  [@@deriving sexp_of]
end

let people =
  Bonsai.state_machine0
    ()
    ~sexp_of_model:[%sexp_of: Model.t]
    ~equal:[%equal: Model.t]
    ~sexp_of_action:[%sexp_of: Action.t]
    ~default_model:Model.default
    ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model action ->
    match action with
    | Add name -> Map.set model ~key:name ~data:()
    | Remove name -> Map.remove model name)
;;

let add_new_person_form ~inject_add_person =
  let%sub form = Form.Elements.Textbox.string () in
  let%arr form = form
  and inject_add_person = inject_add_person in
  let on_submit name = Vdom.Effect.Many [ Form.set form ""; inject_add_person name ] in
  form
  |> Form.label "name"
  |> Form.validate ~f:(fun name ->
       if String.for_all name ~f:Char.is_whitespace
       then Error (Error.of_string "name must not be empty")
       else Ok ())
  |> Form.view_as_vdom ~on_submit:(Form.Submit.create ~f:on_submit ())
;;

let people_table people ~inject_remove_person =
  Bonsai.assoc
    (module String)
    people
    ~f:(fun name (_ : unit Value.t) ->
      let%sub counter = State_examples.counter_state_machine in
      let%arr counter = counter
      and name = name
      and inject_remove_person = inject_remove_person in
      let open Vdom.Node in
      let remove_person =
        td
          [ button
              ~attrs:[ Vdom.Attr.on_click (fun _ -> inject_remove_person name) ]
              [ text "x" ]
          ]
      in
      let name = td [ text name ] in
      let counter = td [ counter ] in
      tr [ name; counter; remove_person ])
;;

let kudo_tracker =
  let%sub people, inject_action = people in
  let%sub add_new_person_form =
    let%sub inject_add_person =
      let%arr inject_action = inject_action in
      fun name -> inject_action (Add name)
    in
    add_new_person_form ~inject_add_person
  in
  let%sub people_table =
    let%sub inject_remove_person =
      let%arr inject_action = inject_action in
      fun name -> inject_action (Remove name)
    in
    people_table people ~inject_remove_person
  in
  let%arr people_table = people_table
  and add_new_person_form = add_new_person_form in
  let open Vdom.Node in
  div
    [ h2 [ text "kudos tracker" ]
    ; table
        [ thead [ tr [ th [ text "Name" ]; th [ text "# Kudos" ]; th [ text "Remove" ] ] ]
        ; tbody (Map.data people_table)
        ]
    ; h2 [ text "Add Person" ]
    ; add_new_person_form
    ]
;;

(* $MDX part-end *)

let () = Util.run kudo_tracker ~id:"kudo_tracker"
