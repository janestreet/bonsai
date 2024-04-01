open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax

(* $MDX part-begin=counters_for_users_assoc *)

let counters_for_users_assoc graph : Vdom.Node.t Bonsai.t =
  let users =
    [ "Alice", (); "Bob", (); "Charlie", () ] |> String.Map.of_alist_exn |> Bonsai.return
  in
  let counters =
    Bonsai.assoc
      (module String)
      users
      ~f:(fun _ _ graph -> State_examples.counter_ui graph)
      graph
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

let () = Util.run counters_for_users_assoc ~id:"counters_for_users_assoc"

(* $MDX part-begin=counters_for_users_scoped *)
module Form = Bonsai_web_ui_form.With_automatic_view

let counters_for_users_scoped graph : Vdom.Node.t Bonsai.t =
  let form =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "Alice"; "Bob"; "Charlie" ])
      graph
  in
  let active_user =
    let%arr form = form in
    Form.value_or_default form ~default:"Alice"
  in
  Bonsai.scope_model
    (module String)
    ~on:active_user
    graph
    ~for_:(fun graph ->
      let%arr counter = State_examples.counter_ui graph
      and name = active_user
      and form = form in
      Vdom.Node.div
        [ Form.view_as_vdom form; Vdom.Node.p [ Vdom.Node.text name ]; counter ])
;;

(* $MDX part-end *)

let () = Util.run counters_for_users_scoped ~id:"counters_for_users_scoped"
