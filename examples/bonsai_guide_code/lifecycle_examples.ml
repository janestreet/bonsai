open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

(* $MDX part-begin=lifecycle *)
let lifecycle_demo graph =
  let log_val, log =
    Bonsai.state_machine0
      ~default_model:""
      ~apply_action:(fun _ curr new_ -> curr ^ new_)
      graph
  in
  let show, toggle_show = Bonsai.toggle ~default_model:false graph in
  let main_view =
    match%sub show with
    | true ->
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%arr log = log in
           log "🚀")
        ~on_deactivate:
          (let%arr log = log in
           log "🔥")
        graph;
      Vdom.Node.text [%string "Active!!!!"] |> Bonsai.return
    | false -> Vdom.Node.text "Nothing to see here..." |> Bonsai.return
  in
  let%arr log_val = log_val
  and toggle_show = toggle_show
  and main_view = main_view in
  Vdom.Node.(
    div
      [ div
          [ button
              ~attrs:[ Vdom.Attr.on_click (fun _ -> Effect.all_unit [ toggle_show ]) ]
              [ text "toggle show" ]
          ; text log_val
          ]
      ; main_view
      ])
;;

(* $MDX part-end *)

let () = Util.run lifecycle_demo ~id:"lifecycle"
