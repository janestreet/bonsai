open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

let fake_slow_capitalize_string_rpc =
  Effect.of_deferred_fun (fun text ->
    let rand_delay = Random.float_range 0.0 1.0 in
    let%map.Deferred () = Async_kernel.after (Time_ns.Span.of_sec rand_delay) in
    String.uppercase text)
;;

let textbox =
  let%sub state =
    Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%arr text, set_text = state in
  let view =
    Vdom.Node.input
      ~attrs:[ Vdom.Attr.(value_prop text @ on_input (fun _ -> set_text)) ]
      ()
  in
  text, view
;;

let component =
  let%sub text, view = textbox in
  let%sub capitalized =
    Bonsai.Edge.Poll.(
      effect_on_change
        ~sexp_of_input:[%sexp_of: String.t]
        ~sexp_of_result:[%sexp_of: String.t]
        ~equal_input:[%equal: String.t]
        ~equal_result:[%equal: String.t]
        (Starting.initial "")
        text
        ~effect:(Value.return fake_slow_capitalize_string_rpc))
  in
  let%arr view = view
  and capitalized = capitalized in
  Vdom.Node.div [ view; Vdom.Node.text capitalized ]
;;

let () = Bonsai_web.Start.start component
