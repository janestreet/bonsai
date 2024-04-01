open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

(* $MDX part-begin=on_change *)
let on_change_demo graph =
  let view, value = State_examples.counter ~step:(Bonsai.return 1) graph in
  Bonsai.Edge.on_change'
    ~equal:Int.equal
    ~callback:
      (Bonsai.return (fun (prev_value : int option) (new_value : int) ->
         match prev_value with
         | None -> (* Do nothing on first render*) Effect.Ignore
         | Some prev_value ->
           Effect.alert
             [%string "prev value: %{prev_value#Int}, new value: %{new_value#Int}"]))
    value
    graph;
  view
;;

(* $MDX part-end *)

let () = Util.run on_change_demo ~id:"on_change"
