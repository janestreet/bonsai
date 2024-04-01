open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

let counter ~step graph = State_examples.counter ~step graph |> Tuple2.get1

(* $MDX part-begin=maybe_show_naive *)
let maybe_show_naive show graph =
  let counter = counter ~step:(return 1) graph in
  let%arr counter = counter
  and show = show in
  match show with
  | false -> Vdom.Node.none
  | true -> counter
;;

(* $MDX part-end *)

let show_control f graph =
  let show, toggle = Bonsai.toggle ~default_model:false graph in
  let%arr toggle = toggle
  and content = f show graph in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> toggle) ]
        [ Vdom.Node.text "Toggle" ]
    ; content
    ]
;;

let () = Util.run (show_control maybe_show_naive) ~id:"maybe_show_naive"

(* $MDX part-begin=maybe_show *)
let maybe_show show graph =
  let counter = counter ~step:(return 1) graph in
  match%sub show with
  | false -> Bonsai.return Vdom.Node.none
  | true -> counter
;;

(* $MDX part-end *)

let () = Util.run (show_control maybe_show) ~id:"maybe_show"

module Show_control_2 = struct
  type t =
    [ `Count_by_1
    | `Count_by_2
    | `No
    ]
  [@@deriving equal, sexp]
end

(* $MDX part-begin=maybe_show_2 *)
let maybe_show_2 show graph =
  match%sub show with
  | `Count_by_1 -> counter ~step:(return 1) graph
  | `Count_by_2 -> counter ~step:(return 2) graph
  | `No -> Bonsai.return Vdom.Node.none
;;

(* $MDX part-end *)

let show_control_2 f graph =
  let form =
    Form.Elements.Dropdown.list
      (module Show_control_2)
      ~equal:[%equal: Show_control_2.t]
      (Bonsai.return [ `Count_by_1; `Count_by_2; `No ])
      graph
  in
  let show =
    let%arr form = form in
    Form.value_or_default form ~default:`Count_by_1
  in
  let%arr content = f show graph
  and form = form in
  Vdom.Node.div [ Form.view_as_vdom form; content ]
;;

let () = Util.run (show_control_2 maybe_show_2) ~id:"maybe_show_2"

(* $MDX part-begin=maybe_show_var *)
let maybe_show_var show graph =
  match%sub show with
  | `Count_by step -> counter ~step graph
  | `No -> Bonsai.return Vdom.Node.none
;;

(* $MDX part-end *)
(* $MDX part-begin=maybe_show_var_guard *)
let maybe_show_var_guard show graph =
  match%sub show with
  | `Count_by step when Int.equal step 1 -> counter ~step graph
  | `Count_by step when Int.equal step 4 -> counter ~step graph
  | `Count_by step -> counter ~step graph
  | `No -> Bonsai.return Vdom.Node.none
;;

(* $MDX part-end *)

(* $MDX part-begin=maybe_show_var_scope_model *)
let maybe_show_var_scope_model show graph =
  match%sub show with
  | `Count_by step ->
    Bonsai.scope_model
      (module Int)
      ~on:step
      ~for_:(fun graph -> counter ~step graph)
      graph
  | `No -> Bonsai.return Vdom.Node.none
;;

(* $MDX part-end *)

module Show_control_var = struct
  type t =
    [ `Count_by of int
    | `No
    ]
  [@@deriving equal, sexp]
end

let show_control_var f graph =
  let form =
    Form.Elements.Dropdown.list
      (module Show_control_var)
      ~equal:[%equal: Show_control_var.t]
      (Bonsai.return [ `Count_by 1; `Count_by 2; `Count_by 4; `Count_by 8; `No ])
      graph
  in
  let show =
    let%arr form = form in
    Form.value_or_default form ~default:(`Count_by 1)
  in
  let%arr content = f show graph
  and form = form in
  Vdom.Node.div [ Form.view_as_vdom form; content ]
;;

let () = Util.run (show_control_var maybe_show_var) ~id:"maybe_show_var"
let () = Util.run (show_control_var maybe_show_var_guard) ~id:"maybe_show_var_guard"

let () =
  Util.run (show_control_var maybe_show_var_scope_model) ~id:"maybe_show_var_scope_model"
;;

(* $MDX part-begin=maybe_show_dynamic_count *)
let maybe_show_dynamic_count show graph =
  match%sub show with
  | `Count_by_1 -> counter ~step:(return 1) graph
  | `Count_by_2 -> counter ~step:(return 2) graph
  | `No -> Bonsai.return Vdom.Node.none
;;

(* $MDX part-end *)

let show_control_2 f graph =
  let form =
    Form.Elements.Dropdown.list
      (module Show_control_2)
      ~equal:[%equal: Show_control_2.t]
      (Bonsai.return [ `Count_by_1; `Count_by_2; `No ])
      graph
  in
  let show =
    let%arr form = form in
    Form.value_or_default form ~default:`Count_by_1
  in
  let%arr content = f show graph
  and form = form in
  Vdom.Node.div [ Form.view_as_vdom form; content ]
;;

let () = Util.run (show_control_2 maybe_show_dynamic_count) ~id:"maybe_show_2"

(* $MDX part-begin=multiple_counters *)

let multiple_counters (input : unit Int.Map.t Bonsai.t) graph =
  let counters =
    Bonsai.assoc
      (module Int)
      input
      ~f:(fun key (_ : unit Bonsai.t) graph ->
        let%arr key = key
        and counter = State_examples.counter_ui graph in
        Vdom.Node.tr
          [ Vdom.Node.td [ Vdom.Node.textf "counter #%d:" key ]
          ; Vdom.Node.td [ counter ]
          ])
      graph
  in
  let%arr counters = counters in
  Vdom.Node.table (Map.data counters)
;;

(* $MDX part-end *)

(* $MDX part-begin=multiple_counters_dynamic *)

let multiple_counters_dynamic graph =
  let counter_view, n = State_examples.counter ~step:(Bonsai.return 1) graph in
  let map_containing_n_entries =
    let%arr n = n in
    if n <= 0
    then Int.Map.empty
    else List.init n ~f:(fun i -> i, ()) |> Int.Map.of_alist_exn
  in
  let%arr counter_view = counter_view
  and table = multiple_counters map_containing_n_entries graph in
  Vdom.Node.div [ counter_view; table ]
;;

(* $MDX part-end *)

let () = Util.run multiple_counters_dynamic ~id:"multiple_counters_dynamic"
