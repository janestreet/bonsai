open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Apply_action_context = Bonsai.Apply_action_context

(* $MDX part-begin=resettable_counters *)
let two_counters graph =
  let%arr counter1 = State_examples.counter_ui graph
  and counter2 = State_examples.counter_ui graph in
  Vdom.Node.div
    ~attrs:[ [%css {|border: 1px solid black; padding: 4px|}] ]
    [ counter1; counter2 ]
;;

let reset_ui graph ~f =
  let view, reset = Bonsai.with_model_resetter ~f graph in
  let%arr view = view
  and reset = reset in
  Vdom.Node.div
    [ view
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> reset) ]
        [ Vdom.Node.text "Reset" ]
    ]
;;

let resettable_counters = reset_ui ~f:two_counters

(* $MDX part-end *)

let () = Util.run resettable_counters ~id:"resettable_counters"

(* $MDX part-begin=resettable_counters_from_inside *)
let resettable_counters_from_inside graph =
  Bonsai.with_model_resetter'
    ~f:(fun ~reset graph ->
      let%arr counter1 = State_examples.counter_ui graph
      and counter2 = State_examples.counter_ui graph
      and reset = reset in
      Vdom.Node.div
        ~attrs:[ [%css {|border: 1px solid black; padding: 4px|}] ]
        [ counter1
        ; Vdom.Node.button
            ~attrs:[ Vdom.Attr.on_click (fun _ -> reset) ]
            [ Vdom.Node.text "Reset" ]
        ; counter2
        ])
    graph
;;

(* $MDX part-end *)

let () = Util.run resettable_counters_from_inside ~id:"resettable_counters_from_inside"

(* $MDX part-begin=connection_type *)
module Connection : sig
  (* Some generic connection type that doesn't know of Bonsai *)
  type t

  (* Connect to some resource *)
  val create : uri:string -> t

  (* Notify subscribers whenever the connection status changes *)
  val on_status_change
    :  t
    -> ([ `Connected | `Connecting | `Disconnected ] -> unit Effect.t)
    -> unit Effect.t
end
(* $MDX part-end *)
(*_ This comment used for spacing MDX comment above *) = struct
  open Async_kernel

  type t = unit

  let create ~uri:_ = ()

  let on_status_change () f =
    Effect.of_thunk (fun () ->
      Clock_ns.every' (Time_ns.Span.of_int_sec 1) (fun () ->
        Effect.Expert.handle_non_dom_event_exn (f `Connecting);
        let%bind.Deferred () = Clock_ns.after (Time_ns.Span.of_int_sec 1) in
        Effect.Expert.handle_non_dom_event_exn (f `Connected);
        let%bind.Deferred () = Clock_ns.after (Time_ns.Span.of_int_sec 3) in
        Effect.Expert.handle_non_dom_event_exn (f `Disconnected);
        return ()))
  ;;
end

(* $MDX part-begin=connection_status *)
let connection_status graph conn : [ `Connected | `Connecting | `Disconnected ] Bonsai.t =
  let status, set_status = Bonsai.state `Disconnected graph in
  let register_status_change =
    let%arr set_status = set_status in
    Connection.on_status_change conn set_status
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:register_status_change graph in
  status
;;

(* $MDX part-end *)

(* $MDX part-begin=connection_status_ui *)
let conn = Connection.create ~uri:"https://google.com"

let connection_status_ui graph =
  let connection_status =
    match%sub connection_status graph conn with
    | `Connected -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Connected" ])
    | `Connecting -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Connecting" ])
    | `Disconnected -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Disconnected" ])
  in
  let%arr connection_status = connection_status in
  Vdom.Node.div [ connection_status ]
;;

(* $MDX part-end *)
let () = Util.run connection_status_ui ~id:"connection_status_ui"

(* $MDX part-begin=resettable_connection_and_counters *)
let connection_and_counters graph =
  let%arr connection_status_ui = connection_status_ui graph
  and counters = two_counters graph in
  Vdom.Node.div [ connection_status_ui; counters ]
;;

let resettable_ui = reset_ui ~f:connection_and_counters

(* $MDX part-end *)

let () = Util.run resettable_ui ~id:"resettable_connection_and_counters"

(* $MDX part-begin=connection_status_reset *)
let connection_status graph conn : [ `Connected | `Connecting | `Disconnected ] Bonsai.t =
  let status, set_status =
    Bonsai.state ~reset:(fun model_when_reset -> model_when_reset) `Disconnected graph
  in
  let register_status_change =
    let%arr set_status = set_status in
    Connection.on_status_change conn set_status
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:register_status_change graph in
  status
;;

(* $MDX part-end *)

let connection_status_ui graph =
  let connection_status =
    match%sub connection_status graph conn with
    | `Connected -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Connected" ])
    | `Connecting -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Connecting" ])
    | `Disconnected -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Disconnected" ])
  in
  let%arr connection_status = connection_status in
  Vdom.Node.div [ connection_status ]
;;

let connection_and_counters graph =
  let%arr connection_status_ui = connection_status_ui graph
  and counters = two_counters graph in
  Vdom.Node.div [ connection_status_ui; counters ]
;;

let () =
  Util.run
    (reset_ui ~f:connection_and_counters)
    ~id:"proper_reset_ui_with_connection_status"
;;

(* $MDX part-begin=exchange_type *)
module Exchange : sig
  module Order_id = Int

  (* A connection to an exchange *)
  type t
  type event = Fill of Order_id.t

  val create : unit -> t

  (* Sends an order to the exchange *)
  val send_order : t -> Order_id.t -> unit Effect.t

  (* Cancels an open order on the exchange *)
  val cancel_order : t -> Order_id.t -> unit Effect.t

  (* Subscribe to notifications of which orders have been filled *)
  val subscribe : t -> (event -> unit Effect.t) -> unit Effect.t
end
(* $MDX part-end *)
(* _ This comment used for spacing MDX comment above *) = struct
  open Async_kernel
  module Order_id = Int

  (* All of this code is pretty hacky and only to enable the demo *)

  type event = Fill of Order_id.t

  type t =
    { orders : int Order_id.Table.t
    ; mutable subscription : event -> unit Effect.t
    }

  let create () =
    { orders = Order_id.Table.create (); subscription = (fun _ -> Effect.Ignore) }
  ;;

  let seq = ref 0

  let next_seq () =
    let res = !seq in
    incr seq;
    res
  ;;

  let fill_in_3_seconds t (order_id, seq) =
    upon
      (Clock_ns.after (Time_ns.Span.of_int_sec 3))
      (fun () ->
        match Hashtbl.find t.orders order_id with
        | Some seq' when seq' = seq ->
          Effect.Expert.handle_non_dom_event_exn (t.subscription (Fill order_id))
        | _ -> ())
  ;;

  (* No checking for duplicate order ids, seems fine. *)
  let send_order t id =
    Effect.of_thunk (fun () ->
      let seq = next_seq () in
      Hashtbl.set t.orders ~key:id ~data:seq;
      fill_in_3_seconds t (id, seq))
  ;;

  let cancel_order t id = Effect.of_thunk (fun () -> Hashtbl.remove t.orders id)
  let subscribe t f = Effect.of_thunk (fun () -> t.subscription <- f)
end

open Exchange

(* $MDX part-begin=order_manager_definition *)
module Model = struct
  type t =
    { open_orders : Order_id.Set.t
    ; filled_orders : Order_id.t list
    ; next_order_id : int
    }

  let empty = { open_orders = Order_id.Set.empty; filled_orders = []; next_order_id = 0 }
end

module Action = struct
  type t =
    | Create_ui_order
    | Received_fill of Exchange.Order_id.t
end

let order_manager (exchange : Exchange.t) graph =
  let model, inject_action =
    Bonsai.state_machine0
      ~default_model:Model.empty
      ~apply_action:
        (fun
          context { open_orders; next_order_id; filled_orders } (action : Action.t) ->
        match action with
        | Create_ui_order ->
          let this_order_id = next_order_id in
          let open_orders = Set.add open_orders this_order_id in
          Apply_action_context.schedule_event
            context
            (Exchange.send_order exchange this_order_id);
          { open_orders; next_order_id = this_order_id + 1; filled_orders }
        | Received_fill order_id ->
          let filled_orders = filled_orders @ [ order_id ] in
          let open_orders = Set.remove open_orders order_id in
          { open_orders; next_order_id; filled_orders })
      graph
  in
  let subscribe_to_exchange =
    let%arr inject_action = inject_action in
    Exchange.subscribe exchange (fun (Fill order_id) ->
      inject_action (Received_fill order_id))
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:subscribe_to_exchange graph in
  let inject_new_order =
    let%arr inject_action = inject_action in
    inject_action Create_ui_order
  in
  let open_orders =
    let%arr { open_orders; _ } = model in
    open_orders
  in
  let filled_orders =
    let%arr { filled_orders; _ } = model in
    filled_orders
  in
  open_orders, filled_orders, inject_new_order
;;

(* $MDX part-end *)

(* $MDX part-begin=trading_ui *)
let trading_ui exchange graph =
  let open_orders, filled_orders, inject_new_order = order_manager exchange graph in
  let%arr open_orders = open_orders
  and filled_orders = filled_orders
  and inject_new_order = inject_new_order in
  Vdom.Node.div
    [ Vdom.Node.text [%string "Open orders:"]
    ; Vdom.Node.sexp_for_debugging [%sexp (open_orders : Order_id.Set.t)]
    ; Vdom.Node.text [%string "Filled orders:"]
    ; Vdom.Node.sexp_for_debugging [%sexp (filled_orders : Order_id.t list)]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> inject_new_order) ]
        [ Vdom.Node.text "New order" ]
    ]
;;

(* $MDX part-end *)

let exchange = Exchange.create ()
let () = Util.run (trading_ui exchange) ~id:"trading_ui"
let () = Util.run (reset_ui ~f:(trading_ui exchange)) ~id:"trading_ui_reset"

let order_manager (exchange : Exchange.t) graph =
  (* $MDX part-begin=order_manager_with_reset *)
  let model, inject_action =
    Bonsai.state_machine0
      ~default_model:Model.empty
      ~apply_action:
        (fun
          context { open_orders; next_order_id; filled_orders } (action : Action.t) ->
        match action with
        | Create_ui_order ->
          let this_order_id = next_order_id in
          let open_orders = Set.add open_orders this_order_id in
          Apply_action_context.schedule_event
            context
            (Exchange.send_order exchange this_order_id);
          { open_orders; next_order_id = this_order_id + 1; filled_orders }
        | Received_fill order_id ->
          let filled_orders = filled_orders @ [ order_id ] in
          let open_orders = Set.remove open_orders order_id in
          { open_orders; next_order_id; filled_orders })
      ~reset:(fun context (model : Model.t) ->
        Set.iter model.open_orders ~f:(fun order_id ->
          Apply_action_context.schedule_event
            context
            (Exchange.cancel_order exchange order_id));
        Model.empty)
      graph
  in
  (* $MDX part-end *)
  let subscribe_to_exchange =
    let%arr inject_action = inject_action in
    Exchange.subscribe exchange (fun (Fill order_id) ->
      inject_action (Received_fill order_id))
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:subscribe_to_exchange graph in
  let inject_new_order =
    let%arr inject_action = inject_action in
    inject_action Create_ui_order
  in
  let open_orders =
    let%arr { open_orders; _ } = model in
    open_orders
  in
  let filled_orders =
    let%arr { filled_orders; _ } = model in
    filled_orders
  in
  open_orders, filled_orders, inject_new_order
;;

(* $MDX part-begin=trading_ui *)
let trading_ui exchange graph =
  let open_orders, filled_orders, inject_new_order = order_manager exchange graph in
  let%arr open_orders = open_orders
  and filled_orders = filled_orders
  and inject_new_order = inject_new_order in
  Vdom.Node.div
    [ Vdom.Node.text [%string "Open orders:"]
    ; Vdom.Node.sexp_for_debugging [%sexp (open_orders : Order_id.Set.t)]
    ; Vdom.Node.text [%string "Filled orders:"]
    ; Vdom.Node.sexp_for_debugging [%sexp (filled_orders : Order_id.t list)]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> inject_new_order) ]
        [ Vdom.Node.text "New order" ]
    ]
;;

(* $MDX part-end *)

let () = Util.run (reset_ui ~f:(trading_ui exchange)) ~id:"proper_trading_ui_reset"
