open! Core
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect
open Bonsai.Let_syntax

let pipe (type a) ?(sexp_of = sexp_of_opaque) (local_ graph) =
  let module Model = struct
    type t =
      { queued_actions : a Fdeque.t
      ; queued_receivers : (unit, a) Effect.Private.Callback.t Fdeque.t
      }

    let equal = phys_equal
    let default = { queued_actions = Fdeque.empty; queued_receivers = Fdeque.empty }
    let sexp_of_t { queued_actions; _ } = Fdeque.sexp_of_t sexp_of queued_actions
  end
  in
  let module Action = struct
    type t =
      | Add_action of a
      | Add_receiver of (unit, a) Effect.Private.Callback.t

    let sexp_of_t = function
      | Add_action a -> sexp_of a
      | Add_receiver r -> sexp_of_opaque r
    ;;
  end
  in
  let _, inject =
    Bonsai.state_machine
      graph
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model:Model.default
      ~apply_action:(fun context model -> function
      | Add_action a ->
        (match Fdeque.dequeue_front model.queued_receivers with
         | None ->
           let queued_actions = Fdeque.enqueue_back model.queued_actions a in
           { model with queued_actions }
         | Some (hd, queued_receivers) ->
           Bonsai.Apply_action_context.schedule_event
             context
             (Effect.Private.Callback.respond_to hd a);
           { model with queued_receivers })
      | Add_receiver r ->
        (match Fdeque.dequeue_front model.queued_actions with
         | None ->
           let queued_receivers = Fdeque.enqueue_back model.queued_receivers r in
           { model with queued_receivers }
         | Some (hd, queued_actions) ->
           Bonsai.Apply_action_context.schedule_event
             context
             (Effect.Private.Callback.respond_to r hd);
           { model with queued_actions }))
  in
  let enqueue =
    let%arr inject in
    fun a -> inject (Add_action a)
  in
  let dequeue =
    let%arr inject in
    Effect.Private.make ~request:() ~evaluator:(fun r ->
      Effect.Expert.handle
        (inject (Add_receiver r))
        ~on_exn:(Effect.Private.Callback.on_exn r))
  in
  enqueue, dequeue
;;
