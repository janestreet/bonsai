open! Core_kernel
open! Async_kernel
open! Import
include Bonsai.Effect
module Callback = Private.Callback

let of_deferred_fun (type query result) f =
  let module E =
    Vdom.Event.Define (struct
      module Action = struct
        type t = (query, result) Callback.t
      end

      let handle action =
        don't_wait_for
          (let%map.Deferred result = f (Callback.request action) in
           let evt = Callback.respond_to action result in
           Vdom.Event.Expert.handle_non_dom_event_exn evt)
      ;;
    end)
  in
  let evaluator = E.inject in
  stage (fun request -> Private.make ~request ~evaluator)
;;
