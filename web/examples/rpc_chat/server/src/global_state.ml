open! Core
open! Async
open Bonsai_chat_common

type t =
  { message_bus : (Message.t -> unit) Bus.Read_write.t
  ; messages : Message.t Queue.t Room.Table.t
  }

let create () =
  let message_bus =
    Bus.create
      [%here]
      Bus.Callback_arity.Arity1
      ~on_subscription_after_first_write:Bus.On_subscription_after_first_write.Allow
      ~on_callback_raise:(fun error -> print_s [%sexp (error : Error.t)])
  in
  let messages =
    Room.Table.of_alist_exn
      [ ( Room.of_string "incr_dom-room"
        , Queue.of_list
            [ Message.
                { room = Room.of_string "bonsai-room"
                ; author = "Bonsai Developers"
                ; contents = "hello world!"
                }
            ] )
      ; ( Room.of_string "bonsai-room"
        , Queue.of_list
            [ Message.
                { room = Room.of_string "bonsai-room"
                ; author = "Bonsai Developers"
                ; contents = "hello world!"
                }
            ] )
      ]
  in
  { message_bus; messages }
;;
