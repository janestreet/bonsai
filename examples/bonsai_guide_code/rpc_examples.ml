open! Core
open! Async_kernel
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Lib = Bonsai_guide_code_lib.Rpc_examples

(* The code for these examples is defined inside `lib`, since we want to test it. *)

let () =
  Util.run
    ~custom_connector:Lib.custom_connector
    Lib.double_number_app
    ~id:"double-the-number-rpc"
;;

let () =
  Util.run
    ~custom_connector:Lib.custom_connector
    Lib.current_time_app
    ~id:"poll-the-current-time"
;;
