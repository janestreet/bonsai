open! Core
open! Async_kernel
open! Import
include Bonsai.Effect

module Deferred_fun_arg = struct
  module Action = struct
    type 'r t = T : 'a * ('a -> 'r Deferred.t) -> 'r t
  end

  let handle (Action.T (a, f)) ~on_response =
    don't_wait_for
      (let%map.Deferred result = f a in
       on_response result)
  ;;
end

module Deferred_fun = Ui_effect.Define1 (Deferred_fun_arg)

let of_deferred_fun f a = Deferred_fun.inject (T (a, f))
