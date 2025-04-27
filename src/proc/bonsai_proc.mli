open! Core
module Cont := Bonsai

(** [Bonsai_proc] is a legacy Bonsai API based on [Value.t] and [Computation.t]. New apps
    should use the [Bonsai] library instead. *)

include
  Proc_intf.S
  with type 'a Value.t = 'a Cont.t
   and type 'a Computation.t = local_ Cont.graph -> 'a Cont.t
   and type 'a Computation_status.t = 'a Cont.Computation_status.t
   and type 'a Dynamic_scope.t = 'a Cont.Dynamic_scope.t
   and type 'a Effect_throttling.Poll_result.t = 'a Cont.Effect_throttling.Poll_result.t
   and type 'a Var.t = 'a Cont.Expert.Var.t

module Private = Bonsai.Private

module Bonsai : sig
  include
    Proc_intf.S
    with type 'a Value.t = 'a Cont.t
     and type 'a Computation.t = local_ Cont.graph -> 'a Cont.t
     and type 'a Computation_status.t = 'a Cont.Computation_status.t
     and type 'a Dynamic_scope.t = 'a Cont.Dynamic_scope.t
     and type 'a Effect_throttling.Poll_result.t = 'a Cont.Effect_throttling.Poll_result.t
     and type 'a Var.t = 'a Cont.Expert.Var.t

  module Private = Bonsai.Private
end
