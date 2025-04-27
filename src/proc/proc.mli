open! Core
module Cont := Bonsai

include
  Proc_intf.S
  with type 'a Value.t = 'a Cont.t
   and type 'a Computation.t = local_ Cont.graph -> 'a Cont.t
   and type 'a Computation_status.t = 'a Cont.Computation_status.t
   and type 'a Dynamic_scope.t = 'a Cont.Dynamic_scope.t
   and type 'a Effect_throttling.Poll_result.t = 'a Cont.Effect_throttling.Poll_result.t
   and type 'a Var.t = 'a Cont.Expert.Var.t
