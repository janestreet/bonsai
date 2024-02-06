open! Core
open! Import

include
  Proc_intf.S
    with type 'a Value.t = 'a Cont.t
     and type 'a Computation.t = Cont.graph -> 'a Cont.t
