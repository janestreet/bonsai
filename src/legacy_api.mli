open! Core
open! Import

include
  Legacy_api_intf.S
    with type ('input, 'result) t = 'input Cont.t -> Cont.graph -> 'result Cont.t
