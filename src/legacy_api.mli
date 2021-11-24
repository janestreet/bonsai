open! Core
open! Import

include
  Legacy_api_intf.S
  with type ('input, 'result) t = 'input Proc.Value.t -> 'result Proc.Computation.t
