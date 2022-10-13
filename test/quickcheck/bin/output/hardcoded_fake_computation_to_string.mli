open! Core
open Bonsai

val computation
  : (int, int * (int * (int -> unit Ui_effect.t)), Int.comparator_witness) Base.Map.t
      Computation.t
