open! Core_kernel
open Bonsai_web

module Model : sig
  type t
end

module Add_counter_component : sig
  module Action : T
end

module Counter_component : sig
  module Action : T
end

val application_component : (unit, Model.t, Vdom.Node.t) Bonsai.t
val initial_model : Model.t
