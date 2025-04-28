open! Core

module Location_behavior : sig
  type t =
    | Location_of_callsite (** Uses [~here:[%here]] *)
    | Location_in_scope (** Uses [~here:here] *)
end

val sub : Location_behavior.t -> Ppx_let_expander.t
val arr : Location_behavior.t -> Ppx_let_expander.t

module For_testing : sig
  module Balance_list_tree = Balance_list_tree
end
