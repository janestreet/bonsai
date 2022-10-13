open! Core
open Types

module Make (Name : Name) : sig
  open Make(Name)

  val replace_v : Value.t -> from:Name.t -> to_:Name.t -> Value.t
  val replace_c : Computation.t -> from:Name.t -> to_:Name.t -> Computation.t

  val organize_bindings
    :  Binding.t list
    -> curr_id:Name.Count.t
    -> last_body:Computation.t
    -> point_to:Name.t
    -> Binding.t list list * Name.Count.t
end
