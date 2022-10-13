open! Core
module Types = Types

module Make (Name : Types.Name) : sig
  module Types : module type of Types.Make (Name)
  module Transform : module type of Transform.Make (Name)

  module Value : sig
    include module type of Types.Value

    val named : ?here:Source_code_position.t -> Name.t -> t
    val mapn : ?here:Source_code_position.t -> t list -> t
    val singleton : ?here:Source_code_position.t -> unit -> t
    val fake : t
  end

  module Computation : sig
    include module type of Types.Computation

    val sub : ?here:Source_code_position.t -> bound:t -> as_:Name.t -> for_:t -> unit -> t
    val return : ?here:Source_code_position.t -> Value.t -> t

    val wrap
      :  ?here:Source_code_position.t
      -> name:string
      -> introduces:Name.t list
      -> bodies:t list
      -> unit
      -> t
  end
end
