open Ppxlib

val register : unit -> unit

module For_testing : sig
  val create_structure : loc:location -> structure_item -> value_binding -> structure
end
