open! Base
open Import
open Gen_js_api

module type S = sig
  type t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
end

module type Js_obj = sig
  module type S = S

  (** This just provides an Obj.magic-based implementation of [t_to_js] and [t_of_js]
      since Js.t objects are, in fact, objects in javascript (e.g. Ojs). *)
  module Make (M : T) : S with type t = M.t Js.t
end
