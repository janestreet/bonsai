open! Base
open Import
open Gen_js_api
include Js_obj_intf

module Make (M : T) = struct
  type t = M.t Js.t

  let t_of_js : Ojs.t -> t = Stdlib.Obj.magic
  let t_to_js : t -> Ojs.t = Stdlib.Obj.magic
end
