open! Core_kernel
open! Import
open Component

module Case_action : sig
  type t =
    | T :
        { action : 'a
        ; type_id : 'a Type_equal.Id.t
        ; userdata : 'u
        ; sexp_of_userdata : 'u -> Sexp.t
        }
        -> t
  [@@deriving sexp_of]

  val type_id : t Type_equal.Id.t
end

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | Erase_action :
      { t : ('i, 'm, 'a, 'r, 'incr, 'event) unpacked
      ; action_type_id : 'a Type_equal.Id.t
      ; sexp_of_key : 'k -> Sexp.t
      ; key : 'k
      ; on_action_mismatch : ('k * 'm, 'm) on_action_mismatch
      }
      -> ('i, 'm, Case_action.t, 'r, 'incr, 'event) unpacked
  | Enum :
      { components :
          ('key, ('input, 'model, 'action, 'result, 'incr, 'event) unpacked, 'cmp) Map.t
      ; which : 'input -> 'model -> 'key
      }
      -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
