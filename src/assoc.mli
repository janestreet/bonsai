open! Core_kernel
open! Import
open Component

type ('data, 'result, 'k, 'cmp, 'r_by_k, 'd_by_k) t =
  { r_by_k : ('r_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
  ; d_by_k : ('d_by_k, ('k, 'data, 'cmp) Map.t) Type_equal.t
  }

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | By_model :
      { t : ('k * 'input, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; action_type_id : 'action Type_equal.Id.t
      ; sexp_of_key : 'k -> Sexp.t
      ; assoc : ('model, 'result, 'k, 'cmp, 'r_by_k, 'm_by_k) t
      }
      -> ('input, 'm_by_k, 'k * 'action, 'r_by_k, 'incr, 'event) unpacked
  (** We need the Type_equal witnesses here because the typechecker's rules aren't
      powerful enough to just have the Comparator.t here. *)
  | By_input :
      { t : ('k * 'input, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; action_type_id : 'action Type_equal.Id.t
      ; sexp_of_key : 'k -> Sexp.t
      ; assoc : ('input, 'result, 'k, 'cmp, 'r_by_k, 'i_by_k) t
      }
      -> ('i_by_k, 'model, 'k * 'action, 'r_by_k, 'incr, 'event) unpacked
  (** We need the Type_equal witnesses here because the typechecker's rules aren't
      powerful enough to just have the Comparator.t here. *)
