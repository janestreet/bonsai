open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t : ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
      ; on_action_for_none : (unit, 'model option) on_action_mismatch
      }
      -> ('input, 'model option, 'action, 'result option, 'incr, 'event) unpacked
