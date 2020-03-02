open! Core_kernel
open! Import
open Component

(** {v
                  outer             outer'
                    |                 ^
                    |                 |
               get  |            set  +----- outer
                    |                 |
                    v                 |
                  inner             inner'
         v} *)
type ('m_up, 'm_down) t =
  { get : 'm_up -> 'm_down
  ; set : 'm_up -> 'm_down -> 'm_up
  }

val compose : ('high, 'mid) t -> ('mid, 'low) t -> ('high, 'low) t

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { t : ('input, 'm1, 'action, 'result, 'incr, 'event) unpacked
      ; projection : ('m2, 'm1) t
      }
      -> ('input, 'm2, 'action, 'result, 'incr, 'event) unpacked
