open! Core_kernel
open! Import

type ('input, 'model, 'action, 'result, 'incr, 'event) Component.unpacked +=
  | Pure_input :
      ('input -> 'result)
      -> ('input, _, Nothing.t, 'result, _, _) Component.unpacked
  | Pure_model :
      ('model -> 'result)
      -> (_, 'model, Nothing.t, 'result, _, _) Component.unpacked
  | Return_input : ('input, _, Nothing.t, 'input, _, _) Component.unpacked
  | Return_model : (_, 'model, Nothing.t, 'model, _, _) Component.unpacked
