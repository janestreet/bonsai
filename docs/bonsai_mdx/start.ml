open! Core_kernel
open! Import

module Handle = struct
  type (_, _) t = unit

  let stop _ = failwith "unimplemented"
  let started _ = failwith "unimplemented"
  let schedule _ _ = failwith "unimplemented"
  let set_input _ _ = failwith "unimplemented"
end

let start_standalone ~initial_input:_ ~initial_model:_ ~bind_to_element_with_id:_ _ = ()
let start ~initial_input:_ ~initial_model:_ ~bind_to_element_with_id:_ _ = ()
