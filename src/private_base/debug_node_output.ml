open! Core

let output_ref : (string -> unit) ref = ref (Core.Debug.eprintf "%s")
let output s = !output_ref s
let get_output () = !output_ref
let set_output f = output_ref := f
