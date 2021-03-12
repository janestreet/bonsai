open! Core_kernel
open! Bonsai_web

val run : id:string -> Vdom.Node.t Bonsai.Computation.t -> unit
val run_vdom : ?include_html:bool -> Vdom.Node.t -> id:string -> unit
