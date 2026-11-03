(** Stores only a ref used for [debug_node] output. [Bonsai.Private_base] has
    [(visibility (restricted ...))] in the jbuild so that framework-level code (bonsai,
    bonsai_web, bonsai_term, etc.) can configure how [debug_node] messages are printed
    without exposing the ref in Bonsai's public API. *)

(** Output a [debug_node] message *)
val output : string -> unit

(** Retrieve the current output function. Pair with [set_output] to save and restore it. *)
val get_output : unit -> string -> unit

(** Override the output function. Defaults to [Core.Debug.eprintf]. *)
val set_output : (string -> unit) -> unit
