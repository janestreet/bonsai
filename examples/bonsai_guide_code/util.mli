open! Core
open! Bonsai_web

val run
  :  ?custom_connector:(Rpc_effect.Where_to_connect.Custom.t -> Rpc_effect.Connector.t)
  -> id:string
  -> Vdom.Node.t Computation.t
  -> unit

val run_vdom : ?include_html:bool -> Vdom.Node.t -> id:string -> unit
