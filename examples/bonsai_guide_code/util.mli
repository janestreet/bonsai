open! Core
open! Bonsai_web.Cont

val run
  :  ?custom_connector:(Rpc_effect.Where_to_connect.Custom.t -> Rpc_effect.Connector.t)
  -> id:string
  -> (Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> unit

val run_vdom_val : Vdom.Node.t Bonsai.t -> id:string -> unit
val run_vdom : Vdom.Node.t -> id:string -> unit
