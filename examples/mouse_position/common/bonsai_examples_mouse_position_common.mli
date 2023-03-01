open! Core
open Async_rpc_kernel
open Username_kernel

module Mouse_position : sig
  type t =
    { x : int
    ; y : int
    }
  [@@deriving equal, sexp]
end

module Session : Unique_id.Id

module Active_users : sig
  type t = { active_users : Username.t Session.Map.t } [@@deriving equal, sexp]
end

module Protocol : sig
  module Active_users : sig
    val rpc : (unit, Active_users.t) Polling_state_rpc.t
  end

  module Set_mouse_position : sig
    val rpc : (Mouse_position.t, unit) Rpc.Rpc.t
  end

  module Get_mouse_position : sig
    val rpc : (Session.t, Mouse_position.t option) Rpc.Rpc.t
  end
end

module Connection_state : sig
  type t =
    { user : Username.t
    ; session : Session.t
    ; connection : Rpc.Connection.t
    }
end

module Rpc_implementations : sig
  val create
    :  on_client_and_server_out_of_sync:(Sexp.t -> unit)
    -> Connection_state.t Rpc.Implementation.t list
end
