open! Core
open Async_rpc_kernel
open Username_kernel

module Mouse_position = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving bin_io, equal, sexp]
end

module Session = Unique_id.Int ()

module Active_users = struct
  type t = { active_users : Username.t Session.Map.t [@diff.map] }
  [@@deriving diff, bin_io, equal, sexp]
end

module Protocol = struct
  module Active_users = struct
    let rpc =
      Polling_state_rpc.create
        ~name:"active_users"
        ~version:0
        ~query_equal:[%equal: unit]
        ~bin_query:[%bin_type_class: unit]
        (module Active_users)
    ;;
  end

  module Set_mouse_position = struct
    let rpc =
      Rpc.Rpc.create
        ~name:"set_mouse_position"
        ~version:0
        ~bin_query:[%bin_type_class: Mouse_position.t]
        ~bin_response:[%bin_type_class: unit]
    ;;
  end

  module Get_mouse_position = struct
    let rpc =
      Rpc.Rpc.create
        ~name:"get_mouse_position"
        ~version:0
        ~bin_query:[%bin_type_class: Session.t]
        ~bin_response:[%bin_type_class: Mouse_position.t option]
    ;;
  end
end

module Connection_state = struct
  type t =
    { user : Username.t
    ; session : Session.t
    ; connection : Rpc.Connection.t
    }
end

module Rpc_implementations = struct
  open Async_kernel

  let create ~on_client_and_server_out_of_sync =
    (* The body of this function is a module just to get the benefit of
       vertical whitespace separation. *)
    let module M = struct
      let last_move_time = ref Session.Map.empty
      let active_users_ref = ref Session.Map.empty
      let mouse_positions = ref Session.Map.empty

      let active_users =
        Polling_state_rpc.implement
          Protocol.Active_users.rpc
          ~on_client_and_server_out_of_sync
          (fun _connection_state _query ->
             return { Active_users.active_users = !active_users_ref })
        |> Rpc.Implementation.lift
             ~f:(fun ({ Connection_state.connection; _ } as user_state) ->
               user_state, connection)
      ;;

      let set_mouse_position =
        Rpc.Rpc.implement'
          Protocol.Set_mouse_position.rpc
          (fun { Connection_state.user; session; connection = _ } mouse_position ->
             last_move_time := Map.set !last_move_time ~key:session ~data:(Time_ns.now ());
             active_users_ref := Map.set !active_users_ref ~key:session ~data:user;
             mouse_positions := Map.set !mouse_positions ~key:session ~data:mouse_position)
      ;;

      let get_mouse_position =
        Rpc.Rpc.implement'
          Protocol.Get_mouse_position.rpc
          (fun _connection_state session -> Map.find !mouse_positions session)
      ;;

      let implementations () =
        Async_kernel.every (Time_ns.Span.of_sec 1.0) (fun () ->
          let twenty_seconds_ago =
            Time_ns.sub (Time_ns.now ()) (Time_ns.Span.of_sec 20.0)
          in
          last_move_time
          := Map.filter !last_move_time ~f:(fun t -> Time_ns.( > ) t twenty_seconds_ago);
          active_users_ref
          := Map.filter_keys !active_users_ref ~f:(Map.mem !last_move_time);
          mouse_positions := Map.filter_keys !mouse_positions ~f:(Map.mem !last_move_time));
        [ active_users; set_mouse_position; get_mouse_position ]
      ;;
    end
    in
    M.implementations ()
  ;;
end
