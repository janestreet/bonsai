open! Core
open! Async

let initialize_connection user _initiated_from _addr connection =
  print_s [%message "user joined" (user : Krb.Principal.Name.t)];
  { User_state.user; connection }
;;

let main ~http_settings =
  let global_state = Global_state.create () in
  let respond (_ : Krb.Principal.Name.t) =
    Cohttp_static_handler.Single_page_handler.(
      embedded_js_handler
        (default_with_body_div ~div_id:"app")
        ~scripts:[ Embedded_files.main_dot_bc_dot_js ]
        ~css:[]
        ~on_unknown_url:`Not_found)
  in
  let%bind server =
    Simple_web_server.create
      ~rpc_config:
        (Simple_web_server.Rpc_config.create
           ~implementations:(Rpc_implementations.implementations global_state)
           ~initial_connection_state:initialize_connection)
      http_settings
      respond
  in
  let server = Or_error.ok_exn server in
  Simple_web_server.close_finished server
;;

let command =
  Command.async
    ~summary:"Start server for example [rpc-chat]"
    (let%map.Command http_settings = Http_settings.param ~app_name:"bonsai-example" () in
     fun () -> main ~http_settings)
;;
