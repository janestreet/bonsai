open! Core_kernel
open! Async

let initialize_connection user _initiated_from _addr connection =
  print_s [%message "user joined" (user : Krb.Principal.Name.t)];
  { User_state.user; connection }
;;

let main ~http_settings =
  let global_state = Global_state.create () in
  let%bind server =
    Simple_web_server.create
      ~rpc_config:
        (Simple_web_server.Rpc_config.create
           ~implementations:(Rpc_implementations.implementations global_state)
           ~initial_connection_state:initialize_connection)
      http_settings
      (Fn.const
         Cohttp_static_handler.Single_page_handler.(
           js_handler
             (default_with_body_div ~div_id:"app")
             ~js_files:[ "../../client/main.bc.js" ]
             ~css_files:[ "../../client/style.css" ]
             ~on_unknown_url:`Not_found))
  in
  let server = Or_error.ok_exn server in
  Simple_web_server.close_finished server
;;

let command_serve =
  Command.async
    ~summary:"start server"
    (let%map_open.Command http_settings =
       Http_settings.param ~app_name:"bonsai-chat-example" ()
     in
     fun () -> main ~http_settings)
;;

let command = Command.group ~summary:"rpc_chat server" [ "serve", command_serve ]
