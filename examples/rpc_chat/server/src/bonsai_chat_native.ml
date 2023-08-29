open! Core
open! Async

let initialize_connection user _initiated_from _addr connection =
  print_s [%message "user joined" (user : Krb.Principal.Name.t)];
  { User_state.user; connection }
;;

let csp =
  Csp_monoid.reduce
    [ Csp_monoid.default_for_clientside
    ; Csp_monoid.frame_ancestor "https://localhost:*"
    (* allow to be iframed in localhost addresses for local dev *)
    ; Csp_monoid.frame_ancestor "https://bonsai:*"
      (* allow to be iframed in https://bonsai/ *)
    ]
  |> Csp_monoid.finalize
;;

let main ~http_settings =
  let global_state = Global_state.create () in
  let respond (_ : Krb.Principal.Name.t) =
    let open Cohttp_static_handler in
    Single_page_handler.create_handler
      (Single_page_handler.default_with_body_div ~div_id:"app")
      ~assets:
        [ Asset.local
            Asset.Kind.javascript
            (Asset.What_to_serve.embedded ~contents:Embedded_files.main_dot_bc_dot_js)
        ]
      ~on_unknown_url:`Not_found
  in
  let%bind server =
    Simple_web_server.create
      ~authorize:Krb_http.Authorize.accept_all
      ~rpc_config:
        (Simple_web_server.Rpc_config.create
           ~implementations:(Rpc_implementations.implementations global_state)
           ~initial_connection_state:initialize_connection)
      ~content_security_policy:(`Block csp)
      http_settings
      respond
  in
  let server = Or_error.ok_exn server in
  Simple_web_server.close_finished server
;;

let command =
  Command.async
    ~summary:"Start server for example [rpc-chat]"
    (let%map.Command http_settings = Http_settings.param () in
     fun () -> main ~http_settings)
    ~behave_nicely_in_pipeline:false
;;
