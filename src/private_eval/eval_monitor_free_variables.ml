open! Core
open! Import

(* extends the environment with a new version of the incremental identified by [id] with
   an incremental that prints whenever the input changes. *)
let make_value_loud ~here environment id =
  match Environment.find environment id with
  | None ->
    (* not being able to find this value is probably a bug, but let's not crash inside
       this debugging utility.  *)
    environment
  | Some value ->
    let incr_info = (Incr.user_info value : Info.t option) in
    let loud_value =
      Incr.map value ~f:(fun a ->
        print_s
          [%message
            "node updated"
              ~monitor:(here : Source_code_position.t)
              (incr_info : Info.t option)];
        a)
    in
    Environment.add_overwriting environment ~key:id ~data:loud_value
;;

let f
  (type a)
  ~(gather : a Computation.gather_fun)
  ~recursive_scopes
  ~time_source
  ~inner
  ~here
  ~free_vars
  =
  let%bind.Trampoline (T inner) = gather ~recursive_scopes ~time_source inner in
  let run ~environment ~fix_envs ~path ~model ~inject =
    let environment =
      Type_id_set.fold
        free_vars
        ~init:environment
        { f = (fun env id -> make_value_loud ~here env id) }
    in
    inner.run ~environment ~fix_envs ~path ~model ~inject
  in
  Trampoline.return (Computation.T { inner with run })
;;
