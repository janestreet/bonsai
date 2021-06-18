open! Core
open! Async_kernel
open! Import
open Js_of_ocaml

type 'a t =
  { var : 'a Bonsai.Var.t
  ; setter : 'a -> unit
  ; clear : unit -> unit
  ; effect : 'a -> unit Effect.t
  }

let getter_setter kind =
  let open Option.Let_syntax in
  let%map storage =
    match kind with
    | `Local_storage -> Dom_html.window##.localStorage |> Js.Optdef.to_option
    | `Session_storage -> Dom_html.window##.sessionStorage |> Js.Optdef.to_option
  in
  let set key value = storage##setItem (Js.string key) (Js.string value) in
  let get key =
    storage##getItem (Js.string key) |> Js.Opt.to_option |> Option.map ~f:Js.to_string
  in
  let delete key = storage##removeItem (Js.string key) in
  get, set, delete
;;

let create (type a) (module M : Sexpable with type t = a) kind ~unique_id ~default =
  let getter, setter, deleter =
    match getter_setter kind with
    | Some (getter, setter, delete) -> getter, setter, delete
    | None -> (fun _key -> None), (fun _key _value -> ()), fun _key -> ()
  in
  let value =
    match getter unique_id with
    | None ->
      eprint_s
        [%message
          "WARNING: Could not find a sexp for persistent_var" (unique_id : string)];
      default
    | Some sexp ->
      (match Or_error.try_with (fun () -> M.t_of_sexp (Sexp.of_string sexp)) with
       | Ok a -> a
       | Error e ->
         eprint_s
           [%message
             "WARNING: Could not deserialize persistent-var"
               (unique_id : string)
               (e : Error.t)];
         default)
  in
  let var = Bonsai.Var.create value in
  let setter t = t |> M.sexp_of_t |> Sexp.to_string_mach |> setter unique_id in
  let clear () = deleter unique_id in
  let effect =
    unstage
      (Effect.of_sync_fun (fun a ->
         setter a;
         Bonsai.Var.set var a))
  in
  { var; setter; clear; effect }
;;

let set { var; setter; clear = _; effect = _ } a =
  setter a;
  Bonsai.Var.set var a
;;

let value { var; setter = _; clear = _; effect = _ } = Bonsai.Var.value var

let update { var; setter; clear = _; effect = _ } ~f =
  Bonsai.Var.update var ~f:(fun old ->
    let new_ = f old in
    setter new_;
    new_)
;;

let get { var; _ } = Bonsai.Var.get var
let clear_persistence { var = _; setter = _; clear; effect = _ } = clear ()
let effect { effect; _ } = effect
