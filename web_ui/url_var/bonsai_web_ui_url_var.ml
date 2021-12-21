open! Core
open! Bonsai_web
module History = Html5_history.Opinionated

let get_uri () =
  let open Js_of_ocaml in
  Dom_html.window##.location##.href |> Js.to_string |> Uri.of_string
;;

module Components = struct
  type t =
    { path : string
    ; query : string list String.Map.t
    ; fragment : string option
    }
  [@@deriving sexp, equal]

  let create ?(path = "") ?(query = String.Map.empty) ?(fragment = None) () =
    { path; query; fragment }
  ;;

  let to_path_and_query { path; query; fragment } =
    let uri = get_uri () in
    uri
    |> Fn.flip Uri.with_path path
    |> Fn.flip Uri.with_query (Map.to_alist query)
    |> Fn.flip Uri.with_fragment fragment
  ;;
end

module type S = sig
  type t [@@deriving sexp, equal]

  val parse_exn : Components.t -> t
  val unparse : t -> Components.t
end

module type S_via_sexp = sig
  type t [@@deriving sexp, equal]
end

module Literally_just_a_gigantic_sexp (M : S_via_sexp) : S with type t = M.t = struct
  include M

  let query_param_name = "query"

  let parse_exn { Components.query; _ } =
    Map.find_exn query query_param_name |> List.hd_exn |> Sexp.of_string |> [%of_sexp: t]
  ;;

  let unparse t =
    let uri = get_uri () in
    let param = Sexp.to_string ([%sexp_of: t] t) in
    { Components.path = Uri.path uri
    ; query = String.Map.singleton query_param_name [ param ]
    ; fragment = Uri.fragment uri
    }
  ;;
end

type 'a t =
  { var : 'a Bonsai.Var.t
  ; setter : 'a -> unit
  ; effect : 'a -> unit Effect.t
  }

let create_exn (type a) (module S : S with type t = a) ~fallback =
  let module Uri_routing = struct
    include S

    let parse uri =
      let path = Uri.path uri |> String.chop_prefix_if_exists ~prefix:"/" in
      let query =
        uri
        |> Uri.query
        |> String.Map.of_alist_multi
        |> Map.filter_map ~f:(function
          | [ value ] -> Some value
          | _ -> None)
      in
      let fragment = Uri.fragment uri in
      let components = { Components.path; query; fragment } in
      match parse_exn components with
      | a -> Ok a
      | exception e ->
        eprint_s [%message "couldn't parse uri" (components : Components.t) (e : exn)];
        Error `Not_found
    ;;

    let to_path_and_query uri = Components.to_path_and_query (unparse uri)
  end
  in
  let module History_state = struct
    type uri_routing = a

    include S

    include Binable.Of_sexpable_with_uuid (struct
        include S

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "918e794b-02c3-4f27-ad86-3f406a41fc4b"
        ;;
      end)

    let to_uri_routing = Fn.id
    let of_uri_routing = Fn.id
  end
  in
  let t =
    History.init_exn
      ~log_s:(ignore : Sexp.t -> unit)
      (module History_state)
      (module Uri_routing)
      ~on_bad_uri:(`Default_state fallback)
  in
  let value = History.current t in
  let var = Bonsai.Var.create value in
  let setter = History.update t in
  let effect =
    Effect.of_sync_fun (fun a ->
      setter a;
      Bonsai.Var.set var a)
  in
  Bus.iter_exn (History.changes_bus t) [%here] ~f:(Bonsai.Var.set var);
  { var; setter; effect }
;;

let set { var; setter; effect = _ } a =
  setter a;
  Bonsai.Var.set var a
;;

let value { var; setter = _; effect = _ } = Bonsai.Var.value var

let update { var; setter; effect = _ } ~f =
  Bonsai.Var.update var ~f:(fun old ->
    let new_ = f old in
    setter new_;
    new_)
;;

let get { var; _ } = Bonsai.Var.get var
let set_effect { effect; _ } = effect
