open! Core
module Bonsai = Bonsai.Cont
open Bonsai.Let_syntax

module Status = struct
  type t =
    | Busy
    | Idle
  [@@deriving sexp, equal]
end

module Response = struct
  type 'a t =
    | Result of 'a
    | Exn of Exn.t
    | Busy
  [@@deriving sexp_of]
end

module Lock_action = struct
  type t =
    | Acquire
    | Release
  [@@deriving sexp]
end

let effect f (local_ graph) =
  let status, inject_status =
    Bonsai.actor
      graph
      ~sexp_of_model:[%sexp_of: Status.t]
      ~equal:[%equal: Status.t]
      ~sexp_of_action:[%sexp_of: Lock_action.t]
      ~default_model:Idle
      ~recv:(fun _ctx model action ->
        match action with
        | Acquire ->
          let response =
            match model with
            | Busy -> false
            | Idle -> true
          in
          Busy, response
        | Release -> Idle, true)
  in
  let effect =
    let%arr inject_status and f in
    let open Ui_effect.Let_syntax in
    fun query ->
      match%bind inject_status Acquire with
      | false -> return Response.Busy
      | true ->
        let result_effect =
          Ui_effect.protect
            (Ui_effect.lazy_ (lazy (f query)))
            ~finally:(inject_status Release |> Ui_effect.ignore_m)
        in
        (match%map Ui_effect.try_with result_effect with
         | Ok result -> Response.Result result
         | Error exn -> Response.Exn exn)
  in
  effect, status
;;
