open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Vdom

module Model = struct
  type t = int Int.Map.t [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | New
    | Update of int * int
  [@@deriving sexp]
end

let default_model = Int.Map.empty

let apply_action ~inject:_ ~schedule_event:_ model = function
  | Action.New -> Map.add_exn model ~key:(Map.length model) ~data:0
  | Update (location, diff) ->
    Map.update model location ~f:(Option.value_map ~default:0 ~f:(( + ) diff))
;;

let component =
  let%sub state =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model
      ~apply_action
  in
  let%arr state, inject = state in
  let button text action =
    Node.button ~attrs:[ Attr.on_click (fun _ -> inject action) ] [ Node.text text ]
  in
  let add_button = button "add" New in
  let for_each i c =
    Node.div
      [ button "-1" (Update (i, -1)); Node.textf "%d" c; button "+1" (Update (i, 1)) ]
  in
  let counters = state |> Map.data |> List.mapi ~f:for_each in
  Node.div (add_button :: counters)
;;

let () = Bonsai_web.Start.start component
