open! Core
open Bonsai_web
open Bonsai.Let_syntax


module User_info = struct
  type t =
    { name : string
    ; int_id : int
    }
  [@@deriving compare, equal, fields, sexp]

  let sample_data =
    List.mapi [ "prod"; "dev"; "test" ] ~f:(fun i suffix ->
      List.mapi [ "bonsai"; "incremental"; "app" ] ~f:(fun j name ->
        let name = String.concat ~sep:"-" [ name; suffix ] in
        let int_id = (10 * i) + j in
        name, Fields.create ~name ~int_id))
    |> List.concat
    |> String.Map.of_alist_exn
  ;;
end

module Search_bar = struct
  module Username = struct
    type t = { username : string } [@@deriving compare, equal, fields, sexp]

    let of_user_info user_info = Fields.create ~username:(User_info.name user_info)
    let to_string t = username t
    let of_string username = Fields.create ~username |> Option.some
  end

  module Input = struct
    include Bonsai_web_ui_search_bar.Input

    let create = Fields.create
  end

  let component =
    Bonsai_web_ui_search_bar.create
      (module Username)
      ~of_string:Username.of_string
      ~additional_query_results_on_click:2
      ~max_query_results:5
      ()
  ;;
end

module Input = struct
  type t = { all_users : User_info.t String.Map.t } [@@deriving fields]

  let default () = { all_users = User_info.sample_data }
end

let selected_display selected_user =
  match%arr selected_user with
  | None -> Vdom.Node.div [ Vdom.Node.text "No user selected" ]
  | Some ({ name; int_id } : User_info.t) ->
    Vdom.Node.div
      [ Vdom.Node.text "Selected user"
      ; Vdom.Node.br ()
      ; Vdom.Node.textf "name : %s , id %d" name int_id
      ]
;;

let set_model_component =
  let module User_opt = struct
    type t = User_info.t option [@@deriving equal, sexp]
  end
  in
  Bonsai.state (module User_opt) ~default_model:None
;;

let to_server_input input =
  let%sub set_model = set_model_component in
  let%arr current_user, inject_set_model = set_model
  and all_users = input >>| Input.all_users in
  let choices = all_users |> Map.data |> List.map ~f:Search_bar.Username.of_user_info in
  let on_select username =
    username |> Search_bar.Username.to_string |> Map.find all_users |> inject_set_model
  in
  current_user, Search_bar.Input.create ~choices ~on_select
;;

let component input =
  let%sub current_user, search_bar_input = to_server_input input in
  let%sub selected = selected_display current_user in
  let%sub search_bar = Search_bar.component search_bar_input in
  let%arr selected = selected
  and search_bar = search_bar in
  Vdom.Node.div [ search_bar; selected ]
;;

let () =
  let input = Bonsai.Var.create (Input.default ()) in
  Bonsai_web.Start.start (component (Bonsai.Var.value input))
;;
