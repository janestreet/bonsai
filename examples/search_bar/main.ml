open! Core
open Bonsai_web
open Bonsai.Let_syntax

module Input = struct
  type t = { all_users : User_info.t String.Map.t } [@@deriving fields]

  let default () = Fields.create ~all_users:(Lazy.force User_info.sample_data)
end

let selected_display =
  let open Vdom in
  Bonsai.Arrow_deprecated.pure ~f:(fun selected_user ->
    match selected_user with
    | None -> Node.div [ Node.text "No user selected" ]
    | Some ({ name; int_id } : User_info.t) ->
      Node.div
        [ Node.text "Selected user"
        ; Node.br ()
        ; Node.textf "name : %s , id %d" name int_id
        ])
;;

let set_model_component =
  let module User_opt = struct
    type t = User_info.t option [@@deriving equal, sexp]
  end
  in
  Bonsai.state [%here] (module User_opt) ~default_model:None
;;

let to_server_input input =
  let%sub set_model = set_model_component in
  return
  @@ let%map current_user, inject_set_model = set_model
  and all_users = input >>| Input.all_users in
  let choices =
    all_users |> Map.data |> List.map ~f:Search_bar.Username.of_user_info
  in
  let on_select username =
    username |> Search_bar.Username.to_string |> Map.find all_users |> inject_set_model
  in
  current_user, Search_bar.Input.create ~choices ~on_select
;;

let component input =
  let%sub current_user, search_bar_input = to_server_input input in
  let%sub selected = selected_display current_user in
  let%sub search_bar = Search_bar.component search_bar_input in
  return
    (let%map selected = selected
     and search_bar = search_bar in
     Vdom.Node.div [ search_bar; selected ])
;;

let () =
  let input = Bonsai.Var.create (Input.default ()) in
  let (_ : _ Bonsai_web.Start.Handle.t) =
    Start.start
      ~bind_to_element_with_id:"app"
      Start.Result_spec.just_the_view
      (component (Bonsai.Var.value input))
  in
  ()
;;
