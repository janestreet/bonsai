open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module State = struct
  type 'a t =
    { current : 'a
    ; set : 'a -> unit Ui_effect.t
    }
  [@@deriving fields]

  let create = Fields.create
end

module Result = struct
  type 'a t =
    { tabs : Vdom.Node.t list
    ; current : 'a
    }

  let combine_trivially { tabs; current } =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "bonsai_ui_tab_container" ]
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "bonsai_ui_tab_tabs" ] tabs
      ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "bonsai_ui_tab_body" ] [ current ]
      ]
  ;;
end

let tab_state (type t) ?equal (module M : Bonsai.Model with type t = t) ~initial =
  let%sub state_result = Bonsai.state initial ~sexp_of_model:[%sexp_of: M.t] ?equal in
  let%arr current, set = state_result in
  State.create ~current ~set
;;

let tab_ui
      (type t)
      ?decorate
      ?additional_button_attributes
      (module M : Bonsai.Model with type t = t)
      ~all_tabs
      ~equal
      state
      ~f
  =
  let default_additional_button_attributes () =
    Value.return (fun ~is_selected:_ _ -> Vdom.Attr.empty)
  in
  let default_decorate sexp_of_t =
    Value.return (fun t -> t |> sexp_of_t |> Sexp.to_string_hum |> Vdom.Node.text)
  in
  let decorate = Option.value decorate ~default:(default_decorate M.sexp_of_t) in
  let additional_button_attributes =
    Option.value
      additional_button_attributes
      ~default:(default_additional_button_attributes ())
  in
  let tab_to_button =
    let%map state = state
    and decorate = decorate
    and additional_button_attributes = additional_button_attributes in
    fun kind ->
      let is_selected = equal kind (State.current state) in
      let selected_attr =
        if is_selected then Vdom.Attr.class_ "selected" else Vdom.Attr.empty
      in
      let attr =
        Vdom.Attr.(
          on_click (fun _ -> State.set state kind)
          @ selected_attr
          @ class_ "bonsai_ui_tab"
          @ name (kind |> M.sexp_of_t |> Sexp.to_string_mach)
          @ additional_button_attributes ~is_selected kind)
      in
      Vdom.Node.button ~attrs:[ attr ] [ decorate kind ]
  in
  let tabs =
    let%map all_tabs = all_tabs
    and tab_to_button = tab_to_button in
    List.map all_tabs ~f:tab_to_button
  in
  let%sub result =
    let change_tab = state >>| State.set in
    let current = state >>| State.current in
    f ~change_tab current
  in
  let%arr current = result
  and tabs = tabs in
  { Result.tabs; current }
;;
