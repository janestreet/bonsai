open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

module Style = struct
  include
    [%css
    stylesheet
      {|
      .main {
        padding: 24px;
        display: flex;
        flex-direction: column;
        gap: 24px;
        }

      .grid {
        display: flex;
        flex-wrap: wrap;
        justify-content: flex-start;
        gap: 12px;
      }

      .icon {
        width: 96px;
        height: 96px;
        padding: 4px;
        box-sizing: border-box;
        background: #fff;

        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;

        border-radius: 2px;
        border: 1px solid #ddd;
        cursor: pointer;
        overflow: hidden;
      }

      .icon:hover {
        background: #ddd;
      }

      .icon svg {
        margin: 8px;
        flex-shrink: none;
      }

      .icon p {
        width: 100%;
        height: 24px;
        margin: 0;
        overflow: hidden;

        font-family: roboto, sans-serif;
        text-align: center;
        font-size: 12px;
        line-height: 12px;
        text-overflow: ellipsis;
      }

      .search {
        display: flex;
        align-items: center;
        gap: 8px;
      }

      .search input {
        padding: 4px 8px;
        border-radius: 0;
        border: 1px solid #000;
      }
      .search input:active, .search input:focus {
        border: 1px solid #5555fe;
        outline: none;
      }
  |}]
end

module Temporary_toggle = struct
  let state ~base ~temporary timeout graph =
    let state =
      Tuple2.uncurry Bonsai.both
      @@ Bonsai.state
           Time_ns.min_value_representable
           ~sexp_of_model:[%sexp_of: Time_ns.Alternate_sexp.t]
           ~equal:[%equal: Time_ns.Alternate_sexp.t]
           graph
    in
    let toggle_back_time =
      Bonsai.map state ~f:(fun (last_set_time, _) -> Time_ns.add last_set_time timeout)
    in
    let toggle = Bonsai.Clock.at toggle_back_time graph in
    let get_now = Bonsai.Clock.get_current_time graph in
    let%arr _, set_time = state
    and toggle = toggle
    and get_now = get_now in
    let output =
      match toggle with
      | Before -> temporary
      | After -> base
    in
    let turn_on =
      let%bind.Effect now = get_now in
      set_time now
    in
    output, turn_on
  ;;
end

module Icon_grid = struct
  let icon_card icon graph =
    let copied =
      Temporary_toggle.state
        ~base:`Show_icon
        ~temporary:`Show_copied
        Time_ns.Span.second
        graph
    in
    let%arr copied, set_copied = copied
    and icon = icon in
    let name = Codicons.name icon in
    let variant_name =
      String.map name ~f:(function
        | '-' -> '_'
        | c -> c)
      |> String.capitalize
    in
    let on_click =
      Vdom.Attr.on_click (fun _ ->
        let%bind.Effect (_ : unit Or_error.t) =
          Js_clipboard.Asynchronous.copy_text (Js_of_ocaml.Js.string variant_name)
        in
        set_copied)
    in
    let style = Style.icon in
    match copied with
    | `Show_icon ->
      Vdom.(
        Node.button
          ~attrs:[ Vdom.Attr.combine on_click style ]
          [ Codicons.svg icon; Node.p [ Node.text name ] ])
    | `Show_copied ->
      Vdom.(
        Node.div
          ~attrs:[ style ]
          [ Codicons.svg Copy; Node.p [ Node.text [%string "Copied %{variant_name}!"] ] ])
  ;;

  let component icons graph =
    let icons =
      Bonsai.map icons ~f:(String.Map.of_list_with_key_exn ~get_key:Codicons.name)
    in
    let cards = Bonsai.assoc (module String) icons ~f:(fun _ -> icon_card) graph in
    let%arr cards = cards in
    Vdom.Node.div ~attrs:[ Style.grid ] (Map.data cards)
  ;;
end

module Search = struct
  let component () graph =
    let input =
      Form.Elements.Textbox.string
        ~placeholder:(Bonsai.return "Filter icons")
        ~allow_updates_when_focused:`Never
        ()
        graph
    in
    let%arr input = input in
    let search =
      Form.value input
      |> Result.ok
      |> Option.value ~default:""
      |> String.filter ~f:Char.is_alpha
    in
    let filter_icon =
      Codicons.svg (if String.is_empty search then Filter else Filter_filled)
    in
    let filtered_icon_list =
      List.filter Codicons.all ~f:(fun icon ->
        Fuzzy_match.is_match
          (Codicons.name icon)
          ~pattern:search
          ~char_equal:Char.Caseless.equal)
    in
    let view =
      Vdom.Node.div
        ~attrs:[ Style.search ]
        (filter_icon :: (Form.view input |> Form.View.to_vdom_plain))
    in
    filtered_icon_list, view
  ;;
end

let app graph =
  let search = Search.component () graph in
  let icons = Bonsai.map ~f:fst search in
  let grid = Icon_grid.component icons graph in
  let%arr grid = grid
  and _, search = search in
  Vdom.Node.div ~attrs:[ Style.main ] [ search; grid ]
;;

let () = Bonsai_web.Start.start app
