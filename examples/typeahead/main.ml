open! Core
open! Bonsai_web

module Pokemon = struct
  module T = struct
    type t =
      | Bulbasaur
      | Ivysaur
      | Venusaur
      | Charmander
      | Charmeleon
      | Charizard
      | Squirtle
      | Wartortle
      | Blastoise
      | Caterpie
      | Magikarp
      | Metapod
      | Butterfree
      | Weedle
      | Kakuna
      | Beedrill
      | Pidgey
      | Pidgeotto
    [@@deriving compare, enumerate, equal, variants, sexp]
  end

  include T
  include Comparable.Make (T)

  let to_string t = sexp_of_t t |> Sexp.to_string
end

(* thanks to the good folks in webdev-public, you can no longer fool this into letting you
   choose a pokemon as your favourite if its already your not favourite! *)

let components =
  let open! Bonsai.Let_syntax in
  let open! Bonsai_web_ui_typeahead in
  let%sub all_options =
    Bonsai.state
      [%here]
      (module struct
        type t = Pokemon.t list [@@deriving equal, compare, sexp]
      end)
      ~default_model:Pokemon.all
  in
  let%sub favourite_pokemon, typeahead_single_vdom, _set =
    Typeahead.create
      ~on_select_change:
        (let%map _, inject_all_options = all_options in
         fun favourite_pokemon ->
           Option.value_map
             favourite_pokemon
             ~default:Pokemon.all
             ~f:(fun favourite_pokemon ->
               List.filter Pokemon.all ~f:(fun pokemon ->
                 not (Pokemon.equal favourite_pokemon pokemon)))
           |> inject_all_options)
      ~to_string:(Bonsai.Value.return Pokemon.to_string)
      ~placeholder:"Select a pokemon"
      ~all_options:(Value.return Pokemon.all)
      (module Pokemon)
  in
  let%sub (_ : Pokemon.Set.t), typeahead_multi_vdom, _ =
    Typeahead.create_multi
      (module Pokemon)
      ~to_string:Pokemon.to_string
      ~on_set_change:
        (let%map inject_all_options = all_options >>| snd
         and favourite_pokemon = favourite_pokemon in
         fun not_good_pokemon ->
           (* Remove the favourite pokemon if it exists. *)
           let all_pokemon =
             let all = Pokemon.all |> Pokemon.Set.of_list in
             Option.value_map favourite_pokemon ~default:all ~f:(fun favourite_pokemon ->
               Set.remove all favourite_pokemon)
           in
           Set.diff all_pokemon not_good_pokemon |> Set.to_list |> inject_all_options)
      ~placeholder:"Select many pokemon"
      ~all_options:(all_options >>| fst)
  in
  return
  @@ let%map typeahead_single_vdom = typeahead_single_vdom
  and typeahead_multi_vdom = typeahead_multi_vdom in
  Vdom.Node.create
    "main"
    [ Vdom.Node.section
        [ Vdom.Node.h4
            ~attr:(Vdom.Attr.style (Css_gen.margin ~top:(`Px 0) ~bottom:(`Px 0) ()))
            [ Vdom.Node.text "Typeaheads" ]
        ; Vdom.Node.p
            ~attr:(Vdom.Attr.style (Css_gen.margin ~bottom:(`Px 0) ()))
            [ Vdom.Node.span
                [ Vdom.Node.text "For the best user experience, we recommend " ]
            ; Vdom.Node.create "s" [ Vdom.Node.text "Internet Explorer 6" ]
            ; Vdom.Node.span [ Vdom.Node.text " jane-web-style 6+" ]
            ]
        ]
    ; Vdom.Node.section
        [ Vdom.Node.label
            [ Vdom.Node.text "What's your favourite pokemon?"; typeahead_single_vdom ]
        ]
    ; Vdom.Node.section
        [ Vdom.Node.label
            [ Vdom.Node.text "Which pokemon do you like less?"; typeahead_multi_vdom ]
        ]
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" components
;;
