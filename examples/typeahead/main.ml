open! Core
open! Bonsai_web

module Pokemon = struct
  module T = struct
    let all_of_string = []

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
      | Other of string
    [@@deriving compare, enumerate, equal, variants, sexp]
  end

  include T
  include Comparable.Make (T)

  let to_string t = sexp_of_t t |> Sexp.to_string

  let of_string s =
    try Sexp.of_string s |> t_of_sexp with
    | _ -> Other s
  ;;
end

(* thanks to the good folks in webdev-public, you can no longer fool this into letting you
   choose a pokemon as your favourite if its already your not favourite! *)

let components =
  let open! Bonsai.Let_syntax in
  let open! Bonsai_web_ui_typeahead in
  let%sub all_options =
    Bonsai.state
      Pokemon.all
      ~sexp_of_model:[%sexp_of: Pokemon.t list]
      ~equal:[%equal: Pokemon.t list]
  in
  let typeahead_single ?handle_unknown_option () =
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
      ?handle_unknown_option
      ~all_options:(Value.return Pokemon.all)
      (module Pokemon)
      ~equal:[%equal: Pokemon.t]
  in
  let%sub { selected = favourite_pokemon; view = typeahead_single_vdom; _ } =
    typeahead_single ()
  in
  let%sub { view = typeahead_multi_vdom; _ } =
    Typeahead.create_multi
      (module Pokemon)
      ~to_string:(Value.return Pokemon.to_string)
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
  let%sub { view = typeahead_single_with_custom_input_vdom; _ } =
    typeahead_single
      ~handle_unknown_option:
        (Value.return (fun input ->
           Option.some_if (Int.equal 5 (String.length input)) (Pokemon.of_string input)))
      ()
  in
  let typeahead_multi_with_custom_input ~all_options =
    Typeahead.create_multi
      ~to_string:(Value.return Pokemon.to_string)
      ~placeholder:"Select many pokemon"
      ~handle_unknown_option:
        (Value.return (fun input ->
           (* custom [handle_unknown_option] that does a check on unknown inputs *)
           Option.some_if (String.contains ~pos:0 input 'B') (Pokemon.of_string input)))
      ~all_options
      (module Pokemon)
  in
  let%sub { view = typeahead_multi_with_empty_options_vdom; _ } =
    typeahead_multi_with_custom_input ~all_options:(Value.return [])
  in
  let%sub { view = typeahead_multi_with_custom_input_vdom; _ } =
    typeahead_multi_with_custom_input ~all_options:(Value.return Pokemon.all)
  in
  let%arr typeahead_single_vdom = typeahead_single_vdom
  and typeahead_multi_vdom = typeahead_multi_vdom
  and typeahead_single_with_custom_input_vdom = typeahead_single_with_custom_input_vdom
  and typeahead_multi_with_empty_options_vdom = typeahead_multi_with_empty_options_vdom
  and typeahead_multi_with_custom_input_vdom = typeahead_multi_with_custom_input_vdom in
  Vdom.Node.create
    "main"
    [ Vdom.Node.section
        [ Vdom.Node.h4
            ~attrs:[ Vdom.Attr.style (Css_gen.margin ~top:(`Px 0) ~bottom:(`Px 0) ()) ]
            [ Vdom.Node.text "Typeaheads" ]
        ; Vdom.Node.p
            ~attrs:[ Vdom.Attr.style (Css_gen.margin ~bottom:(`Px 0) ()) ]
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
            [ Vdom.Node.text
                "Which pokemon do you like less? You can't choose your favourite! "
            ; typeahead_multi_vdom
            ]
        ]
    ; Vdom.Node.section
        [ Vdom.Node.label
            [ Vdom.Node.text
                "What's the pokemon with the best name? If you don't like any of these, \
                 create your own! Must have five letters."
            ; typeahead_single_with_custom_input_vdom
            ]
        ]
    ; Vdom.Node.section
        [ Vdom.Node.label
            [ Vdom.Node.text
                "What's your favorite pokemon that starts with a \"B\"? This form has no \
                 suggestions, so be creative."
            ; typeahead_multi_with_empty_options_vdom
            ]
        ]
    ; Vdom.Node.section
        [ Vdom.Node.label
            [ Vdom.Node.text
                "What are all the pokemon you like? Choose from this list or input a \
                 custom value that starts with a \"B\"."
            ; typeahead_multi_with_custom_input_vdom
            ]
        ]
    ]
;;

let () = Bonsai_web.Start.start components
