open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module N = Vdom.Node
module A = Vdom.Attr
module Form = Bonsai_web_ui_form
module E = Form.Elements

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

type form_transformer =
  Sexp_grammar.grammar Sexp_grammar.with_tag Value.t
  -> recurse:(Sexp_grammar.grammar Value.t -> Sexp.t Form.t Computation.t)
  -> Sexp.t Form.t Computation.t

let error_hint form =
  let%sub form = form in
  Form.Dynamic.error_hint form
;;

module Customization = struct
  type 'a t =
    { apply_to_tag : key:string -> value:Sexp.t -> bool
    ; on_match : 'a
    }

  let create_for_view ~apply_to_tag on_match = { apply_to_tag; on_match }

  let transform_form'
        (type a)
        (module M : Sexpable with type t = a)
        ~apply_to_tag
        (on_match :
           Sexp_grammar.grammar Sexp_grammar.with_tag Value.t
         -> recurse:(Sexp_grammar.grammar Value.t -> Sexp.t Form.t Computation.t)
         -> a Form.t Computation.t)
    =
    let transform grammar ~recurse =
      let%map.Computation on_match = on_match grammar ~recurse in
      Form.project on_match ~parse_exn:[%sexp_of: M.t] ~unparse:[%of_sexp: M.t]
    in
    { apply_to_tag; on_match = transform }
  ;;

  let transform_form
        ~apply_to_tag
        (on_match :
           Sexp_grammar.grammar Sexp_grammar.with_tag Value.t
         -> recurse:(Sexp_grammar.grammar Value.t -> Sexp.t Form.t Computation.t)
         -> Sexp.t Form.t Computation.t)
    =
    transform_form' (module Sexp) ~apply_to_tag on_match
  ;;

  let constant_form
        (type a)
        (module M : Sexpable with type t = a)
        ~apply_to_tag
        (on_match : a Form.t Computation.t)
    =
    transform_form' (module M) ~apply_to_tag (fun _ ~recurse:_ -> on_match)
  ;;

  let applies t (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag) =
    t.apply_to_tag ~key:with_tag.key ~value:with_tag.value
  ;;

  let apply_if_name_matches t (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag) =
    Option.some_if (applies t with_tag) t.on_match
  ;;

  let apply t (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Value.t) =
    t.on_match with_tag
  ;;

  module Defaults = struct
    module Form = struct
      let transform_key_data_pair
            (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Value.t)
            ~recurse
        =
        let%sub grammar =
          let%arr with_tag = with_tag in
          with_tag.grammar
        in
        match%sub grammar with
        | List (Many (List (Cons (first, Cons (second, Empty))))) ->
          let pair_form =
            let%sub first =
              error_hint
                (let%sub form = recurse first in
                 let%arr form = form in
                 let view = Form.View.set_label (Vdom.Node.text "Key") (Form.view form) in
                 Form.Expert.create ~view ~value:(Form.value form) ~set:(Form.set form))
            in
            let%sub second =
              error_hint
                (let%sub form = recurse second in
                 let%arr form = form in
                 let view =
                   Form.View.set_label (Vdom.Node.text "Data") (Form.view form)
                 in
                 Form.Expert.create ~view ~value:(Form.value form) ~set:(Form.set form))
            in
            let%arr first = first
            and second = second in
            let view = Form.View.tuple [ Form.view first; Form.view second ] in
            let value =
              match Or_error.both (Form.value first) (Form.value second) with
              | Ok (first, second) -> Ok (Sexp.List [ first; second ])
              | Error _ as err -> err
            in
            let set = function
              | Sexp.List [ first_val; second_val ] ->
                Effect.Many [ Form.set first first_val; Form.set second second_val ]
              | _ -> Effect.Ignore
            in
            Form.Expert.create ~view ~value ~set
          in
          Computation.map
            (Form.Elements.Multiple.list pair_form)
            ~f:
              (Form.project
                 ~parse_exn:(fun l -> Sexp.List l)
                 ~unparse:(function
                   | Sexp.List l -> l
                   | Sexp.Atom _ -> []))
        | _ -> recurse grammar
      ;;

      let assoc_key_value_labels =
        transform_form
          ~apply_to_tag:(fun ~key ~value:_ ->
            String.equal key Sexplib0.Sexp_grammar.assoc_tag)
          transform_key_data_pair
      ;;

      let transform_multiple_button_name
            (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Value.t)
            ~(recurse : Sexp_grammar.grammar Value.t -> Sexp.t Form.t Computation.t)
        =
        let%sub { grammar; value; key = _ } = return with_tag in
        let%sub form = recurse grammar in
        let%sub text =
          match%arr value with
          | Sexp.Atom s -> s
          | value -> Sexp.to_string_hum value
        in
        let%arr form = form
        and text = text in
        let view =
          let view = Form.view form in
          match view.view with
          | List ({ append_item = Append_info { append; text = None }; _ } as t) ->
            { view with
              view =
                List { t with append_item = Append_info { append; text = Some text } }
            }
          | List { append_item = Append_view _; _ }
          | List { append_item = Append_info { text = Some _; _ }; _ }
          | Empty | Collapsible _ | Raw _ | Record _ | Variant _ | Tuple _ | Option _ ->
            view
        in
        Form.Expert.create ~value:(Form.value form) ~set:(Form.set form) ~view
      ;;

      let transform_multiple_button_name =
        transform_form
          ~apply_to_tag:(fun ~key ~value:_ -> String.equal key "grammar.add_element_text")
          transform_multiple_button_name
      ;;

      (* Without override, these would all be string text boxes *)
      let nice_time_ns =
        constant_form
          (module Time_ns.Alternate_sexp)
          ~apply_to_tag:(fun ~key ~value ->
            String.equal key Sexplib0.Sexp_grammar.type_name_tag
            && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Alternate_sexp.t"))
          (Form.Elements.Date_time.datetime_local ())
      ;;

      let nice_time_of_day =
        constant_form
          (module Time_ns.Ofday)
          ~apply_to_tag:(fun ~key ~value ->
            String.equal key Sexplib0.Sexp_grammar.type_name_tag
            && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Ofday.t"))
          (Form.Elements.Date_time.time ())
      ;;

      let nice_date =
        constant_form
          (module Date)
          ~apply_to_tag:(fun ~key ~value ->
            String.equal key Sexplib0.Sexp_grammar.type_name_tag
            && Sexp.equal value ([%sexp_of: string] "Core.Date.t"))
          (Form.Elements.Date_time.date ())
      ;;

      let all =
        [ assoc_key_value_labels
        ; nice_time_ns
        ; nice_time_of_day
        ; nice_date
        ; transform_multiple_button_name
        ]
      ;;
    end
  end
end

module Style =
  [%css
    stylesheet
      {|
  .with_whitespace {
    white-space: pre-wrap;
  }

  .record_field_name {
    font-weight: bold;
  }

  .error {
    font-weight: bold;
    border: solid 2px red
  }

  .override_showing {
    color: black;
  }

  .override_hidden {
    color: gray;
  }

  .override_text {
    text-align: center;
    font-weight: bold;
    cursor: pointer;
  }

  pre {
    margin: 0;
  }

  .inline_padding {
    padding-inline: 5px;
  }

  .bold_text {
    font-weight: 700;
  }

  .scrollable_tooltip {
    overflow-y: auto;
    /* arbitrary value from eyeballing */
    max-height: 14rem;
  }
  |}]

let error_box message = N.pre ~attrs:[ Style.error ] [ N.text message ]

let print_the_atom ~monospaced = function
  | Sexp.Atom a ->
    if monospaced
    then N.pre [ N.text a ]
    else N.span ~attrs:[ Style.with_whitespace ] [ N.text a ]
  | sexp ->
    error_box [%string "Error printing atom. Expected atom, but got: %{sexp#Sexp}"]
;;

let view grammar ~customizations =
  let rec view_grammar (grammar : Sexp_grammar.grammar)
    : Sexp.t Value.t -> Vdom.Node.t Computation.t
    =
    match Sexp_grammar.Unroll_recursion.of_grammar_exn grammar with
    | Bool | String -> Bonsai.pure (print_the_atom ~monospaced:false)
    | Integer | Char | Float -> Bonsai.pure (print_the_atom ~monospaced:true)
    | Option g ->
      let view = view_grammar g in
      (function%sub
        | List [] | Atom "None" | Atom "none" -> Bonsai.const (N.pre [ N.text "None" ])
        | List [ a ] | List [ Atom ("Some" | "some"); a ] -> view a
        | other ->
          let%arr other = other in
          error_box
            [%string "Expected a sexp representing an option, but got: %{other#Sexp}"])
    | List l ->
      let view_list = view_list_grammar l in
      let list_kind =
        match l with
        | Fields _ -> N.ul
        | _ -> N.ol
      in
      fun s ->
        (match%map.Computation view_list s with
         | [] -> N.pre [ N.text "[]" ]
         | all -> list_kind (List.map all ~f:(fun a -> N.li [ a ])))
    | Lazy g ->
      fun s ->
        (Bonsai.lazy_ [@alert "-deprecated"]) (Lazy.map g ~f:(fun g -> view_grammar g s))
    | Tagged with_tag -> view_with_tag view_grammar with_tag
    | Variant { case_sensitivity = _; clauses = [] } -> fun _ -> Bonsai.const N.none
    | Variant { case_sensitivity; clauses } -> view_clauses case_sensitivity clauses
    | Union [] -> fun _ -> Bonsai.const N.none
    | Any _ | Union _ -> Bonsai.pure N.sexp_for_debugging
    | Tyvar _ | Tycon _ | Recursive _ -> assert false
  and view_fields ({ fields; allow_extra_fields } : Sexp_grammar.record) sexp =
    let fields_map =
      fields
      |> List.map ~f:Grammar_helper.Tags.strip_tags
      |> List.map ~f:(fun { name; required; args } -> name, (required, args))
      |> String.Map.of_alist_exn
    in
    let original_field_order =
      fields
      |> List.map ~f:Grammar_helper.Tags.strip_tags
      |> List.map ~f:(fun { name; _ } -> name)
    in
    let%sub sexp_map =
      match%arr sexp with
      | Sexp.Atom _ -> String.Map.empty
      | List fields ->
        List.filter_map fields ~f:(function
          | List [ Atom name; value ] -> Some (name, Sexp.List [ value ])
          | _ -> None)
        |> String.Map.of_alist_reduce ~f:(fun _first second ->
          (* there should never be duplicate fields, but if there are, take the last one *)
          second)
    in
    let%sub map =
      Bonsai.assoc
        (module String)
        sexp_map
        ~f:(fun field_name field_value ->
          let%sub key_opt =
            let%arr field_name = field_name in
            if Map.mem fields_map field_name then Some field_name else None
          in
          Bonsai.enum
            (module struct
              type t = string option [@@deriving sexp, compare, equal]

              let all = None :: List.map (Map.keys fields_map) ~f:(fun s -> Some s)
            end)
            ~match_:key_opt
            ~with_:(function
              | None ->
                let%arr field_name = field_name
                and field_value = field_value in
                error_box
                  [%string
                    "Grammar has no field named '%{field_name}', but the sexp does, and \
                     it has value %{field_value#Sexp}"]
              | Some field_name ->
                let _, grammar = Map.find_exn fields_map field_name in
                let%map.Computation all = view_list_grammar grammar field_value in
                let all_view =
                  match all with
                  | [] -> N.none
                  | [ single ] -> single
                  | _ -> all |> List.map ~f:(fun arg -> N.li [ arg ]) |> N.ul
                in
                N.div
                  [ N.span ~attrs:[ Style.record_field_name ] [ N.text field_name ]
                  ; N.br ()
                  ; all_view
                  ]))
    in
    let%sub with_error_messages =
      Bonsai.Map.merge map (Value.return fields_map) ~f:(fun ~key -> function
        | `Left extra_field ->
          (match allow_extra_fields with
           | false ->
             [%string
               "Record has an extra field, named '%{key}', which the grammar does not \
                allow"]
             |> error_box
             |> Some
           | true -> Some extra_field)
        | `Right (required, _) ->
          (match required with
           | true ->
             [%string
               "Record is missing a field named '%{key}', which is required by the \
                grammar"]
             |> error_box
             |> Some
           | false ->
             N.div
               [ N.span ~attrs:[ Style.record_field_name ] [ N.text key ]
               ; N.br ()
               ; N.pre [ N.text "Non-required field not present" ]
               ]
             |> Some)
        | `Both (view, _) -> Some view)
    in
    let%arr with_error_messages = with_error_messages in
    (* We do this [fold_map] because we want to include all of the entries in
       [with_error_messages], while still preserving the order of the original fields. *)
    let extra_fields, original_fields =
      List.fold_map original_field_order ~init:with_error_messages ~f:(fun accum field ->
        Map.remove accum field, Map.find_exn accum field)
    in
    original_fields @ Map.data extra_fields
  and view_clauses
        (case_sensitivity : Sexp_grammar.case_sensitivity)
        (clauses : Sexp_grammar.clause Sexp_grammar.with_tag_list list)
    =
    (* Note that since comparator witnesses have different types, we can't just use this
       case to pick which version of [of_alist_exn] to use. *)
    let normalize_name =
      match case_sensitivity with
      | Case_insensitive -> String.lowercase
      | Case_sensitive -> Fn.id
      | Case_sensitive_except_first_character -> String.capitalize
    in
    let clause_map =
      clauses
      |> List.map ~f:Grammar_helper.Tags.strip_tags
      |> List.map ~f:(fun { name; clause_kind } ->
        normalize_name name, view_clause clause_kind)
      |> String.Map.of_alist_exn
    in
    fun sexp ->
      let%sub tag, remaining =
        match%sub sexp with
        | Sexp.Atom s -> return (Value.both s (Value.return (Sexp.List [])))
        | List (Atom s :: remaining) ->
          let%arr s = s
          and remaining = remaining in
          s, Sexp.List remaining
        | List _ as other ->
          let%arr other = other in
          raise_s [%message "expected variant, found" (other : Sexp.t)]
      in
      let%sub tag_to_match_on =
        let%arr tag = tag >>| normalize_name in
        if Map.mem clause_map tag then Some tag else None
      in
      let%sub args =
        Bonsai.enum
          (module struct
            type t = string option [@@deriving sexp, compare, equal]

            let all = None :: List.map (Map.keys clause_map) ~f:(fun s -> Some s)
          end)
          ~match_:tag_to_match_on
          ~with_:(function
            | None ->
              let%arr tag = tag in
              let valid_tags = Map.keys clause_map |> String.concat ~sep:", " in
              [ error_box
                  [%string
                    "The variant constructor '%{tag}' does not exist. Expected one of: \
                     %{valid_tags} "]
              ]
            | Some k ->
              let c = Map.find_exn clause_map k in
              c remaining)
      in
      let%arr tag = tag
      and args = args in
      match args with
      | [] -> N.div [ N.text tag ]
      | _ -> N.div [ N.text tag; N.ul (List.map ~f:(fun a -> N.li [ a ]) args) ]
  and view_clause (clause : Sexp_grammar.clause_kind)
    : Sexp.t Value.t -> N.t list Computation.t
    =
    match clause with
    | Atom_clause -> fun _ -> Bonsai.const []
    | List_clause { args } -> fun sexp -> view_list_grammar args sexp
  and view_with_tag f with_tag sexp =
    match
      List.find_map customizations ~f:(fun customization ->
        Customization.apply_if_name_matches customization with_tag)
    with
    | Some custom_view -> custom_view sexp
    | None -> f with_tag.grammar sexp
  and view_list_grammar
    : Sexp_grammar.list_grammar -> Sexp.t Value.t -> Vdom.Node.t list Computation.t
    = function
      | Empty -> fun _ -> Bonsai.const []
      | Cons (g, rest) as cons ->
        (function%sub
          | List (a :: xs) ->
            let xs = Value.map xs ~f:(fun xs -> Sexp.List xs) in
            let%map.Computation a = view_grammar g a
            and xs = view_list_grammar rest xs in
            a :: xs
          | List [] ->
            (Bonsai.lazy_ [@alert "-deprecated"])
              (lazy
                (let cons = [%sexp (cons : Sexp_grammar.list_grammar)] in
                 [%string
                   "Encountered empty list sexp when attempting to process the grammar:  \
                    %{cons#Sexp}"]
                 |> error_box
                 |> List.return
                 |> Bonsai.const))
          | sexp ->
            let%arr sexp = sexp in
            [%string
              "Encountered malformed sexp when attempting to view list grammar. Expected \
               non-empty list, but got %{sexp#Sexp}."]
            |> error_box
            |> List.return)
      | Many g as many ->
        (function%sub
          | List xs ->
            let xs = xs >>| List.mapi ~f:(fun i x -> i, x) >>| Int.Map.of_alist_exn in
            let%map.Computation xs =
              Bonsai.assoc (module Int) xs ~f:(fun _ data -> view_grammar g data)
            in
            Map.data xs
          | Atom atom ->
            let many = [%sexp (many : Sexp_grammar.list_grammar)] in
            let%arr atom = atom in
            [%string
              "Encountered an atom, '%{atom}', while processing a list grammar: %{many#Sexp}"]
            |> error_box
            |> List.return)
      | Fields f -> view_fields f
  in
  view_grammar grammar
;;

let view
      (type a)
      (module M : S with type t = a)
      ?(customizations = [])
      (value : a Value.t)
      `This_view_may_change_without_notice
  =
  let%sub sexp = Bonsai.pure M.sexp_of_t value in
  view M.t_sexp_grammar.untyped sexp ~customizations
;;

let project_to_sexp
      (type a)
      (module M : Sexpable with type t = a)
      (form : a Form.t Computation.t)
  =
  let%map.Computation form = form in
  Form.project form ~parse_exn:M.sexp_of_t ~unparse:M.t_of_sexp
;;

let maybe_set_tooltip doc view =
  match doc with
  | Some str -> Form.View.set_tooltip (Vdom.Node.text str) view
  | None -> view
;;

let form
      ?textbox_for_string
      (grammar : Sexp_grammar.grammar Value.t)
      ~on_set_error
      ~customizations
  =
  let rec grammar_form (grammar : Sexp_grammar.grammar Value.t)
    : Sexp.t Form.t Computation.t
    =
    (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (grammar_form_impl grammar))
  and grammar_form_impl (grammar : Sexp_grammar.grammar Value.t)
    : Sexp.t Form.t Computation.t
    =
    match%sub grammar with
    | Bool ->
      E.Checkbox.bool ~default:false () |> project_to_sexp (module Bool) |> error_hint
    | String ->
      (match textbox_for_string with
       | None -> E.Textarea.string () |> project_to_sexp (module String)
       | Some () -> E.Textbox.string () |> project_to_sexp (module String))
      |> error_hint
    | Integer ->
      E.Number.int ~default:0 ~step:1 () |> project_to_sexp (module Int) |> error_hint
    | Char ->
      E.Textbox.stringable (module Char) |> project_to_sexp (module Char) |> error_hint
    | Float ->
      E.Number.float ~default:0. ~step:1. ()
      |> project_to_sexp (module Float)
      |> error_hint
    | Option g -> option_form g
    | List l -> list_grammar_form l
    | Lazy g ->
      (Bonsai.lazy_ [@alert "-deprecated"])
        (lazy
          (let%sub g = Bonsai.pure force g in
           grammar_form_impl g))
    | Tagged with_tag -> with_tag_form with_tag |> error_hint
    | Variant { case_sensitivity = _; clauses = [] } ->
      (* There's no value that a form can produce for a variant type with no clauses. So,
         we just produce a form that errors. *)
      Bonsai.const (Form.return_error (Error.create_s [%message "no clauses in variant"]))
    | Variant { case_sensitivity = _; clauses } -> clauses_form clauses |> error_hint
    | Union [] ->
      Bonsai.const (Form.return_error (Error.create_s [%message "no grammars in union"]))
    | Any _ | Union _ -> E.Textarea.sexpable (module Sexp)
    | Tyvar _ | Tycon _ | Recursive _ ->
      Bonsai.const
        (Form.return_error
           (Error.create_s [%message "unreachable code in Bonsai_web_ui_autogenerated"]))
  and option_form (grammar : Sexp_grammar.grammar Value.t) =
    let%sub form, _ =
      Bonsai.wrap
        ()
        ~sexp_of_model:[%sexp_of: Unit.t]
        ~equal:[%equal: Unit.t]
        ~default_model:()
        ~apply_action:(fun context (_, inner) () sexp ->
          Bonsai.Apply_action_context.schedule_event context (Form.set inner sexp))
        ~f:(fun (_ : unit Value.t) inject_outer ->
          let%sub outer, set_outer =
            Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t]
          in
          let%sub toggle_view =
            let%sub path = Bonsai.path_id in
            let%arr outer = outer
            and set_outer = set_outer
            and path = path in
            E.Checkbox.Private.make_input
              ~id:(Vdom.Attr.id path)
              ~extra_attrs:[]
              ~state:outer
              ~set_state:set_outer
          in
          let%sub inner =
            match%sub outer with
            | false -> Bonsai.const (Form.return (Sexp.List []))
            | true -> grammar_form grammar
          in
          let%sub inner_value = Bonsai.pure Form.value inner in
          let%sub view =
            match%sub outer with
            | false ->
              let%arr toggle_view = toggle_view in
              Form.View.option ~toggle:toggle_view ~status:(Currently_none None)
            | true ->
              let%arr toggle_view = toggle_view
              and inner = inner in
              Form.View.option
                ~toggle:toggle_view
                ~status:(Currently_some (Form.view inner))
          in
          let%sub value =
            match%sub outer with
            | false -> Bonsai.const (Ok (Sexp.List []))
            | true ->
              (match%sub inner_value with
               | Ok inner ->
                 let%arr inner = inner in
                 Ok (Sexp.List [ inner ])
               | Error err ->
                 let%arr err = err in
                 Error err)
          in
          let%sub set =
            let%arr set_outer = set_outer
            and inject_outer = inject_outer in
            function
            | Sexp.List [] | Atom "None" | Atom "none" -> set_outer false
            | List [ a ] | List [ Atom ("Some" | "some"); a ] ->
              Effect.Many [ set_outer true; inject_outer a ]
            | _ as sexp ->
              on_set_error [%message "expected option sexp, but got" (sexp : Sexp.t)]
          in
          let%arr view = view
          and value = value
          and set = set
          and inner = inner in
          Form.Expert.create ~view ~value ~set, inner)
    in
    return form
  and clauses_form (clauses : Sexp_grammar.clause Sexp_grammar.with_tag_list list Value.t)
    =
    let%sub form, _ =
      Bonsai.wrap
        ()
        ~sexp_of_model:[%sexp_of: Unit.t]
        ~equal:[%equal: Unit.t]
        ~default_model:()
        ~apply_action:(fun context (_, inner) () sexp ->
          Bonsai.Apply_action_context.schedule_event context (Form.set inner sexp))
        ~f:(fun (_ : unit Value.t) inject_outer ->
          let%sub clauses_and_docs =
            let%arr clauses = clauses in
            List.map clauses ~f:(fun clause ->
              let clause, tags = Grammar_helper.Tags.collect_and_strip_tags clause in
              clause, Grammar_helper.Tags.find_doc_tag tags)
          in
          let%sub clauses_names, clauses_as_map =
            let%arr clauses_and_docs = clauses_and_docs in
            let as_map =
              String.Map.of_alist_exn
                (List.map clauses_and_docs ~f:(fun ((clause, _doc) as clause_and_tag) ->
                   clause.name, clause_and_tag))
            in
            let just_the_names =
              List.map clauses_and_docs ~f:(fun (clause, _doc) -> clause.name)
            in
            just_the_names, as_map
          in
          let%sub outer, set_outer, outer_view =
            let open E.Dropdown.Private in
            let module Opt = struct
              type t = string Opt.t [@@deriving sexp, equal]

              let to_option = Opt.to_option
            end
            in
            let%sub outer, set_outer =
              Bonsai.state
                Uninitialized
                ~sexp_of_model:[%sexp_of: Opt.t]
                ~equal:[%equal: Opt.t]
            in
            let%sub path = Bonsai.path_id in
            let%sub view =
              let%sub dropdown =
                let%arr path = path
                and outer = outer
                and set_outer = set_outer
                and clause_names = clauses_names in
                make_input
                  ~to_string:(Form.View.sexp_to_pretty_string [%sexp_of: string])
                  (module String)
                  ~equal:[%equal: String.t]
                  ~id:(Vdom.Attr.id path)
                  ~include_empty:true
                  ~default_value:None
                  ~state:outer
                  ~set_state:set_outer
                  ~all:clause_names
                  ~extra_attrs:
                    [ Vdom.Attr.style (Css_gen.width (`Percent (Percent.of_mult 1.))) ]
                  ~extra_option_attrs:(Fn.const [])
              in
              let%sub theme = View.Theme.current in
              let%arr clauses_as_map = clauses_as_map
              and theme = theme
              and dropdown = dropdown in
              let info =
                Map.fold
                  clauses_as_map
                  ~init:[]
                  ~f:(fun ~key:name ~data:(_clause, doc) acc ->
                    match doc with
                    | None -> acc
                    | Some doc ->
                      View.vbox
                        [ Vdom.Node.span ~attrs:[ Style.bold_text ] [ Vdom.Node.text name ]
                        ; Vdom.Node.text doc
                        ]
                      :: acc)
                |> List.rev
              in
              match info with
              | [] -> dropdown
              | info ->
                View.hbox
                  [ dropdown
                  ; View.tooltip'
                      theme
                      ~direction:Right
                      ~tooltip_attrs:[ Style.scrollable_tooltip ]
                      ~container_attrs:[ Style.inline_padding ]
                      ~tooltip:(View.vbox ~gap:(`Rem 0.15) info)
                      (Vdom.Node.text "?")
                  ]
            in
            let%arr outer = outer
            and set_outer = set_outer
            and view = view in
            ( Opt.to_option outer
            , (function
                | None -> set_outer Explicitly_none
                | Some outer -> set_outer (Set outer))
            , view )
          in
          let%sub clauses_forms =
            Bonsai.assoc
              (module String)
              clauses_as_map
              ~f:(fun name clause ->
                let%sub { Sexp_grammar.clause_kind; _ }, _doc = return clause in
                let%sub is_active =
                  let%arr outer = outer
                  and name = name in
                  match outer with
                  | None -> false
                  | Some clause_name when String.equal clause_name name -> true
                  | Some _ -> false
                in
                if%sub is_active
                then (
                  match%sub clause_kind with
                  | Atom_clause -> Bonsai.const (Form.return (Sexp.List []))
                  | List_clause { args : Sexp_grammar.list_grammar } ->
                    list_grammar_form args)
                else Bonsai.const (Form.return (Sexp.List [])))
          in
          let%sub inner =
            let%arr outer = outer
            and clauses_forms = clauses_forms in
            match outer with
            | None -> Form.return (Sexp.List [])
            | Some selected_name ->
              (match Map.find clauses_forms selected_name with
               | Some form -> form
               | None ->
                 {| BUG: auto-generated forms could not find a form with the selected variant name |}
                 |> Error.of_string
                 |> Form.return_error)
          in
          let%sub view =
            match%sub outer with
            | None ->
              let%arr outer_view = outer_view in
              Form.View.variant ~clause_selector:outer_view ~selected_clause:None
            | Some clause_name ->
              let%arr clause_name = clause_name
              and outer_view = outer_view
              and inner = inner in
              Form.View.variant
                ~clause_selector:outer_view
                ~selected_clause:(Some { clause_name; clause_view = Form.view inner })
          in
          let%sub set =
            let%arr clauses_and_docs = clauses_and_docs
            and set_outer = set_outer
            and inject_outer = inject_outer in
            function
            | Sexp.List (Sexp.Atom clause_name :: args) ->
              (match
                 List.find clauses_and_docs ~f:(fun (clause, _docs) ->
                   String.equal clause.name clause_name)
               with
               | Some (clause, _docs) ->
                 Effect.Many
                   [ set_outer (Some clause.name); inject_outer (Sexp.List args) ]
               | None ->
                 on_set_error
                   [%message
                     "unknown clause_name while setting a clause form"
                       (clause_name : string)])
            | Sexp.Atom clause_name ->
              (match
                 List.find clauses_and_docs ~f:(fun (clause, _docs) ->
                   String.equal clause.name clause_name)
               with
               | Some (clause, _docs) -> set_outer (Some clause.name)
               | None ->
                 on_set_error
                   [%message
                     "unknown clause_name while setting a clause form"
                       (clause_name : string)])
            | sexp ->
              on_set_error
                [%message "unexpected format while setting a clause form" (sexp : Sexp.t)]
          in
          let%sub value =
            let%arr outer = outer
            and inner = inner in
            match outer, Form.value inner with
            | Some clause_name, Ok (Sexp.List []) -> Ok (Sexp.Atom clause_name)
            | Some clause_name, Ok (Sexp.List args) ->
              Ok (Sexp.List (Sexp.Atom clause_name :: args))
            | Some _, Ok sexp ->
              Or_error.error_s [%message "invalid sexp encountered" (sexp : Sexp.t)]
            | None, _ -> Or_error.error_s [%message "a value is required"]
            | _, (Error _ as err) -> err
          in
          let%arr view = view
          and value = value
          and set = set
          and inner = inner in
          Form.Expert.create ~view ~value ~set, inner)
    in
    return form
  and list_grammar_form (grammar : Sexp_grammar.list_grammar Value.t) =
    (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (list_grammar_form_impl grammar))
  and list_grammar_form_impl (grammar : Sexp_grammar.list_grammar Value.t) =
    (* Tuples don't have labels, so we annotate their arguments with ordinals. The
       special-case check for a singleton list is because we don't want to add an ordinal
       to variants/fields that take a single argument. *)
    let%sub should_annotate_with_ordinals =
      match%arr grammar with
      | Cons (_, Empty) -> false
      | Cons _ -> true
      | Empty | Many _ | Fields _ -> false
    in
    let rec annotate_with_ordinals (grammar : Sexp_grammar.list_grammar Value.t) ~depth
      : (Sexp.t Or_error.t * (Sexp.t -> unit Effect.t) * Form.View.t list) Computation.t
      =
      match%sub grammar with
      | Empty -> Bonsai.const (Ok (Sexp.List []), (fun _ -> Effect.Ignore), [])
      | Many grammar ->
        let%map.Computation list_form = grammar_form grammar |> E.Multiple.list in
        let view = Form.view list_form in
        let value =
          let%map.Or_error value = Form.value list_form in
          Sexp.List value
        in
        let set = function
          | Sexp.List l -> Form.set list_form l
          | Sexp.Atom _ as atom ->
            on_set_error
              [%message "attempted to set atom into list grammar" (atom : Sexp.t)]
        in
        value, set, [ view ]
      | Fields fields -> fields_grammar_form fields
      | Cons (g, rest) ->
        let%sub g_form = grammar_form g in
        let%sub rest_value, rest_set, rest_views =
          (Bonsai.lazy_ [@alert "-deprecated"])
            (lazy (annotate_with_ordinals rest ~depth:(depth + 1)))
        in
        let%sub g_form =
          match%sub should_annotate_with_ordinals with
          | false -> return g_form
          | true ->
            error_hint
              (let%arr g_form = g_form in
               Form.label (Ordinal_abbreviation.to_string depth) g_form)
        in
        let%sub value =
          let%arr g_form = g_form
          and rest_value = rest_value
          and grammar = grammar in
          let%bind.Or_error g = Form.value g_form
          and rest = rest_value in
          match rest with
          | Sexp.Atom _ as atom ->
            Or_error.error_s
              [%message
                "got unexpected atom for list grammar"
                  (grammar : Sexp_grammar.list_grammar)
                  (atom : Sexp.t)]
          | List l -> Ok (Sexp.List (g :: l))
        in
        let%sub set =
          let%arr g_form = g_form
          and rest_set = rest_set
          and grammar = grammar in
          function
          | Sexp.List (g :: rest) ->
            Effect.Many [ Form.set g_form g; rest_set (Sexp.List rest) ]
          | sexp ->
            on_set_error
              [%message
                "attempted to set atom into list grammar"
                  (grammar : Sexp_grammar.list_grammar)
                  (sexp : Sexp.t)]
        in
        let%sub views =
          let%arr rest_views = rest_views
          and g_form = g_form in
          Form.view g_form :: rest_views
        in
        let%arr value = value
        and views = views
        and set = set in
        value, set, views
    in
    let%map.Computation value, set, views = annotate_with_ordinals grammar ~depth:1 in
    let view =
      match views with
      | [] -> Form.View.empty
      | [ single ] -> single
      | _ -> Form.View.tuple views
    in
    Form.Expert.create ~value ~set ~view
  and optional_field_grammar_form (args : Sexp_grammar.list_grammar Value.t) =
    let%sub form, _ =
      Bonsai.wrap
        ()
        ~sexp_of_model:[%sexp_of: Unit.t]
        ~equal:[%equal: Unit.t]
        ~default_model:()
        ~apply_action:(fun context (_, inner) () inner_value ->
          Bonsai.Apply_action_context.schedule_event context (Form.set inner inner_value))
        ~f:(fun (_ : unit Value.t) inject_outer ->
          (* We can't use toggle here because we need to be able to set the value directly
             as part of [Form.set] *)
          let%sub override, set_override =
            Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t]
          in
          let%sub toggle =
            let%arr override = override
            and set_override = set_override
            and args = args in
            match args with
            | Empty ->
              Vdom_input_widgets.Checkbox.simple
                ~merge_behavior:Legacy_dont_merge
                ~is_checked:override
                ~label:""
                ~on_toggle:(set_override (not override))
                ()
            | _ ->
              let text = if override then "[override]" else "[default]" in
              let override_status =
                if override then Style.override_showing else Style.override_hidden
              in
              N.div
                ~attrs:
                  [ A.(
                      override_status
                      @ Style.override_text
                      @ on_click (fun _ -> set_override (not override)))
                  ]
                [ N.text text ]
          in
          let%sub inner =
            match%sub override with
            | false ->
              Bonsai.const
                (Form.return_error (Error.of_string "unreachable auto-gen code"))
            | true -> list_grammar_form args
          in
          let%arr override = override
          and set_override = set_override
          and toggle = toggle
          and inject_outer = inject_outer
          and inner = inner in
          let view =
            match override with
            | false -> Form.View.option ~toggle ~status:(Currently_none None)
            | true -> Form.View.option ~toggle ~status:(Currently_some (Form.view inner))
          in
          let value =
            match override with
            | false -> Ok None
            | true ->
              (match Form.value inner with
               | Ok s -> Ok (Some s)
               | Error _ as err -> err)
          in
          let set = function
            | None -> set_override false
            | Some s -> Effect.Many [ set_override true; inject_outer s ]
          in
          Form.Expert.create ~view ~value ~set, inner)
    in
    return form
  and fields_grammar_form (fields : Sexp_grammar.record Value.t) =
    let%sub { fields; allow_extra_fields } = return fields in
    let%sub original_field_order =
      let%arr fields = fields in
      fields
      |> List.map ~f:Grammar_helper.Tags.strip_tags
      |> List.map ~f:(fun { name; _ } -> name)
    in
    let%sub forms =
      let%sub fields_by_name =
        let%arr fields = fields in
        List.map fields ~f:(fun (field : Sexp_grammar.field Sexp_grammar.with_tag_list) ->
          let field, tags = Grammar_helper.Tags.collect_and_strip_tags field in
          let doc = Grammar_helper.Tags.find_doc_tag tags in
          field.name, (field, doc))
        |> String.Map.of_alist_exn
      in
      Bonsai.assoc
        (module String)
        fields_by_name
        ~f:(fun _ field_and_doc ->
          (* [args_form] is a [Sexp.t option Form.t] because we need the ability to include
             or omit the sexp produced by that part of the form depending on if we want to
             use the default value. [None] indicates that we should just use the default
             value. *)
          let%sub field, doc = return field_and_doc in
          let%sub required =
            let%arr field = field in
            field.required
          in
          let%sub args =
            let%arr field = field in
            field.args
          in
          let%sub args_form =
            match%sub required with
            | true ->
              let%map.Computation form = list_grammar_form args in
              Form.project
                form
                ~parse_exn:(fun s -> Some s)
                ~unparse:(fun s -> Option.value_exn ~here:[%here] s)
            | false -> optional_field_grammar_form args
          in
          let%arr args_form = args_form
          and required = required
          and doc = doc in
          args_form, `Required required, `Doc doc)
    in
    let%sub view =
      let%arr forms = forms
      and original_field_order = original_field_order in
      List.map original_field_order ~f:(fun field_name ->
        let form, `Required _, `Doc doc = Map.find_exn forms field_name in
        { Form.View.field_view = form |> Form.view |> maybe_set_tooltip doc; field_name })
      |> Form.View.record
    in
    let%sub set =
      let%arr forms = forms
      and allow_extra_fields = allow_extra_fields in
      fun sexp ->
        let sexp_map =
          match sexp with
          | Sexp.Atom _ -> String.Map.empty
          | List fields ->
            List.filter_map fields ~f:(function
              | List [ Atom name; value ] -> Some (name, Sexp.List [ value ])
              | _ -> None)
            |> String.Map.of_alist_exn
        in
        Map.merge sexp_map forms ~f:(fun ~key -> function
          | `Left args ->
            (match allow_extra_fields with
             | true -> None
             | false ->
               Some
                 (on_set_error
                    [%message
                      "extra fields are not allowed, but got an extra field"
                        (key : string)
                        (args : Sexp.t)]))
          | `Right (form, `Required false, `Doc _) -> Some (Form.set form None)
          | `Right (_, `Required true, `Doc _) ->
            Some (on_set_error [%message "required field is not present" (key : string)])
          | `Both (args, (form, `Required _, `Doc _)) -> Some (Form.set form (Some args)))
        |> Map.data
        |> Effect.Many
    in
    let%sub value =
      let%sub values =
        Bonsai.Map.filter_mapi forms ~f:(fun ~key ~data:(form, `Required _, `Doc _) ->
          match Form.value form with
          | Error _ as err -> Some err
          | Ok None -> None
          | Ok (Some (Sexp.Atom _ as atom)) ->
            Some
              (Or_error.error_s
                 [%message "expected list of args from subform, but got" (atom : Sexp.t)])
          | Ok (Some (Sexp.List value)) -> Some (Ok (Sexp.List (Sexp.Atom key :: value))))
      in
      let%arr values = values in
      Map.data values |> Or_error.all |> Or_error.map ~f:(fun list -> Sexp.List list)
    in
    let%arr value = value
    and view = view
    and set = set in
    value, set, [ view ]
  and with_tag_form (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Value.t) =
    let%sub customization_to_use =
      let%arr with_tag = with_tag in
      List.find_mapi customizations ~f:(fun i customization ->
        match Customization.apply_if_name_matches customization with_tag with
        | None -> None
        | Some _ -> Some i)
    in
    Bonsai.enum
      (module struct
        type t = int option [@@deriving sexp, equal, compare]

        let all = None :: List.init (List.length customizations) ~f:(fun i -> Some i)
      end)
      ~match_:customization_to_use
      ~with_:(function
        | None ->
          let%sub grammar =
            let%arr with_tag = with_tag in
            with_tag.grammar
          in
          grammar_form grammar
        | Some index ->
          let customization = List.nth_exn customizations index in
          Customization.apply customization with_tag ~recurse:grammar_form)
  in
  grammar_form grammar
;;

let form'
      ?(on_set_error = Effect.print_s)
      ?(customizations = Customization.Defaults.Form.all)
      ?textbox_for_string
      sexp_grammar
  =
  let%sub sexp_grammar =
    Bonsai.pure Sexp_grammar.Unroll_recursion.of_grammar_exn sexp_grammar
  in
  let%sub form = form ?textbox_for_string sexp_grammar ~on_set_error ~customizations in
  let%arr form = form
  and sexp_grammar = sexp_grammar in
  let validate_sexp = Sexp_grammar.validate_sexp_untyped sexp_grammar in
  Form.Expert.create ~view:(Form.view form) ~value:(Form.value form) ~set:(fun sexp ->
    match unstage validate_sexp sexp with
    | Ok () -> Form.set form sexp
    | Error error ->
      on_set_error
        [%message
          "BUG: Sexp representation of set form value does not match sexp grammar. Does \
           your sexp_of_t function match your sexp grammar?"
            ~value:(sexp : Sexp.t)
            (error : Error.t)])
;;

let form
      (type a)
      (module M : S with type t = a)
      ?on_set_error
      ?customizations
      ?textbox_for_string
      ()
  : a Form.t Computation.t
  =
  let%map.Computation form =
    form'
      ?on_set_error
      ?customizations
      ?textbox_for_string
      (Value.return M.t_sexp_grammar.untyped)
  in
  Form.project form ~parse_exn:M.t_of_sexp ~unparse:M.sexp_of_t
;;

let view_as_vdom ?on_submit ?editable form =
  let on_submit =
    Option.map
      on_submit
      ~f:
        (fun
          { Form.Submit.f; handle_enter; button_text; button_attr; button_location } ->
          let on_submit = Form.value form |> Result.ok |> Option.map ~f in
          { Form.View.on_submit; handle_enter; button_text; button_attr; button_location })
  in
  Render_form.to_vdom ?on_submit ?editable (Form.view form)
;;
