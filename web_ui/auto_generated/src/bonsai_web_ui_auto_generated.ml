open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module N = Vdom.Node
module A = Vdom.Attr
module Form = Bonsai_web_ui_form.With_automatic_view
module Form2 = Bonsai_web_ui_form.With_manual_view
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

  let apply t = t.on_match

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
                (* The order of these sets is important and enables optimization! *)
                Effect.Many [ Form.set second second_val; Form.set first first_val ]
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
          (Form.Elements.Date_time.datetime_local ~allow_updates_when_focused:`Never ())
      ;;

      let nice_time_of_day =
        constant_form
          (module Time_ns.Ofday)
          ~apply_to_tag:(fun ~key ~value ->
            String.equal key Sexplib0.Sexp_grammar.type_name_tag
            && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Ofday.t"))
          (Form.Elements.Date_time.time ~allow_updates_when_focused:`Never ())
      ;;

      let nice_date =
        constant_form
          (module Date)
          ~apply_to_tag:(fun ~key ~value ->
            String.equal key Sexplib0.Sexp_grammar.type_name_tag
            && Sexp.equal value ([%sexp_of: string] "Core.Date.t"))
          (Form.Elements.Date_time.date ~allow_updates_when_focused:`Never ())
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
  ~allow_duplication_of_list_items
  =
  let with_tag_form
    ~grammar_form
    (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Value.t)
    =
    let%sub customization_to_use =
      let%arr with_tag = with_tag in
      List.find_mapi customizations ~f:(fun i customization ->
        match Customization.applies customization with_tag with
        | false -> None
        | true -> Some i)
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
          grammar_form grammar |> error_hint
        | Some index ->
          let customization = List.nth_exn customizations index in
          Customization.apply customization with_tag ~recurse:grammar_form)
  in
  let option_form ~grammar_form (grammar : Sexp_grammar.grammar Value.t) =
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
              ~key:path
              ~id:(Vdom.Attr.id path)
              ~extra_attrs:[]
              ~state:outer
              ~set_state:set_outer
              ()
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
            let%sub bonk = Bonsai_extra.bonk in
            let%arr set_outer = set_outer
            and inject_outer = inject_outer
            and outer = outer
            and bonk = bonk in
            function
            | Sexp.List [] | Atom "None" | Atom "none" -> set_outer false
            | List [ a ] | List [ Atom ("Some" | "some"); a ] ->
              (match outer with
               | true -> bonk (inject_outer a)
               | false -> Effect.Many [ set_outer true; bonk (inject_outer a) ])
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
  in
  let fields_grammar_form
    ~list_grammar_form
    ~optional_field_grammar_form
    (fields : Sexp_grammar.record Value.t)
    =
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
        (* This reversal of order is important and enables optimization! *)
        |> List.rev
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
  in
  let optional_field_grammar_form
    ~list_grammar_form
    (args : Sexp_grammar.list_grammar Value.t)
    =
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
          let%sub bonk = Bonsai_extra.bonk in
          let%arr override = override
          and set_override = set_override
          and toggle = toggle
          and bonk = bonk
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
            | Some s -> Effect.Many [ set_override true; bonk (inject_outer s) ]
          in
          Form.Expert.create ~view ~value ~set, inner)
    in
    return form
  in
  let list_form_with_duplication ~grammar_form (grammar : Sexp_grammar.grammar Value.t) =
    let%sub list_form =
      let form = grammar_form grammar in
      Form2.Elements.Multiple.list form
    in
    let%sub duplicate =
      let%arr list_form = list_form in
      fun idx ->
        match Form2.value list_form with
        | Error _ -> Effect.Ignore
        | Ok value ->
          Form2.set
            list_form
            (List.concat_mapi value ~f:(fun i value ->
               if i = idx then [ value; value ] else [ value ]))
    in
    let%sub view =
      let render_button ~theme ~enabled ~on_click text =
        let color =
          match enabled with
          | true -> Css_gen.color (`Name "blue")
          | false -> Css_gen.color (`Name "gray")
        in
        View.button
          theme
          ~attrs:[ Vdom.Attr.type_ "button"; Vdom.Attr.style color ]
          ~disabled:(not enabled)
          ~on_click
          text
      in
      let%sub theme = View.Theme.current in
      let%arr list_form = list_form
      and duplicate = duplicate
      and theme = theme in
      let ({ items; add_element } : _ Form2.Elements.Multiple.t) = Form2.view list_form in
      let items =
        List.mapi items ~f:(fun i { form; remove } ->
          let remove_view =
            View.hbox
              ~gap:(`Em_float 0.5)
              [ View.textf "%d - " i
              ; render_button ~theme ~enabled:true ~on_click:remove "[ remove ]"
              ; render_button
                  ~theme
                  ~enabled:(Or_error.is_ok (Form2.value list_form))
                  ~on_click:(duplicate i)
                  "[ duplicate ]"
              ]
          in
          Form.View.list_item
            ~view:(Form.view form)
            ~remove_item:(Remove_view remove_view))
      in
      Form.View.list
        ~append_item:(Append_info { append = add_element; text = None })
        ~legacy_button_position:`Indented
        items
    in
    let%sub value =
      let%arr list_form = list_form in
      let%map.Or_error value = Form2.value list_form in
      Sexp.List value
    in
    let%sub set =
      let%arr list_form = list_form in
      function
      | Sexp.List l -> Form2.set list_form l
      | Sexp.Atom _ as atom ->
        on_set_error [%message "attempted to set atom into list grammar" (atom : Sexp.t)]
    in
    let%arr value = value
    and set = set
    and view = view in
    value, set, [ view ]
  in
  let list_form_without_duplication ~grammar_form (grammar : Sexp_grammar.grammar Value.t)
    =
    let%map.Computation list_form = grammar_form grammar |> E.Multiple.list in
    let view = Form.view list_form in
    let value =
      let%map.Or_error value = Form.value list_form in
      Sexp.List value
    in
    let set = function
      | Sexp.List l -> Form.set list_form l
      | Sexp.Atom _ as atom ->
        on_set_error [%message "attempted to set atom into list grammar" (atom : Sexp.t)]
    in
    value, set, [ view ]
  in
  let rec annotate_with_ordinals
    ~grammar_form
    ~fields_grammar_form
    (grammar : Sexp_grammar.list_grammar Value.t)
    ~(depth : int Value.t)
    ~should_annotate_with_ordinals
    : (Sexp.t Or_error.t * (Sexp.t -> unit Effect.t) * Form.View.t list) Computation.t
    =
    let annotate_with_ordinals =
      annotate_with_ordinals
        ~grammar_form
        ~fields_grammar_form
        ~should_annotate_with_ordinals
    in
    let list_form_without_duplication = list_form_without_duplication ~grammar_form in
    let list_form_with_duplication = list_form_with_duplication ~grammar_form in
    match%sub grammar with
    | Empty -> Bonsai.const (Ok (Sexp.List []), (fun _ -> Effect.Ignore), [])
    | Many grammar ->
      (match allow_duplication_of_list_items with
       | false -> list_form_without_duplication grammar
       | true -> list_form_with_duplication grammar)
    | Fields fields -> fields_grammar_form fields
    | Cons (g, rest) ->
      let%sub g_form = grammar_form g in
      let%sub rest_value, rest_set, rest_views =
        (Bonsai.lazy_ [@alert "-deprecated"])
          (lazy
            (let%sub depth =
               let%arr depth = depth in
               depth + 1
             in
             annotate_with_ordinals rest ~depth))
      in
      let%sub g_form =
        match%sub should_annotate_with_ordinals with
        | false -> return g_form
        | true ->
          error_hint
            (let%arr g_form = g_form
             and depth = depth in
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
          (* The order of these sets is important and enables optimization! *)
          Effect.Many [ rest_set (Sexp.List rest); Form.set g_form g ]
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
  let fields_grammar_form ~list_grammar_form =
    fields_grammar_form
      ~list_grammar_form
      ~optional_field_grammar_form:(optional_field_grammar_form ~list_grammar_form)
  in
  let rec list_grammar_form ~grammar_form (grammar : Sexp_grammar.list_grammar Value.t) =
    let list_grammar_form = list_grammar_form ~grammar_form in
    let fields_grammar_form = fields_grammar_form ~list_grammar_form in
    (Bonsai.lazy_ [@alert "-deprecated"])
      (lazy
        ((* Tuples don't have labels, so we annotate their arguments with ordinals. The
            special-case check for a singleton list is because we don't want to add an ordinal
            to variants/fields that take a single argument. *)
         let%sub should_annotate_with_ordinals =
           match%arr grammar with
           | Cons (_, Empty) -> false
           | Cons _ -> true
           | Empty | Many _ | Fields _ -> false
         in
         let%map.Computation value, set, views =
           annotate_with_ordinals
             ~grammar_form
             ~fields_grammar_form
             grammar
             ~depth:(Value.return 1)
             ~should_annotate_with_ordinals
         in
         let view =
           match views with
           | [] -> Form.View.empty
           | [ single ] -> single
           | _ -> Form.View.tuple views
         in
         Form.Expert.create ~value ~set ~view))
  in
  let constant_clauses_form
    (clauses : Sexp_grammar.clause Sexp_grammar.with_tag_list list Value.t)
    =
    let%sub clauses =
      let%arr clauses = clauses in
      List.map clauses ~f:(fun clause -> (Grammar_helper.Tags.strip_tags clause).name)
    in
    let%sub form =
      Form.Elements.Dropdown.list
        (module String)
        ~to_string:Fn.id
        ~equal:String.equal
        clauses
    in
    let%arr form = form in
    Form.project
      form
      ~parse_exn:(fun name -> Sexp.Atom name)
      ~unparse:(function
        | Sexp.Atom name -> name
        | List _ ->
          raise_s
            [%message "BUG: invalid non-atom constructor set into constant clause form"])
  in
  let clauses_form
    ~grammar_form
    (clauses : Sexp_grammar.clause Sexp_grammar.with_tag_list list Value.t)
    =
    let list_grammar_form = list_grammar_form ~grammar_form in
    let%sub is_constant_clauses =
      let open Grammar_helper in
      let%arr clauses = clauses in
      List.for_all clauses ~f:(fun clause ->
        let clause, tags = Tags.collect_and_strip_tags clause in
        match Tags.is_empty tags, clause.clause_kind with
        | true, Atom_clause -> true
        | false, Atom_clause | false, List_clause _ | true, List_clause _ -> false)
    in
    match%sub is_constant_clauses with
    | true -> constant_clauses_form clauses
    | false ->
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
                        [ Vdom.Node.span
                            ~attrs:[ Style.bold_text ]
                            [ Vdom.Node.text name ]
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
              let%sub bonk = Bonsai_extra.bonk in
              let%arr clauses_and_docs = clauses_and_docs
              and set_outer = set_outer
              and inject_outer = inject_outer
              and bonk = bonk
              and outer = outer in
              function
              | Sexp.List (Sexp.Atom clause_name :: args) ->
                (match
                   List.find clauses_and_docs ~f:(fun (clause, _docs) ->
                     String.equal clause.name clause_name)
                 with
                 | Some (clause, _docs) ->
                   (match outer with
                    | Some name when String.equal name clause.name ->
                      bonk (inject_outer (Sexp.List args))
                    | Some _ | None ->
                      Effect.Many
                        [ set_outer (Some clause.name)
                        ; bonk (inject_outer (Sexp.List args))
                        ])
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
                  [%message
                    "unexpected format while setting a clause form" (sexp : Sexp.t)]
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
  in
  let rec grammar_form (grammar : Sexp_grammar.grammar Value.t)
    : Sexp.t Form.t Computation.t
    =
    (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (grammar_form_impl grammar))
  and grammar_form_impl (grammar : Sexp_grammar.grammar Value.t)
    : Sexp.t Form.t Computation.t
    =
    let list_grammar_form = list_grammar_form ~grammar_form in
    let option_form = option_form ~grammar_form in
    let with_tag_form = with_tag_form ~grammar_form in
    let clauses_form = clauses_form ~grammar_form in
    match%sub grammar with
    | Bool ->
      E.Checkbox.bool ~default:false () |> project_to_sexp (module Bool) |> error_hint
    | String ->
      (match textbox_for_string with
       | None ->
         E.Textarea.string ~allow_updates_when_focused:`Never ()
         |> project_to_sexp (module String)
       | Some () ->
         E.Textbox.string ~allow_updates_when_focused:`Never ()
         |> project_to_sexp (module String))
      |> error_hint
    | Integer ->
      E.Number.int ~default:0 ~step:1 ~allow_updates_when_focused:`Never ()
      |> project_to_sexp (module Int)
      |> error_hint
    | Char ->
      E.Textbox.stringable ~allow_updates_when_focused:`Never (module Char)
      |> project_to_sexp (module Char)
      |> error_hint
    | Float ->
      E.Number.float ~default:0. ~step:1. ~allow_updates_when_focused:`Never ()
      |> project_to_sexp (module Float)
      |> error_hint
    | Option g -> option_form g
    | List l -> list_grammar_form l
    | Lazy g ->
      (Bonsai.lazy_ [@alert "-deprecated"])
        (lazy
          (let%sub g = Bonsai.pure force g in
           grammar_form_impl g))
    | Tagged with_tag -> with_tag_form with_tag
    | Variant { case_sensitivity = _; clauses = [] } ->
      (* There's no value that a form can produce for a variant type with no clauses. So,
         we just produce a form that errors. *)
      Bonsai.const (Form.return_error (Error.create_s [%message "no clauses in variant"]))
    | Variant { case_sensitivity = _; clauses } -> clauses_form clauses |> error_hint
    | Union [] ->
      Bonsai.const (Form.return_error (Error.create_s [%message "no grammars in union"]))
    (* This is a special form of union that's pretty easy to construct and used widely
       in [Css_gen], which are often inputs into Bonsai components. Special casing this
       case to have better support for those, but the general case below should still
       probably be thought about. *)
    | Union
        [ Variant { case_sensitivity = sens_a; clauses = clauses_a }
        ; Variant { case_sensitivity = sens_b; clauses = clauses_b }
        ] ->
      let open Sexp_grammar in
      let%sub merged_variant =
        let%arr sens_a = sens_a
        and sens_b = sens_b
        and clauses_a = clauses_a
        and clauses_b = clauses_b in
        let strictest_case_sensitivity =
          match sens_a, sens_b with
          | Case_sensitive, _ | _, Case_sensitive -> Case_sensitive
          | Case_sensitive_except_first_character, _
          | _, Case_sensitive_except_first_character ->
            Case_sensitive_except_first_character
          | Case_insensitive, Case_insensitive -> Case_insensitive
        in
        Variant
          { case_sensitivity = strictest_case_sensitivity
          ; clauses =
              List.merge
                clauses_a
                clauses_b
                ~compare:(compare_with_tag_list compare_clause)
          }
      in
      grammar_form merged_variant
    | Any _ | Union _ ->
      E.Textarea.sexpable ~allow_updates_when_focused:`Never (module Sexp)
    | Tyvar _ | Tycon _ | Recursive _ ->
      Bonsai.const
        (Form.return_error
           (Error.create_s [%message "unreachable code in Bonsai_web_ui_autogenerated"]))
  in
  grammar_form grammar
;;

let form'
  ?(on_set_error = Effect.print_s)
  ?(customizations = Customization.Defaults.Form.all)
  ?textbox_for_string
  ?(allow_duplication_of_list_items = true)
  sexp_grammar
  =
  let%sub sexp_grammar =
    Bonsai.pure Sexp_grammar.Unroll_recursion.of_grammar_exn sexp_grammar
  in
  let%sub form =
    form
      ?textbox_for_string
      sexp_grammar
      ~on_set_error
      ~customizations
      ~allow_duplication_of_list_items
  in
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
  ?allow_duplication_of_list_items
  ()
  : a Form.t Computation.t
  =
  let%map.Computation form =
    form'
      ?on_set_error
      ?customizations
      ?textbox_for_string
      ?allow_duplication_of_list_items
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
