open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Url_var = Bonsai_web_ui_url_var
module Form = Bonsai_web_ui_form
module Projection = Url_var.For_testing.Projection
module Parse_result = Url_var.For_testing.Parse_result
module Typed = Url_var.Typed
module Parser = Typed.Parser
module Value_parser = Typed.Value_parser
module Versioned_parser = Typed.Versioned_parser
module Path_order = Typed.Parser.Record.Path_order

type 'a t =
  { starting_components : Url_var.Components.t
  ; parser : 'a Projection.t
  ; type_ : (module Sexpable with type t = 'a)
  ; fallback : 'a
  ; title : string
  ; description : string
  ; demo_code : string
  ; example_urls : (string * 'a) list
  }

type packed = T : 'a t -> packed

module Css =
  [%css
    stylesheet
      {|
.paper {
  box-shadow: 0 0 8px rgba(0,0,0,0.2);
  padding: 12px;
  border-radius: 4px;
  margin: 8px;
}


.columns_wrapper {
  align-items: center;
}

.button_row {
  align-items: center;
  max-width: fit-content;
  min-width: -webkit-fill-available;
}

.column {
  text-align: center;
  vertical-align: top;
}

h3 {
  font-weight: normal;
  margin: 8px;
}

* {
  font-family: 'Open Sans', 'Noto Color Emoji', sans-serif;
  box-sizing: border-box;
}

pre {
  font-family: 'Courier New', monospace;
  background-color: #FCFCFC;
  border: 1px solid #E7E7E7;
  text-align: left;
  margin: 4px;
  padding: 4px;
  max-height: 50vh;
  overflow-y: auto;
}


button {
  display: inline-block;
  outline: 0;
  text-align: center;
  cursor: pointer;
  padding: 0px 16px;
  border-radius: 4px;
  min-width: 80px;
  height: 32px;
  background-color: rgb(32, 133, 399);
  color: rgb(255, 255, 255);
  box-sizing: border-box;
  border: 1px solid rgb(0, 120, 212);
  margin: 4px;

}

button:hover {
  background-color: rgb(16, 110, 190);
  border: 1px solid rgb(16, 110, 190);
}

.uri_input {
  width: 100%;
}

.sexp_input {
  width: 100%;
}

.example_description {
  max-width: fit-content;
}

.warning_section {
  align-items: center;
  display: flex;
  flex-direction: row;
  flex-wrap: nowrap;
  align-content: center;
  justify-content: center;
}
|}]

(* This form is the one that reads/write the URI. *)
let uri_form ~default =
  let%sub form =
    let%sub form =
      Form.Elements.Textbox.string ~extra_attrs:(Value.return [ Css.uri_input ]) ()
    in
    let%arr form = form in
    let uri_form =
      Form.project form ~parse_exn:Uri.of_string ~unparse:(fun x ->
        Uri.to_string (Uri.with_host x (Some "my-app.com")))
    in
    Form.project
      uri_form
      ~parse_exn:Url_var.Components.of_uri
      ~unparse:Url_var.Components.to_path_and_query
  in
  Form.Dynamic.with_default default form
;;

(* This form is the one that reads/writes the parsed sexp. *)
let typed_url_form
      (type a)
      ~default
      ~parser
      (module M : Sexpable with type t = a)
      ~fallback
  =
  let%sub form =
    let%sub form =
      Form.Elements.Textbox.sexpable
        ~extra_attrs:(Value.return [ Css.sexp_input ])
        (module M)
    in
    let%arr form = form in
    Form.project
      form
      ~parse_exn:(fun a ->
        let typed_components =
          Projection.unparse
            parser
            { Parse_result.result = a
            ; remaining = Bonsai_web_ui_url_var.Typed.Components.empty
            }
        in
        Typed.Components.to_original_components typed_components)
      ~unparse:(fun components ->
        let typed_components = Typed.Components.of_original_components components in
        try (Projection.parse_exn parser typed_components).result with
        | _ -> fallback)
  in
  Form.Dynamic.with_default default form
;;

let component (type a) (t : a t) =
  let did_fallback_occur ~components_value =
    match components_value with
    | Error _ -> false
    | Ok components ->
      (try
         let (_ : a Parse_result.t) =
           Projection.parse_exn
             t.parser
             (Typed.Components.of_original_components components)
         in
         false
       with
       | _ -> true)
  in
  let is_same_as_fallback (components : Url_var.Components.t) =
    let fallback_components =
      Projection.unparse
        t.parser
        { Parse_result.result = t.fallback; remaining = Url_var.Typed.Components.empty }
      |> Url_var.Typed.Components.to_original_components
    in
    Url_var.Components.equal fallback_components components
  in
  let%sub uri_form = uri_form ~default:(Value.return t.starting_components) in
  let uri_form_value =
    let%map uri_form = uri_form in
    Form.value uri_form
  in
  let uri_form_set =
    let%map uri_form = uri_form in
    fun new_components ->
      match new_components with
      | Ok x -> if is_same_as_fallback x then Effect.return () else Form.set uri_form x
      | Error _ -> Effect.return ()
  in
  let uri_form_set_even_if_same_as_callback =
    let%map uri_form = uri_form in
    fun new_components ->
      match new_components with
      | Ok x -> Form.set uri_form x
      | Error _ -> Effect.return ()
  in
  let%sub typed_url_form =
    typed_url_form
      ~default:(Value.return t.starting_components)
      ~parser:t.parser
      t.type_
      ~fallback:t.fallback
  in
  let typed_url_form_value =
    let%map typed_url_form = typed_url_form in
    Form.value typed_url_form
  in
  let typed_url_form_set =
    let%map typed_url_form = typed_url_form in
    fun x ->
      match x with
      | Ok x -> Form.set typed_url_form x
      | Error _ -> Effect.return ()
  in
  let module T = struct
    type t = Url_var.Components.t Or_error.t [@@deriving sexp, equal]
  end
  in
  let%sub () =
    Bonsai_extra.mirror
      (module T)
      ~store_set:typed_url_form_set
      ~store_value:typed_url_form_value
      ~interactive_set:uri_form_set
      ~interactive_value:uri_form_value
  in
  let%arr uri_form = uri_form
  and typed_url_form = typed_url_form
  and typed_url_form_set = typed_url_form_set
  and uri_form_value = uri_form_value
  and uri_form_set_even_if_same_as_callback = uri_form_set_even_if_same_as_callback in
  let buttons =
    List.map t.example_urls ~f:(fun (name, element) ->
      let result =
        Ok
          (Projection.unparse
             t.parser
             { Parse_result.result = element; remaining = Typed.Components.empty }
           |> Typed.Components.to_original_components)
      in
      Vdom.Node.button
        ~attr:
          (Vdom.Attr.on_click (fun _ ->
             let%bind.Effect () = typed_url_form_set result in
             uri_form_set_even_if_same_as_callback result))
        [ Vdom.Node.text name ])
  in
  let possible_warning =
    if not (did_fallback_occur ~components_value:uri_form_value)
    then []
    else
      let module M = (val t.type_ : Sexpable with type t = a) in
      [ Feather_icon.svg
          Feather_icon.Alert_triangle
          ~fill:Tailwind_colors.red500
          ~extra_attrs:[ Vdom.Attr.style (Css_gen.margin_right (`Px 16)) ]
      ; Vdom.Node.text
          ("Error parsing! Falling back to: " ^ Sexp.to_string (M.sexp_of_t t.fallback))
      ]
  in
  Vdom.Node.div
    ~attr:(Vdom.Attr.many [ Css.paper; Css.column ])
    [ Vdom.Node.h3 [ Vdom.Node.text t.title ]
    ; Vdom.Node.p ~attr:Css.example_description [ Vdom.Node.text t.description ]
    ; Vdom.Node.div ~attr:Css.button_row buttons
    ; Vdom.Node.div
        [ Vdom.Node.div
            ~attr:Css.paper
            [ Vdom.Node.div [ Vdom.Node.text "🌐 URL Bar" ]
            ; Vdom.Node.div ~attr:Css.warning_section possible_warning
            ; Vdom.Node.div
                ~attr:Css.column
                (Form.View.to_vdom_plain (Form.view uri_form))
            ; Vdom.Node.div
                ~attr:Css.columns_wrapper
                [ Feather_icon.svg Feather_icon.Arrow_up
                ; Feather_icon.svg Feather_icon.Arrow_down
                ]
            ; Vdom.Node.div [ Vdom.Node.text "👾 Sexp of parsed URL" ]
            ; Vdom.Node.div
                ~attr:Css.column
                (Form.View.to_vdom_plain (Form.view typed_url_form))
            ]
        ; Vdom.Node.div
            ~attr:Css.column
            [ Vdom.Node.text "🐪 Url module"
            ; Vdom.Node.div [ Vdom.Node.pre [ Vdom.Node.text t.demo_code ] ]
            ]
        ]
    ]
;;

module Homepage_and_settings_demo =
  [%demo
    module Homepage_and_settings = struct
      type t =
        | Homepage
        | Settings of int
      [@@deriving typed_variants, sexp, equal]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Homepage -> Parser.end_of_path Parser.unit
        | Settings -> Parser.from_query_required ~key:"volume" Value_parser.int
      ;;
    end

    let parser = Parser.Variant.make (module Homepage_and_settings)

    let%expect_test _ =
      Parser.check_ok_and_print_urls_or_errors parser;
      [%expect
        {|
        URL parser looks good!
        ┌────────────────────────┐
        │ All urls               │
        ├────────────────────────┤
        │ /                      │
        │ /settings?volume=<int> │
        └────────────────────────┘ |}]
    ;;]

module Homepage_and_settings = Homepage_and_settings_demo.Homepage_and_settings

let home_and_settings : Homepage_and_settings.t t =
  { starting_components = Url_var.Components.create ~path:"" ()
  ; parser = Homepage_and_settings_demo.parser |> Projection.make
  ; type_ = (module Homepage_and_settings)
  ; fallback = Homepage
  ; title = "Path renaming"
  ; description = "This url renames the inferred \"/homepage\" to \"/\""
  ; demo_code = Homepage_and_settings_demo.ppx_demo_string
  ; example_urls =
      [ "/", Homepage_and_settings.Homepage; "/settings?volume=<int>", Settings 100 ]
  }
;;

module Foo_bar_example =
  [%demo
    module My_url = struct
      type t =
        | Foo
        | Bar
      [@@deriving typed_variants, sexp, equal]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Foo -> Parser.unit
        | Bar -> Parser.unit
      ;;
    end

    let parser = Parser.Variant.make (module My_url)

    let%expect_test _ =
      Parser.check_ok_and_print_urls_or_errors parser;
      [%expect
        {|
        URL parser looks good!
        ┌──────────┐
        │ All urls │
        ├──────────┤
        │ /bar     │
        │ /foo     │
        └──────────┘ |}]
    ;;]

let foo_bar_example =
  { starting_components = Url_var.Components.create ~path:"foo" ~query:String.Map.empty ()
  ; parser = Parser.Variant.make (module Foo_bar_example.My_url) |> Projection.make
  ; type_ = (module Foo_bar_example.My_url)
  ; fallback = Foo_bar_example.My_url.Foo
  ; title = "Variants example"
  ; description = ""
  ; demo_code = Foo_bar_example.ppx_demo_string
  ; example_urls =
      [ "/foo", Foo_bar_example.My_url.Foo; "/bar", Foo_bar_example.My_url.Bar ]
  }
;;

module Partial_match_example =
  [%demo
    module Book = struct
      type t =
        | Book_search
        | Book_view of string
      [@@deriving typed_variants, sexp, equal]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Book_search -> Parser.end_of_path Parser.unit
        | Book_view -> Parser.with_prefix [] (Parser.from_path Value_parser.string)
      ;;
    end

    module Movie = struct
      type t =
        | Movie_search
        | Movie_view of string
      [@@deriving typed_variants, sexp, equal]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Movie_search -> Parser.end_of_path Parser.unit
        | Movie_view -> Parser.with_prefix [] (Parser.from_path Value_parser.string)
      ;;
    end

    module Library = struct
      type t =
        | Content_search
        | Book of Book.t
        | Movie of Movie.t
      [@@deriving typed_variants, sexp, equal]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Content_search -> Parser.end_of_path Parser.unit
        | Book -> Parser.with_prefix [ "book" ] (Parser.Variant.make (module Book))
        | Movie -> Parser.with_prefix [ "movie" ] (Parser.Variant.make (module Movie))
      ;;
    end

    module My_url = struct
      type t =
        | Library_search
        | Library_page of
            { library_name : string
            ; library_contents : Library.t
            } [@typed_fields]
      [@@deriving typed_variants, sexp, equal]

      module Anon_library_page = struct
        module Typed_field =
          Typed_variant.Typed_variant_anonymous_records.Typed_field_of_library_page

        let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
          | Library_name -> Parser.from_path Value_parser.string
          | Library_contents -> Parser.Variant.make (module Library)
        ;;

        module Path_order = Path_order (Typed_field)

        let path_order = Path_order.T [ Library_name; Library_contents ]
      end

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Library_search -> Parser.with_remaining_path [ "library" ] Parser.unit
        | Library_page ->
          Parser.with_prefix [ "library" ] (Parser.Record.make (module Anon_library_page))
      ;;
    end

    let parser = Parser.Variant.make (module My_url)

    let%expect_test _ =
      Parser.check_ok_and_print_urls_or_errors parser;
      [%expect
        {|
        URL parser looks good!
        ┌──────────────────────────────────┐
        │ All urls                         │
        ├──────────────────────────────────┤
        │ /library                         │
        │ /library/<string>                │
        │ /library/<string>/book           │
        │ /library/<string>/book/<string>  │
        │ /library/<string>/movie          │
        │ /library/<string>/movie/<string> │
        └──────────────────────────────────┘ |}]
    ;;]

let folder_example : Partial_match_example.My_url.t t =
  { starting_components =
      Url_var.Components.create ~path:"library" ~query:String.Map.empty ()
  ; parser = Parser.Variant.make (module Partial_match_example.My_url) |> Projection.make
  ; type_ = (module Partial_match_example.My_url)
  ; fallback = Partial_match_example.My_url.Library_search
  ; title = "Folder-like urls"
  ; description =
      "The tricky thing in this example is making early terminations of your path parser \
       e.g. \"book/\" vs. \"book/<book_id>\"\n\
       You can express the different between the two through [with_remaining_path] and \
       [with_prefix]"
  ; demo_code = Partial_match_example.ppx_demo_string
  ; example_urls =
      [ "/library", Partial_match_example.My_url.Library_search
      ; ( "/library/<string>"
        , Library_page { library_name = "nyc"; library_contents = Content_search } )
      ; ( "/library/<string>/book"
        , Library_page { library_name = "nyc"; library_contents = Book Book_search } )
      ; ( "/library/<string>/book/<string>"
        , Library_page
            { library_name = "nyc"; library_contents = Book (Book_view "dune") } )
      ; ( "/library/<string>/movie"
        , Library_page { library_name = "nyc"; library_contents = Movie Movie_search } )
      ; ( "/library/<string>/movie/<string>"
        , Library_page
            { library_name = "nyc"; library_contents = Movie (Movie_view "dune") } )
      ]
  }
;;

module Reading_from_query =
  [%demo
    module My_url = struct
      type t = int [@@deriving sexp, equal]

      let parser = Parser.from_query_required ~key:"video" Value_parser.int

      let%expect_test _ =
        Parser.check_ok_and_print_urls_or_errors parser;
        [%expect
          {|
          URL parser looks good!
          ┌───────────────┐
          │ All urls      │
          ├───────────────┤
          │ /?video=<int> │
          └───────────────┘ |}]
      ;;
    end]

let reading_from_query =
  { starting_components =
      Url_var.Components.create ~query:(String.Map.of_alist_exn [ "video", [ "123" ] ]) ()
  ; parser = Reading_from_query.My_url.parser |> Projection.make
  ; type_ = (module Reading_from_query.My_url)
  ; fallback = 0
  ; title = "Reading from query"
  ; description = ""
  ; demo_code = Reading_from_query.ppx_demo_string
  ; example_urls = []
  }
;;

module Reading_from_path =
  [%demo
    module My_url = struct
      type t = int [@@deriving sexp, equal]

      let parser = Parser.from_path Value_parser.int

      let%expect_test _ =
        Parser.check_ok_and_print_urls_or_errors parser;
        [%expect
          {|
          URL parser looks good!
          ┌──────────┐
          │ All urls │
          ├──────────┤
          │ /<int>   │
          └──────────┘ |}]
      ;;
    end]

let reading_from_path =
  { starting_components = Url_var.Components.create ~path:"123" ()
  ; parser = Reading_from_path.My_url.parser |> Projection.make
  ; type_ = (module Reading_from_path.My_url)
  ; fallback = 0
  ; title = "Reading from path"
  ; description = ""
  ; demo_code = Reading_from_path.ppx_demo_string
  ; example_urls = []
  }
;;

module Search =
  [%demo
    module My_url = struct
      type t =
        | Homepage
        | Search of string
      [@@deriving typed_variants, sexp, equal]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Search -> Parser.from_query_required ~key:"q" Value_parser.string
        | Homepage -> Parser.unit
      ;;
    end

    let parser = Parser.Variant.make (module My_url)

    let%expect_test _ =
      Parser.check_ok_and_print_urls_or_errors parser;
      [%expect
        {|
        URL parser looks good!
        ┌────────────────────┐
        │ All urls           │
        ├────────────────────┤
        │ /homepage          │
        │ /search?q=<string> │
        └────────────────────┘ |}]
    ;;]

let search_example =
  { starting_components =
      Url_var.Components.create
        ~path:"search"
        ~query:(String.Map.of_alist_exn [ "q", [ "capybara" ] ])
        ()
  ; parser = Search.parser |> Projection.make
  ; type_ = (module Search.My_url)
  ; fallback = Homepage
  ; title = "Google clone example"
  ; description = ""
  ; demo_code = Search.ppx_demo_string
  ; example_urls = [ "/search?q=<string>", Search "capybara"; "/homepage", Homepage ]
  }
;;

module Error_message_example =
  [%demo
    module My_url = struct
      type t =
        | Foo
        | Bar
      [@@deriving typed_variants, sexp, equal]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Foo -> Parser.with_prefix [ "foo" ] Parser.unit
        | Bar -> Parser.with_prefix [ "foo" ] Parser.unit
      ;;
    end

    let parser = Parser.Variant.make (module My_url)

    let%expect_test _ =
      Parser.check_ok_and_print_urls_or_errors parser;
      [%expect
        {|
        Error with parser.
        ┌─────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
        │ Check name                                              │ Error message                                                                            │
        ├─────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
        │ Ambiguous choices for picking variant constructor check │ ("Duplicate patterns found!"                                                             │
        │                                                         │  (duplicate_patterns (((pattern ((Match foo))) (needed_match Prefix)))))                 │
        │ Duplicate urls check                                    │ ("Ambiguous, duplicate urls expressed in parser! This was probably caused due to conflic │
        │                                                         │ ting renames with [with_prefix] or [with_remaining_path]."                               │
        │                                                         │  (duplicate_urls (/foo)))                                                                │
        └─────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
    ;;]

let error_example_component =
  let out =
    Vdom.Node.div
      ~attr:(Vdom.Attr.many [ Css.paper; Css.column ])
      [ Vdom.Node.h3 [ Vdom.Node.text "Error in URL detection." ]
      ; Vdom.Node.p
          ~attr:Css.example_description
          [ Vdom.Node.text
              "Errors in the URL can be detected before any parsing occurs inside of an \
               expect test!"
          ]
      ; Vdom.Node.div
          ~attr:Css.column
          [ Vdom.Node.text "🐪 Url module"
          ; Vdom.Node.div
              [ Vdom.Node.pre [ Vdom.Node.text Error_message_example.ppx_demo_string ] ]
          ]
      ]
  in
  Bonsai.const out
;;

module Simple_record_example =
  [%demo
    module My_url = struct
      type t =
        { foo : int
        ; bar : string
        }
      [@@deriving typed_fields, sexp, equal]

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | Foo -> Parser.from_query_required Value_parser.int
        | Bar -> Parser.from_path Value_parser.string
      ;;

      module Path_order = Path_order (Typed_field)

      let path_order = Path_order.T [ Bar ]
    end

    let parser = Parser.Record.make (module My_url)

    let%expect_test _ =
      Parser.check_ok_and_print_urls_or_errors parser;
      [%expect
        {|
        URL parser looks good!
        ┌─────────────────────┐
        │ All urls            │
        ├─────────────────────┤
        │ /<string>?foo=<int> │
        └─────────────────────┘ |}]
    ;;]

let simple_record_example =
  { starting_components =
      Url_var.Components.create
        ~path:"hello"
        ~query:(String.Map.of_alist_exn [ "foo", [ "123" ] ])
        ()
  ; parser = Simple_record_example.parser |> Projection.make
  ; type_ = (module Simple_record_example.My_url)
  ; fallback = { Simple_record_example.My_url.foo = 1; bar = "hi" }
  ; title = "Simple record"
  ; description = ""
  ; demo_code = Simple_record_example.ppx_demo_string
  ; example_urls = []
  }
;;

module Anonymous_record_example =
  [%demo
    module My_youtube_clone = struct
      type t =
        | Homepage
        | Video of
            { video_id : string
            ; channel : string
            } [@typed_fields]
      [@@deriving typed_variants, sexp, equal]

      module Anon_video_record = struct
        module Typed_field =
          Typed_variant.Typed_variant_anonymous_records.Typed_field_of_video

        let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
          | Video_id -> Parser.from_query_required Value_parser.string
          | Channel -> Parser.from_query_required Value_parser.string
        ;;

        module Path_order = Path_order (Typed_field)

        let path_order = Path_order.T []
      end

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Homepage -> Parser.end_of_path Parser.unit
        | Video -> Parser.Record.make ~namespace:[] (module Anon_video_record)
      ;;
    end

    let parser = Parser.Variant.make (module My_youtube_clone)

    let%expect_test _ =
      Parser.check_ok_and_print_urls_or_errors parser;
      [%expect
        {|
        URL parser looks good!
        ┌───────────────────────────────────────────┐
        │ All urls                                  │
        ├───────────────────────────────────────────┤
        │ /                                         │
        │ /video?channel=<string>&video_id=<string> │
        └───────────────────────────────────────────┘ |}]
    ;;]

let anonymous_record_example =
  { starting_components =
      Url_var.Components.create
        ~query:
          (String.Map.of_alist_exn
             [ "video_id", [ "dQw4w9WgXcQ" ]; "channel", [ "abcd" ] ])
        ()
  ; parser = Anonymous_record_example.parser |> Projection.make
  ; type_ = (module Anonymous_record_example.My_youtube_clone)
  ; fallback = Homepage
  ; title = "Anonymous records"
  ; description =
      "Anonymous records are weird. If you try and declare one outside of the variant it \
       belongs to, you'll get a weird error. This makes getting a ['a Parser.t] where 'a \
       is an anonymous record tricky. Luckily, [@@deriving typed_variants] takes care of \
       this! It will basically just copy pastes the definition and its functions are \
       able to convert between the internal anonymous record, and the fake anonymous \
       record. You can also derive [typed_fields] on the records by attaching \
       [@typed_fields] to the constructor's anonymous record."
  ; demo_code = Anonymous_record_example.ppx_demo_string
  ; example_urls =
      [ "/", Homepage
      ; ( "video?channel=<string>&video=<string>"
        , Video { video_id = "dQw4w9WgXcQ"; channel = "abcd" } )
      ]
  }
;;

module Tuple_example =
  [%demo
    module My_tuple_url = struct
      type t = int * string [@@deriving typed_fields, sexp, equal]

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | T_1 -> Parser.from_query_required Value_parser.int
        | T_2 -> Parser.from_query_required Value_parser.string
      ;;

      module Path_order = Path_order (Typed_field)

      let path_order = Path_order.T []
    end

    let parser = Parser.Record.make (module My_tuple_url)

    let%expect_test _ =
      Parser.check_ok_and_print_urls_or_errors parser;
      [%expect
        {|
        URL parser looks good!
        ┌──────────────────────────┐
        │ All urls                 │
        ├──────────────────────────┤
        │ /?t_1=<int>&t_2=<string> │
        └──────────────────────────┘ |}]
    ;;]

let tuple_example =
  { starting_components =
      Url_var.Components.create
        ~query:(String.Map.of_alist_exn [ "t_1", [ "1" ]; "t_2", [ "hello" ] ])
        ()
  ; parser = Tuple_example.parser |> Projection.make
  ; type_ = (module Tuple_example.My_tuple_url)
  ; fallback = 1, "hello"
  ; title = "Tuple example"
  ; description = "Deceivingly [Parser.Record] also works with tuples!"
  ; demo_code = Tuple_example.ppx_demo_string
  ; example_urls = []
  }
;;

let examples =
  List.map [ T reading_from_query; T reading_from_path ] ~f:(fun (T example) ->
    component example)
  @ [ error_example_component ]
  @ List.map
      [ T foo_bar_example
      ; T simple_record_example
      ; T search_example
      ; T home_and_settings
      ; T anonymous_record_example
      ; T tuple_example
      ; T folder_example
      ]
      ~f:(fun (T example) -> component example)
  |> Computation.all
;;

(* $MDX part-begin=search_example *)

module My_google_clone = struct
  type t =
    | Homepage
    | Search of string
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.unit
    | Search -> Parser.from_query_required ~key:"q" Value_parser.string
  ;;
end

let parser = Parser.Variant.make (module My_google_clone)
let versioned_parser = Versioned_parser.first_parser parser

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────────┐
    │ All urls           │
    ├────────────────────┤
    │ /homepage          │
    │ /search?q=<string> │
    └────────────────────┘ |}]
;;

(* $MDX part-end *)

let (_ : My_google_clone.t Typed.Versioned_parser.t) = versioned_parser

(* $MDX part-begin=detected_error *)

module Ambiguous_url = struct
  type t =
    | Foo
    | Bar
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Foo -> Parser.with_prefix [ "x" ] Parser.unit
    | Bar -> Parser.with_prefix [ "x" ] Parser.unit
  ;;
end

let parser = Parser.Variant.make (module Ambiguous_url)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌─────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name                                              │ Error message                                                                            │
    ├─────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Ambiguous choices for picking variant constructor check │ ("Duplicate patterns found!"                                                             │
    │                                                         │  (duplicate_patterns (((pattern ((Match x))) (needed_match Prefix)))))                   │
    │ Duplicate urls check                                    │ ("Ambiguous, duplicate urls expressed in parser! This was probably caused due to conflic │
    │                                                         │ ting renames with [with_prefix] or [with_remaining_path]."                               │
    │                                                         │  (duplicate_urls (/x)))                                                                  │
    └─────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

(* $MDX part-end *)

let (_ : Ambiguous_url.t Parser.t) = parser

(* $MDX part-begin=versioning *)

module V1 = struct
  type t = Foo of string [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Foo -> Parser.from_path Value_parser.string
  ;;
end

let v1_parser = Versioned_parser.first_parser (Parser.Variant.make (module V1))

module V2 = struct
  type t = New_foo of string [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | New_foo -> Parser.from_path Value_parser.string
  ;;
end

let v2_parser =
  Versioned_parser.new_parser
    (Parser.Variant.make (module V2))
    ~previous:v1_parser
    ~f:(function
      | Foo s -> New_foo s)
;;

let%expect_test _ =
  Versioned_parser.check_ok_and_print_urls_or_errors v2_parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────┐
    │ All urls          │
    ├───────────────────┤
    │ /new_foo/<string> │
    └───────────────────┘

           |
    falls back to
           |
           v

    URL parser looks good!
    ┌───────────────┐
    │ All urls      │
    ├───────────────┤
    │ /foo/<string> │
    └───────────────┘ |}]
;;

(* $MDX part-end *)

let (_ : Ambiguous_url.t Parser.t) = parser

module _ = struct
  (* $MDX part-begin=old_api *)

  module My_google_clone = struct
    type t =
      | Homepage
      | Search of string
    [@@deriving sexp, equal]

    let parse_exn ({ path; query; _ } : Url_var.Components.t) : t =
      let path = String.split path ~on:'/' in
      match path with
      | [ "home" ] -> Homepage
      | [ "search" ] ->
        (match Map.find (query : _ String.Map.t) "q" with
         | Some [ query ] -> Search query
         | None | Some [] -> failwith "search missing query param"
         | Some (_ :: _ :: _) -> failwith "search with too many query params")
      | _ -> failwith "unknown path"
    ;;

    let unparse (t : t) : Url_var.Components.t =
      match t with
      | Homepage -> Url_var.Components.create ~path:"home" ()
      | Search query ->
        Url_var.Components.create
          ~path:"search"
          ~query:(String.Map.singleton "q" [ query ])
          ()
    ;;
  end

  (* $MDX part-end *)

  let (_ : Url_var.Components.t -> My_google_clone.t) = My_google_clone.parse_exn
  let (_ : My_google_clone.t -> Url_var.Components.t) = My_google_clone.unparse
end

module _ = struct
  (* $MDX part-begin=record_all_from_query *)
  module My_google_clone = struct
    type t =
      { query : string
      ; language : string
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Query -> Parser.from_query_required ~key:"q" Value_parser.string
      | Language -> Parser.from_query_required Value_parser.string
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────┐
      │ All urls                       │
      ├────────────────────────────────┤
      │ /?language=<string>&q=<string> │
      └────────────────────────────────┘ |}]
  ;;
  (* $MDX part-end *)
end

module _ = struct
  (* $MDX part-begin=record_2 *)
  module My_google_clone = struct
    module Language : sig
      type t [@@deriving sexp, equal]

      val to_string : t -> string
      val of_string : string -> t
    end = struct
      include String
    end

    type t =
      { query : string
      ; language : Language.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Query -> Parser.from_query_required ~key:"q" Value_parser.string
      | Language ->
        Parser.from_query_required
          (Value_parser.project
             Value_parser.string
             ~parse_exn:Language.of_string
             ~unparse:Language.to_string)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌─────────────────────────────────────────┐
      │ All urls                                │
      ├─────────────────────────────────────────┤
      │ /?language=<project<string>>&q=<string> │
      └─────────────────────────────────────────┘ |}]
  ;;
  (* $MDX part-end *)
end

module _ = struct
  (* $MDX part-begin=record_path_error *)
  module My_google_clone = struct
    module Language : sig
      type t [@@deriving sexp, equal]

      val to_string : t -> string
      val of_string : string -> t
    end = struct
      include String
    end

    type t =
      { query : string
      ; language : Language.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Query -> Parser.from_query_required ~key:"q" Value_parser.string
      | Language ->
        Parser.from_path
          (Value_parser.project
             Value_parser.string
             ~parse_exn:Language.of_string
             ~unparse:Language.to_string)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      Error with parser.
      ┌────────────────────────┬───────────────────────────────────────────────────────────────────────────────────────┐
      │ Check name             │ Error message                                                                         │
      ├────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────┤
      │ Sane path orders check │ ("Each path parser must be present in path order. The following fields were missing:" │
      │                        │  (missing_fields (Language)))                                                         │
      └────────────────────────┴───────────────────────────────────────────────────────────────────────────────────────┘ |}]
  ;;
  (* $MDX part-end *)
end

module _ = struct
  (* $MDX part-begin=record_path *)
  module My_google_clone = struct
    module Language : sig
      type t [@@deriving sexp, equal]

      val to_string : t -> string
      val of_string : string -> t
    end = struct
      include String
    end

    type t =
      { query : string
      ; language : Language.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Query -> Parser.from_query_required ~key:"q" Value_parser.string
      | Language ->
        Parser.from_path
          (Value_parser.project
             Value_parser.string
             ~parse_exn:Language.of_string
             ~unparse:Language.to_string)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T [ Language ]
  end

  let parser = Parser.Record.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌───────────────────────────────┐
      │ All urls                      │
      ├───────────────────────────────┤
      │ /<project<string>>?q=<string> │
      └───────────────────────────────┘ |}]
  ;;
  (* $MDX part-end *)
end

module _ = struct
  (* $MDX part-begin=record_path_3 *)
  module Nested = struct
    type t =
      { foo : string
      ; bar : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Foo -> Parser.from_query_required Value_parser.string
      | Bar -> Parser.from_query_required Value_parser.int
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  module Parent = struct
    type t =
      { a : Nested.t
      ; b : Nested.t
      }
    [@@deriving typed_fields, equal, sexp]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> Parser.Record.make (module Nested)
      | B -> Parser.Record.make (module Nested)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module Parent)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌─────────────────────────────────────────────────────────┐
      │ All urls                                                │
      ├─────────────────────────────────────────────────────────┤
      │ /?a.bar=<int>&a.foo=<string>&b.bar=<int>&b.foo=<string> │
      └─────────────────────────────────────────────────────────┘ |}]
  ;;
  (* $MDX part-end *)
end

module _ = struct
  (* $MDX part-begin=variant_1 *)
  module My_google_clone = struct
    type t =
      | Homepage
      | Search
      | Maps
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Homepage -> Parser.unit
      | Search -> Parser.unit
      | Maps -> Parser.unit
    ;;
  end

  let parser = Parser.Variant.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
    URL parser looks good!
    ┌───────────┐
    │ All urls  │
    ├───────────┤
    │ /homepage │
    │ /maps     │
    │ /search   │
    └───────────┘ |}]
  ;;
  (* $MDX part-end *)
end

module _ = struct
  (* $MDX part-begin=variant_2 *)
  module My_google_clone = struct
    type t =
      | Homepage
      | Search
      | Maps
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Homepage -> Parser.end_of_path Parser.unit
      | Search -> Parser.unit
      | Maps -> Parser.unit
    ;;
  end

  let parser = Parser.Variant.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
    URL parser looks good!
    ┌──────────┐
    │ All urls │
    ├──────────┤
    │ /        │
    │ /maps    │
    │ /search  │
    └──────────┘ |}]
  ;;
  (* $MDX part-end *)
end

module _ = struct
  (* $MDX part-begin=variant_3 *)
  module My_google_clone = struct
    type t =
      | Homepage
      | Search of string
      | Maps of string
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Homepage -> Parser.end_of_path Parser.unit
      | Search -> Parser.from_query_required ~key:"query" Value_parser.string
      | Maps -> Parser.from_query_required ~key:"location" Value_parser.string
    ;;
  end

  let parser = Parser.Variant.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /                       │
    │ /maps?location=<string> │
    │ /search?query=<string>  │
    └─────────────────────────┘ |}]
  ;;
  (* $MDX part-end *)
end
