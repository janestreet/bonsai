open! Core
open Uri_parsing
module Url_var = Bonsai_web_ui_url_var

let%expect_test "Catch all when there is a fallback that fails" =
  let module Url = struct
    type t =
      | Normal of int
      | Fallback
      | Catch_all
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Normal -> Parser.from_path Value_parser.int
      | Fallback -> Parser.unit
      | Catch_all -> Parser.unit
    ;;
  end
  in
  let parser = Parser.Variant.make (module Url) in
  let versioned_parser = Versioned_parser.first_parser parser in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────┐
    │ All urls      │
    ├───────────────┤
    │ /catch_all    │
    │ /fallback     │
    │ /normal/<int> │
    └───────────────┘ |}];
  let projection =
    Url_var.Typed.make_projection
      ~on_fallback_raises:Url.Catch_all
      versioned_parser
      ~fallback:(fun _ -> failwith "error!")
  in
  let result =
    projection.parse_exn (Url_var.Components.create ~path:"normal/not_an_int" ())
  in
  print_s (Url.sexp_of_t result);
  [%expect {| Catch_all |}]
;;

let%expect_test "first non-typed projection test" =
  let module Old_url = struct
    type t = Foo of string [@@deriving sexp, equal]

    let parse_exn (components : Url_var.Components.t) = Foo components.path
    let unparse (Foo result) = Url_var.Components.create ~path:result ()
  end
  in
  let versioned_parser =
    Url_var.Typed.Versioned_parser.of_non_typed_parser
      ~parse_exn:Old_url.parse_exn
      ~unparse:Old_url.unparse
  in
  let projection = Versioned_parser.eval versioned_parser in
  Uri_parsing_test.expect_output_and_identity_roundtrip
    projection
    ~path:[ "something!" ]
    ~query:String.Map.empty
    ~sexp_of_t:Old_url.sexp_of_t
    ~expect:(fun () -> [%expect {|
   (Foo something!) |}])
;;

module First_url = struct
  type t = Foo of string [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Foo -> Parser.from_path Value_parser.string
  ;;
end

module Second_url = struct
  type t =
    | New_foo of string
    | Bar of int
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | New_foo -> Parser.from_path Value_parser.string
    | Bar -> Parser.from_path Value_parser.int
  ;;
end

module Third_url = struct
  type t =
    | Ultra_new_foo of string
    | New_bar of int
    | Baz of float
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Ultra_new_foo -> Parser.from_path Value_parser.string
    | New_bar -> Parser.from_path Value_parser.int
    | Baz -> Parser.from_path Value_parser.float
  ;;
end

let%expect_test "non-typed -> typed -> typed" =
  let module Old_url = struct
    type t = Foo of string [@@deriving sexp, equal]

    let parse_exn (components : Url_var.Components.t) = Foo components.path
    let unparse (Foo result) = Url_var.Components.create ~path:result ()
  end
  in
  let first_parser =
    Url_var.Typed.Versioned_parser.of_non_typed_parser
      ~parse_exn:Old_url.parse_exn
      ~unparse:Old_url.unparse
  in
  let second_parser = Parser.Variant.make (module Second_url) in
  Parser.check_ok_and_print_urls_or_errors second_parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────┐
    │ All urls          │
    ├───────────────────┤
    │ /bar/<int>        │
    │ /new_foo/<string> │
    └───────────────────┘ |}];
  let second_parser =
    Versioned_parser.new_parser
      second_parser
      ~f:(fun (Foo x) -> New_foo x)
      ~previous:first_parser
  in
  let third_parser = Parser.Variant.make (module Third_url) in
  Parser.check_ok_and_print_urls_or_errors third_parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /baz/<float>            │
    │ /new_bar/<int>          │
    │ /ultra_new_foo/<string> │
    └─────────────────────────┘ |}];
  let third_parser =
    Versioned_parser.new_parser
      third_parser
      ~f:(function
        | Second_url.New_foo x -> Ultra_new_foo x
        | Bar x -> New_bar x)
      ~previous:second_parser
  in
  let projection = Versioned_parser.eval third_parser in
  Uri_parsing_test.expect_output_and_identity_roundtrip
    ~expect_diff:(fun () ->
      [%expect {|
    -1,1 +1,1
    -|(something!)
    +|(ultra_new_foo something!) |}])
    projection
    ~path:[ "something!" ]
    ~query:String.Map.empty
    ~sexp_of_t:Third_url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
   ("URL unrecognized, maybe this is an old URL? Attempting to parse with a previous URL parser. Here's the error of the current parser:"
    (error
     ("Error while parsing! No matching variant contructor found for current path!"
      (components ((path (something!)) (query ())))
      (available_patterns
       ((ultra_new_foo
         ((pattern ((Match ultra_new_foo))) (needed_match Prefix)))
        (new_bar ((pattern ((Match new_bar))) (needed_match Prefix)))
        (baz ((pattern ((Match baz))) (needed_match Prefix))))))))
   ("URL unrecognized, maybe this is an old URL? Attempting to parse with a previous URL parser. Here's the error of the current parser:"
    (error
     ("Error while parsing! No matching variant contructor found for current path!"
      (components ((path (something!)) (query ())))
      (available_patterns
       ((new_foo ((pattern ((Match new_foo))) (needed_match Prefix)))
        (bar ((pattern ((Match bar))) (needed_match Prefix))))))))
   (Ultra_new_foo something!) |}]);
  Uri_parsing_test.expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {|
    -1,1 +1,1
    -|(bar 7)
    +|(new_bar 7) |}])
    projection
    ~path:[ "bar"; "7" ]
    ~query:String.Map.empty
    ~sexp_of_t:Third_url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
   ("URL unrecognized, maybe this is an old URL? Attempting to parse with a previous URL parser. Here's the error of the current parser:"
    (error
     ("Error while parsing! No matching variant contructor found for current path!"
      (components ((path (bar 7)) (query ())))
      (available_patterns
       ((ultra_new_foo
         ((pattern ((Match ultra_new_foo))) (needed_match Prefix)))
        (new_bar ((pattern ((Match new_bar))) (needed_match Prefix)))
        (baz ((pattern ((Match baz))) (needed_match Prefix))))))))
   (New_bar 7)|}]);
  Uri_parsing_test.expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {|
    -1,1 +1,1
    -|(baz 1.0)
    +|(baz 1.) |}])
    projection
    ~path:[ "baz"; "1.0" ]
    ~query:String.Map.empty
    ~sexp_of_t:Third_url.sexp_of_t
    ~expect:(fun () -> [%expect {|
   (Baz 1) |}])
;;

let%expect_test "typed -> typed -> typed" =
  let first_parser =
    Parser.Variant.make (module First_url) |> Versioned_parser.first_parser
  in
  let second_parser =
    Parser.Variant.make (module Second_url)
    |> Versioned_parser.new_parser ~f:(fun (Foo x) -> New_foo x) ~previous:first_parser
  in
  let third_parser =
    Parser.Variant.make (module Third_url)
    |> Versioned_parser.new_parser
         ~f:(function
           | Second_url.New_foo x -> Ultra_new_foo x
           | Bar x -> New_bar x)
         ~previous:second_parser
  in
  Versioned_parser.check_ok_and_print_urls_or_errors third_parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /baz/<float>            │
    │ /new_bar/<int>          │
    │ /ultra_new_foo/<string> │
    └─────────────────────────┘

           |
    falls back to
           |
           v

    URL parser looks good!
    ┌───────────────────┐
    │ All urls          │
    ├───────────────────┤
    │ /bar/<int>        │
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
    └───────────────┘ |}];
  let projection = Versioned_parser.eval third_parser in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query = String.Map.empty; path = [ "unknown" ] });
  [%expect
    {|
    ("URL unrecognized, maybe this is an old URL? Attempting to parse with a previous URL parser. Here's the error of the current parser:"
     (error
      ("Error while parsing! No matching variant contructor found for current path!"
       (components ((path (unknown)) (query ())))
       (available_patterns
        ((ultra_new_foo
          ((pattern ((Match ultra_new_foo))) (needed_match Prefix)))
         (new_bar ((pattern ((Match new_bar))) (needed_match Prefix)))
         (baz ((pattern ((Match baz))) (needed_match Prefix))))))))
    ("URL unrecognized, maybe this is an old URL? Attempting to parse with a previous URL parser. Here's the error of the current parser:"
     (error
      ("Error while parsing! No matching variant contructor found for current path!"
       (components ((path (unknown)) (query ())))
       (available_patterns
        ((new_foo ((pattern ((Match new_foo))) (needed_match Prefix)))
         (bar ((pattern ((Match bar))) (needed_match Prefix))))))))
    ("Error while parsing! No matching variant contructor found for current path!"
     (components ((path (unknown)) (query ())))
     (available_patterns ((foo ((pattern ((Match foo))) (needed_match Prefix)))))) |}];
  Uri_parsing_test.expect_output_and_identity_roundtrip
    ~expect_diff:(fun () ->
      [%expect
        {|
    -1,1 +1,1
    -|(foo something!)
    +|(ultra_new_foo something!) |}])
    projection
    ~path:[ "foo"; "something!" ]
    ~query:String.Map.empty
    ~sexp_of_t:Third_url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
   ("URL unrecognized, maybe this is an old URL? Attempting to parse with a previous URL parser. Here's the error of the current parser:"
    (error
     ("Error while parsing! No matching variant contructor found for current path!"
      (components ((path (foo something!)) (query ())))
      (available_patterns
       ((ultra_new_foo
         ((pattern ((Match ultra_new_foo))) (needed_match Prefix)))
        (new_bar ((pattern ((Match new_bar))) (needed_match Prefix)))
        (baz ((pattern ((Match baz))) (needed_match Prefix))))))))
   ("URL unrecognized, maybe this is an old URL? Attempting to parse with a previous URL parser. Here's the error of the current parser:"
    (error
     ("Error while parsing! No matching variant contructor found for current path!"
      (components ((path (foo something!)) (query ())))
      (available_patterns
       ((new_foo ((pattern ((Match new_foo))) (needed_match Prefix)))
        (bar ((pattern ((Match bar))) (needed_match Prefix))))))))
   (Ultra_new_foo something!) |}]);
  Uri_parsing_test.expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {|
    -1,1 +1,1
    -|(bar 7)
    +|(new_bar 7) |}])
    projection
    ~path:[ "bar"; "7" ]
    ~query:String.Map.empty
    ~sexp_of_t:Third_url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
   ("URL unrecognized, maybe this is an old URL? Attempting to parse with a previous URL parser. Here's the error of the current parser:"
    (error
     ("Error while parsing! No matching variant contructor found for current path!"
      (components ((path (bar 7)) (query ())))
      (available_patterns
       ((ultra_new_foo
         ((pattern ((Match ultra_new_foo))) (needed_match Prefix)))
        (new_bar ((pattern ((Match new_bar))) (needed_match Prefix)))
        (baz ((pattern ((Match baz))) (needed_match Prefix))))))))
   (New_bar 7)|}]);
  Uri_parsing_test.expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {| |}])
    projection
    ~path:[ "baz"; "1." ]
    ~query:String.Map.empty
    ~sexp_of_t:Third_url.sexp_of_t
    ~expect:(fun () -> [%expect {|
   (Baz 1) |}])
;;

let%expect_test "to_url_string" =
  let module Url = struct
    type t =
      { a : int
      ; b : int
      ; c : float
      }
    [@@deriving sexp, equal, typed_fields]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> Parser.from_path Value_parser.int
      | B -> Parser.from_query_required Value_parser.int
      | C -> Parser.from_query_required Value_parser.float
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T [ A ]
  end
  in
  let parser = Parser.Record.make (module Url) in
  print_endline (Url_var.Typed.to_url_string parser { a = 1; b = 2; c = 3. });
  [%expect {| 1?b=2&c=3. |}]
;;
