open! Core
open Uri_parsing

let%test_module "path parser after remaining_path" =
  (module struct
    module T = struct
      type t =
        { foo : string list
        ; bar : string
        }
      [@@deriving sexp_of, typed_fields]

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | Foo -> Parser.from_remaining_path Value_parser.string
        | Bar -> Parser.from_path Value_parser.string
      ;;

      module Path_order = Parser.Record.Path_order (Typed_field)

      let path_order = Path_order.T [ Foo; Bar ]
    end

    let unparseable_parser =
      let parser = Parser.Record.make (module T) in
      Versioned_parser.first_parser parser
    ;;

    let%expect_test "path parser after from remaining path parser passes the check." =
      Versioned_parser.check_ok_and_print_urls_or_errors unparseable_parser;
      [%expect
        {|
        Error with parser.
        ┌───────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
        │ Check name                                        │ Error message                                                                            │
        ├───────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
        │ Remaining path does not block other parsers check │ "Error! There cannot be path parsers inside of a [with_remaining_path] combinator! The r │
        │                                                   │ eason for this is that there won't be any values after that parser!"                     │
        └───────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
        |}]
    ;;

    let%expect_test "path parser after from remaining path parser passes the check." =
      let projection =
        Versioned_parser.eval_for_uri ~encoding_behavior:Correct unparseable_parser
      in
      let original = Uri.make ~path:"a/b/c/d/e" () in
      print_endline (Uri.to_string original);
      [%expect {| a/b/c/d/e |}];
      Expect_test_helpers_core.require_does_raise [%here] (fun () ->
        projection.parse_exn original);
      [%expect
        {|
        ("Error while parsing record field:"
          (error_message "Expected a value in path, but nothing was present")
          (field_name bar)
          (unparseable_components (
            (path  ())
            (query ()))))
        |}]
    ;;
  end)
;;

let%test_module "path parser after end_of_path" =
  (module struct
    module T = struct
      type t =
        { foo : unit
        ; bar : string
        }
      [@@deriving sexp_of, typed_fields]

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | Foo -> Parser.end_of_path Parser.unit
        | Bar -> Parser.from_path Value_parser.string
      ;;

      module Path_order = Parser.Record.Path_order (Typed_field)

      let path_order = Path_order.T [ Foo; Bar ]
    end

    let unparseable_parser =
      let parser = Parser.Record.make (module T) in
      Versioned_parser.first_parser parser
    ;;

    let%expect_test "path parser after from remaining path parser passes the check." =
      Versioned_parser.check_ok_and_print_urls_or_errors unparseable_parser;
      [%expect
        {|
        Error with parser.
        ┌───────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
        │ Check name                                        │ Error message                                                                            │
        ├───────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
        │ Remaining path does not block other parsers check │ "Error! There cannot be path parsers inside of a [with_remaining_path] combinator! The r │
        │                                                   │ eason for this is that there won't be any values after that parser!"                     │
        └───────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
        |}]
    ;;

    let%expect_test "path parser after from remaining path parser passes the check." =
      let projection =
        Versioned_parser.eval_for_uri ~encoding_behavior:Correct unparseable_parser
      in
      let original = Uri.make ~path:"e" () in
      print_endline (Uri.to_string original);
      [%expect {| e |}];
      Expect_test_helpers_core.require_does_raise [%here] (fun () ->
        projection.parse_exn original);
      [%expect
        {|
        ("Error while parsing record field:"
          (error_message (
            "Did not recognize url during parsing! Expected path to match an expected [with_remaining_path] needed path! but\n              the needed path was not recognized!"
            (needed_path ())
            (components ((path (e)) (query ())))))
          (field_name foo)
          (unparseable_components ((path (e)) (query ()))))
        |}]
    ;;
  end)
;;

let%test_module "path parser after with_remaining_path" =
  (module struct
    module T = struct
      type t =
        { foo : unit
        ; bar : string
        }
      [@@deriving sexp_of, typed_fields]

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | Foo -> Parser.with_remaining_path [ "capybara" ] Parser.unit
        | Bar -> Parser.from_path Value_parser.string
      ;;

      module Path_order = Parser.Record.Path_order (Typed_field)

      let path_order = Path_order.T [ Foo; Bar ]
    end

    let unparseable_parser =
      let parser = Parser.Record.make (module T) in
      Versioned_parser.first_parser parser
    ;;

    let%expect_test "path parser after from remaining path parser passes the check." =
      Versioned_parser.check_ok_and_print_urls_or_errors unparseable_parser;
      [%expect
        {|
        Error with parser.
        ┌───────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
        │ Check name                                        │ Error message                                                                            │
        ├───────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
        │ Remaining path does not block other parsers check │ "Error! There cannot be path parsers inside of a [with_remaining_path] combinator! The r │
        │                                                   │ eason for this is that there won't be any values after that parser!"                     │
        └───────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
        |}]
    ;;

    let%expect_test "path parser after from remaining path parser passes the check." =
      let projection =
        Versioned_parser.eval_for_uri ~encoding_behavior:Correct unparseable_parser
      in
      let original = Uri.make ~path:"capybara/foo" () in
      print_endline (Uri.to_string original);
      [%expect {| capybara/foo |}];
      Expect_test_helpers_core.require_does_raise [%here] (fun () ->
        projection.parse_exn original);
      [%expect
        {|
        ("Error while parsing record field:"
          (error_message (
            "Did not recognize url during parsing! Expected path to match an expected [with_remaining_path] needed path! but\n              the needed path was not recognized!"
            (needed_path (capybara))
            (components ((path (capybara foo)) (query ())))))
          (field_name foo)
          (unparseable_components ((path (capybara foo)) (query ()))))
        |}]
    ;;
  end)
;;
