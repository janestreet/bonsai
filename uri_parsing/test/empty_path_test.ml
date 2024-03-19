open! Core
open Uri_parsing

(** These tests are designed to illustrate behavior when `/<string> paths
    are provided an empty string, and how that might conflict with a
    `/` path. We also test the analogous `/prefix` + `/prefix/<string>` cases. *)

let%test_module "Homepage and param if both use `with_prefix`" =
  (module struct
    module Url = struct
      type t =
        | Homepage
        | Param of string
      [@@deriving typed_variants, sexp, equal, compare]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Homepage -> Parser.with_prefix [] Parser.unit
        | Param -> Parser.with_prefix [] (Parser.from_path Value_parser.string)
      ;;
    end

    let parser = Parser.Variant.make (module Url)
    let versioned_parser = Versioned_parser.first_parser parser

    let%expect_test "This is ambiguous" =
      Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
      [%expect
        {|
        Error with parser.
        ┌─────────────────────────────────────────────────────────┬───────────────────────────────────────────────────────────────┐
        │ Check name                                              │ Error message                                                 │
        ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
        │ Ambiguous choices for picking variant constructor check │ ("Duplicate patterns found!"                                  │
        │                                                         │  (duplicate_patterns (((pattern ()) (needed_match Prefix))))) │
        └─────────────────────────────────────────────────────────┴───────────────────────────────────────────────────────────────┘
        |}]
    ;;
  end)
;;

let%test_module "Homepage and param" =
  (module struct
    module Url = struct
      type t =
        | Homepage
        | Param of string
      [@@deriving typed_variants, sexp, equal, compare]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Homepage -> Parser.end_of_path Parser.unit
        | Param -> Parser.with_prefix [] (Parser.from_path Value_parser.string)
      ;;
    end

    let parser = Parser.Variant.make (module Url)
    let versioned_parser = Versioned_parser.first_parser parser

    let%expect_test "Sanity_check" =
      Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
      [%expect
        {|
        URL parser looks good!
        ┌───────────┐
        │ All urls  │
        ├───────────┤
        │ /         │
        │ /<string> │
        └───────────┘
        |}]
    ;;

    let projection = Versioned_parser.eval_for_uri versioned_parser

    let%expect_test "Empty URL round trip" =
      let uri = Uri.empty in
      let { Parse_result.result = parsed; _ } = projection.parse_exn uri in
      print_s [%message (parsed : Url.t)];
      [%expect {| (parsed Homepage) |}];
      let unparsed = projection.unparse (Parse_result.create parsed) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed "") |}];
      [%test_eq: Uri_jane.t] uri unparsed
    ;;

    let%expect_test "Homepage roundtrip" =
      let url = Url.Homepage in
      let unparsed = projection.unparse (Parse_result.create url) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed "") |}];
      let { Parse_result.result = reparsed; _ } = projection.parse_exn unparsed in
      print_s [%message (reparsed : Url.t)];
      [%expect {| (reparsed Homepage) |}];
      [%test_eq: Url.t] url reparsed
    ;;

    let%expect_test "BUG: Param roundtrip" =
      let url = Url.Param "" in
      let unparsed = projection.unparse (Parse_result.create url) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed "") |}];
      let { Parse_result.result = reparsed; _ } = projection.parse_exn unparsed in
      print_s [%message (reparsed : Url.t)];
      [%expect {| (reparsed Homepage) |}];
      (* These should have been equal. *)
      Expect_test_patdiff.print_patdiff_s
        ([%sexp_of: Url.t] url)
        ([%sexp_of: Url.t] reparsed);
      [%expect {|
        -1,1 +1,1
        -|(Param "")
        +|Homepage
        |}]
    ;;
  end)
;;

let%test_module "Only Homepage" =
  (module struct
    module Url = struct
      type t = Homepage [@@deriving typed_variants, sexp, equal, compare]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Homepage -> Parser.end_of_path Parser.unit
      ;;
    end

    let parser = Parser.Variant.make (module Url)
    let versioned_parser = Versioned_parser.first_parser parser

    let%expect_test "Sanity_check" =
      Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
      [%expect
        {|
        URL parser looks good!
        ┌──────────┐
        │ All urls │
        ├──────────┤
        │ /        │
        └──────────┘
        |}]
    ;;

    let projection = Versioned_parser.eval_for_uri versioned_parser

    let%expect_test "Empty URL round trip" =
      let uri = Uri.empty in
      let { Parse_result.result = parsed; _ } = projection.parse_exn uri in
      print_s [%message (parsed : Url.t)];
      [%expect {| (parsed Homepage) |}];
      let unparsed = projection.unparse (Parse_result.create parsed) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed "") |}];
      [%test_eq: Uri_jane.t] uri unparsed
    ;;

    let%expect_test "Homepage roundtrip" =
      let url = Url.Homepage in
      let unparsed = projection.unparse (Parse_result.create url) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed "") |}];
      let { Parse_result.result = reparsed; _ } = projection.parse_exn unparsed in
      print_s [%message (reparsed : Url.t)];
      [%expect {| (reparsed Homepage) |}];
      [%test_eq: Url.t] url reparsed
    ;;
  end)
;;

let%test_module "Only Param" =
  (module struct
    module Url = struct
      type t = Param of string [@@deriving typed_variants, sexp, equal, compare]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Param -> Parser.with_prefix [] (Parser.from_path Value_parser.string)
      ;;
    end

    let parser = Parser.Variant.make (module Url)
    let versioned_parser = Versioned_parser.first_parser parser

    let%expect_test "Sanity_check" =
      Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
      [%expect
        {|
        URL parser looks good!
        ┌───────────┐
        │ All urls  │
        ├───────────┤
        │ /<string> │
        └───────────┘
        |}]
    ;;

    let projection = Versioned_parser.eval_for_uri versioned_parser

    let%expect_test "BUG: Empty URL round trip" =
      let uri = Uri.empty in
      Expect_test_helpers_base.require_does_raise [%here] (fun () ->
        projection.parse_exn uri);
      [%expect {| "Expected a value in path, but nothing was present" |}]
    ;;

    let%expect_test "BUG: Param roundtrip" =
      let url = Url.Param "" in
      let unparsed = projection.unparse (Parse_result.create url) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed "") |}];
      Expect_test_helpers_base.require_does_raise [%here] (fun () ->
        projection.parse_exn unparsed);
      [%expect {| "Expected a value in path, but nothing was present" |}]
    ;;
  end)
;;

let%test_module "Prefixed Home and param if both use `with_prefix`" =
  (module struct
    module Url = struct
      type t =
        | Homepage
        | Param of string
      [@@deriving typed_variants, sexp, equal, compare]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Homepage -> Parser.with_prefix [ "prefix" ] Parser.unit
        | Param -> Parser.with_prefix [ "prefix" ] (Parser.from_path Value_parser.string)
      ;;
    end

    let parser = Parser.Variant.make (module Url)
    let versioned_parser = Versioned_parser.first_parser parser

    let%expect_test "If there's a prefix, it's considered an ambiguity." =
      Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
      [%expect
        {|
        Error with parser.
        ┌─────────────────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────────────┐
        │ Check name                                              │ Error message                                                               │
        ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────┤
        │ Ambiguous choices for picking variant constructor check │ ("Duplicate patterns found!"                                                │
        │                                                         │  (duplicate_patterns (((pattern ((Match prefix))) (needed_match Prefix))))) │
        └─────────────────────────────────────────────────────────┴─────────────────────────────────────────────────────────────────────────────┘
        |}]
    ;;
  end)
;;

let%test_module "Prefixed home and param" =
  (module struct
    module Url = struct
      type t =
        | Homepage
        | Param of string
      [@@deriving typed_variants, sexp, equal, compare]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Homepage -> Parser.with_remaining_path [ "prefix" ] Parser.unit
        | Param -> Parser.with_prefix [ "prefix" ] (Parser.from_path Value_parser.string)
      ;;
    end

    include Url

    let parser = Parser.Variant.make (module Url)
    let versioned_parser = Versioned_parser.first_parser parser

    let%expect_test "If there's a prefix, it's considered an ambiguity." =
      Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
      [%expect
        {|
        URL parser looks good!
        ┌──────────────────┐
        │ All urls         │
        ├──────────────────┤
        │ /prefix          │
        │ /prefix/<string> │
        └──────────────────┘
        |}]
    ;;

    let projection = Versioned_parser.eval_for_uri versioned_parser

    let%expect_test "roundtrip" =
      let uri = Uri.of_string "prefix" in
      let { Parse_result.result = parsed; _ } = projection.parse_exn uri in
      print_s [%message (parsed : Url.t)];
      [%expect {| (parsed Homepage) |}];
      let unparsed = projection.unparse (Parse_result.create parsed) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed prefix) |}];
      [%test_eq: Uri_jane.t] uri unparsed;
      [%expect {| |}]
    ;;

    let%expect_test "roundtrip with ending slash" =
      let uri = Uri.of_string "prefix/" in
      let { Parse_result.result = parsed; _ } = projection.parse_exn uri in
      print_s [%message (parsed : Url.t)];
      [%expect {| (parsed (Param "")) |}];
      let unparsed = projection.unparse (Parse_result.create parsed) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed prefix/) |}];
      [%test_eq: Uri_jane.t] uri unparsed;
      [%expect {| |}]
    ;;
  end)
;;

let%test_module "Prefixed Only Homepage" =
  (module struct
    module Url = struct
      type t = Homepage [@@deriving typed_variants, sexp, equal, compare]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Homepage -> Parser.with_prefix [ "prefix" ] Parser.unit
      ;;
    end

    let parser = Parser.Variant.make (module Url)
    let versioned_parser = Versioned_parser.first_parser parser

    let%expect_test "Sanity_check" =
      Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
      [%expect
        {|
        URL parser looks good!
        ┌──────────┐
        │ All urls │
        ├──────────┤
        │ /prefix  │
        └──────────┘
        |}]
    ;;

    let projection = Versioned_parser.eval_for_uri versioned_parser

    let%expect_test "roundtrip with ending slash" =
      let uri = Uri.of_string "prefix/" in
      let { Parse_result.result = parsed; _ } = projection.parse_exn uri in
      print_s [%message (parsed : Url.t)];
      [%expect {| (parsed Homepage) |}];
      let unparsed = projection.unparse (Parse_result.create parsed) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed prefix) |}];
      Expect_test_patdiff.print_patdiff_s
        ([%sexp_of: Uri_jane.t] uri)
        ([%sexp_of: Uri_jane.t] unparsed);
      [%expect {|
        -1,1 +1,1
        -|prefix/
        +|prefix
        |}]
    ;;

    let%expect_test "roundtrip" =
      let uri = Uri.of_string "prefix" in
      let { Parse_result.result = parsed; _ } = projection.parse_exn uri in
      print_s [%message (parsed : Url.t)];
      [%expect {| (parsed Homepage) |}];
      let unparsed = projection.unparse (Parse_result.create parsed) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed prefix) |}];
      [%test_eq: Uri_jane.t] uri unparsed
    ;;
  end)
;;

let%test_module "Prefixed Only Param" =
  (module struct
    module Url = struct
      type t = Param of string [@@deriving typed_variants, sexp, equal, compare]

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Param -> Parser.with_prefix [ "prefix" ] (Parser.from_path Value_parser.string)
      ;;
    end

    let parser = Parser.Variant.make (module Url)
    let versioned_parser = Versioned_parser.first_parser parser

    let%expect_test "Sanity_check" =
      Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
      [%expect
        {|
        URL parser looks good!
        ┌──────────────────┐
        │ All urls         │
        ├──────────────────┤
        │ /prefix/<string> │
        └──────────────────┘
        |}]
    ;;

    let projection = Versioned_parser.eval_for_uri versioned_parser

    let%expect_test "roundtrip with ending slash" =
      let uri = Uri.of_string "prefix/" in
      let { Parse_result.result = parsed; _ } = projection.parse_exn uri in
      print_s [%message (parsed : Url.t)];
      [%expect {| (parsed (Param "")) |}];
      let unparsed = projection.unparse (Parse_result.create parsed) in
      print_s [%message (unparsed : Uri_jane.t)];
      [%expect {| (unparsed prefix/) |}];
      [%test_eq: Uri_jane.t] uri unparsed;
      [%expect {| |}]
    ;;

    let%expect_test "cannot parse without ending slash" =
      let uri = Uri.of_string "prefix" in
      Expect_test_helpers_base.require_does_raise [%here] (fun () ->
        projection.parse_exn uri);
      [%expect {| "Expected a value in path, but nothing was present" |}]
    ;;
  end)
;;
