open! Core
open Base_quickcheck

module Parsers = struct
  include Uri_parsing.Parser
  include Uri_parsing.Value_parser
end

open Uri_parsing
module Path_order = Parser.Record.Path_order

let diff_queries a b =
  let sexp_of_query query =
    String.Map.sexp_of_t (List.sexp_of_t String.sexp_of_t) query
  in
  Expect_test_patdiff.print_patdiff_s (sexp_of_query a) (sexp_of_query b)
;;

let diff_paths a b =
  let sexp_of_query query = List.sexp_of_t String.sexp_of_t query in
  Expect_test_patdiff.print_patdiff_s (sexp_of_query a) (sexp_of_query b)
;;

let expect_output_and_identity_roundtrip
      ?(expect_diff = fun () -> ())
      ~path
      ~query
      ~sexp_of_t
      ~expect
      (projection : (Components.t, 'a Parse_result.t) Projection.t)
  =
  let result = projection.parse_exn { query; path } in
  print_s (sexp_of_t result.result);
  expect ();
  let { Components.query = unparsed_query; path = unparsed_path } =
    projection.unparse result
  in
  diff_paths path unparsed_path;
  diff_queries query unparsed_query;
  expect_diff ()
;;

let show_structure parser =
  Parser.check_ok_and_print_urls_or_errors parser;
  print_s (Parser.sexp_of_t parser)
;;

module Simple_record = struct
  type t =
    { foo : int
    ; bar : float
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parsers in
    function
    | Foo -> from_query_required int
    | Bar -> from_query_required float
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.T []
end

module Many_query = struct
  type t =
    { ints : int list
    ; floats : float list
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parsers in
    function
    | Ints -> from_query_many int
    | Floats -> from_query_many float
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.T []
end

module Game_id = struct
  type t = int [@@deriving sexp, compare, equal]

  let to_int = Fn.id
  let of_int = Fn.id
end

module Serializable = struct
  type t =
    { a : int
    ; b : float
    ; c : string
    }
  [@@deriving bin_io, sexp, equal]
end

module All_primitives_query = struct
  module Name = struct
    (* Type that satisfies stringable. *)
    type t = string [@@deriving sexp, compare, equal]

    let to_string = Fn.id
    let of_string = Fn.id
  end

  module Point = struct
    (* Type that satisfies sexpable. *)
    type t =
      { x : int
      ; y : int
      }
    [@@deriving sexp, compare, equal]
  end

  type t =
    { int_field : int
    ; float_field : float
    ; string_field : string
    ; bool_field : bool
    ; stringable_field : Name.t
    ; sexpable_field : Point.t
    ; binable_field : Serializable.t
    ; sexpable_via_base64_field : Serializable.t
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parsers in
    function
    | Int_field -> from_query_required int
    | Float_field -> from_query_required float
    | String_field -> from_query_required string
    | Bool_field -> from_query_required bool
    | Stringable_field -> stringable (module Name) |> from_query_required
    | Sexpable_field -> sexpable (module Point) |> from_query_required
    | Binable_field -> binable_via_base64 (module Serializable) |> from_query_required
    | Sexpable_via_base64_field ->
      base64_encoded (sexpable (module Serializable)) |> from_query_required
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.T []
end

let%expect_test "all primitives parser" =
  let module Query = All_primitives_query in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query =
    String.Map.of_alist_exn
      [ "int_field", [ "10" ]
      ; "float_field", [ "1.25" ]
      ; "string_field", [ "hi!" ]
      ; "bool_field", [ "true" ]
      ; "stringable_field", [ "Bonsai" ]
      ; "sexpable_field", [ "((x 1) (y 2))" ]
      ; ( "binable_field"
        , [ Bin_prot.Writer.to_string Serializable.bin_writer_t { a = 1; b = 2.; c = "3" }
            |> Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
          ] )
      ; ( "sexpable_via_base64_field"
        , [ Base64.encode_string
              ~pad:false
              ~alphabet:Base64.uri_safe_alphabet
              (Sexp.to_string (Serializable.sexp_of_t { a = 1; b = 2.; c = "3" }))
          ] )
      ]
  in
  print_s [%message (query : string list String.Map.t)];
  [%expect
    {|
    (query
     ((binable_field (AQAAAAAAAABAATM)) (bool_field (true)) (float_field (1.25))
      (int_field (10)) (sexpable_field ("((x 1) (y 2))"))
      (sexpable_via_base64_field (KChhIDEpKGIgMikoYyAzKSk)) (string_field (hi!))
      (stringable_field (Bonsai)))) |}];
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
   ((int_field 10) (float_field 1.25) (string_field hi!) (bool_field true)
    (stringable_field Bonsai) (sexpable_field ((x 1) (y 2)))
    (binable_field ((a 1) (b 2) (c 3)))
    (sexpable_via_base64_field ((a 1) (b 2) (c 3)))) |}]);
  show_structure parser;
  [%expect
    {|
     URL parser looks good!
     ┌──────────────────────────────────────────────────────────────────────────────────────────┐
     │ All urls                                                                                 │
     ├──────────────────────────────────────────────────────────────────────────────────────────┤
     │ /?binable_field=<base64<binable>>&bool_field=<bool>&float_field=<float>&int_field=<int>& │
     │ sexpable_field=<sexpable>&sexpable_via_base64_field=<base64<sexpable>>&string_field=<str │
     │ ing>&stringable_field=<string>                                                           │
     └──────────────────────────────────────────────────────────────────────────────────────────┘

     (Record
      (label_declarations
       ((binable_field
         (From_query_required (value_parser (Base64_encoded Binable_via_base64))))
        (bool_field (From_query_required (value_parser Bool)))
        (float_field (From_query_required (value_parser Float)))
        (int_field (From_query_required (value_parser Int)))
        (sexpable_field (From_query_required (value_parser Sexpable)))
        (sexpable_via_base64_field
         (From_query_required (value_parser (Base64_encoded Sexpable))))
        (string_field (From_query_required (value_parser String)))
        (stringable_field (From_query_required (value_parser Stringable)))))
      (path_order ())) |}]
;;

let%expect_test "nested query parser" =
  let module Query = struct
    type t =
      { child : Simple_record.t
      ; something_else : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Parsers in
      function
      | Child -> Record.make (module Simple_record)
      | Something_else -> from_query_required int
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:
      (String.Map.of_alist_exn
         [ "child.foo", [ "10" ]; "something_else", [ "2" ]; "child.bar", [ "5.5" ] ])
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () ->
      [%expect {|
      ((child ((foo 10) (bar 5.5))) (something_else 2)) |}]);
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────┐
    │ All urls                                                 │
    ├──────────────────────────────────────────────────────────┤
    │ /?child.bar=<float>&child.foo=<int>&something_else=<int> │
    └──────────────────────────────────────────────────────────┘

    (Record
     (label_declarations
      ((child
        (Record
         (label_declarations
          ((bar (From_query_required (value_parser Float)))
           (foo (From_query_required (value_parser Int)))))
         (path_order ())))
       (something_else (From_query_required (value_parser Int)))))
     (path_order ())) |}]
;;

let%expect_test "missing field exn" =
  let parser = Parser.Record.make (module Simple_record) in
  let projection = Parser.eval parser in
  (* field "bar" is missing. *)
  let query = String.Map.of_alist_exn [ "foo", [ "1" ] ] in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path = [] });
  (* Error message shows the name of the missing field. *)
  [%expect {|
    ("Uri_parsing.Parser.Missing_key(\"bar\")") |}]
;;

let%expect_test "field fails to parse" =
  let parser = Parser.Record.make (module Simple_record) in
  let projection = Parser.eval parser in
  let query = String.Map.of_alist_exn [ "foo", [ "1" ]; "bar", [ "not a float" ] ] in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path = [] });
  [%expect
    {|
   ("Error while parsing record field:"
     (error_message (Invalid_argument "Float.of_string not a float"))
     (field_name bar)
     (unparseable_components ((path ()) (query ((bar ("not a float"))))))) |}]
;;

let%expect_test "many parser" =
  let parser = Parser.Record.make (module Many_query) in
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:
      (String.Map.of_alist_exn
         [ "ints", [ "1"; "2"; "3" ]; "floats", [ "3.1"; "1.2"; "2.3" ] ])
    ~sexp_of_t:Many_query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      ((ints (1 2 3)) (floats (3.1 1.2 2.3))) |}]);
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────────────────┐
    │ All urls                                        │
    ├─────────────────────────────────────────────────┤
    │ /?floats=<multiple<float>>&ints=<multiple<int>> │
    └─────────────────────────────────────────────────┘

    (Record
     (label_declarations
      ((floats (From_query_many (value_parser Float)))
       (ints (From_query_many (value_parser Int)))))
     (path_order ())) |}]
;;

let%expect_test "from_query_many can parse empty lists from missing query fields" =
  let module Url = struct
    type t = { strings : string list } [@@deriving sexp, equal, typed_fields]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Strings -> Parser.from_query_many Value_parser.string
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Url) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────┐
    │ All urls                     │
    ├──────────────────────────────┤
    │ /?strings=<multiple<string>> │
    └──────────────────────────────┘ |}];
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {| |}])
    projection
    ~path:[]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
   ((strings ())) |}])
;;

let%expect_test "from_query_many can parse empty list options from missing query fields" =
  let module Url = struct
    type t = { strings : string list option } [@@deriving sexp, equal, typed_fields]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Strings ->
        Parser.optional_query_fields (Parser.from_query_many Value_parser.string)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Url) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────┐
    │ All urls                     │
    ├──────────────────────────────┤
    │ /                            │
    │ /?strings=<multiple<string>> │
    └──────────────────────────────┘ |}];
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {| |}])
    projection
    ~path:[]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> (* Some []*)
      [%expect {|
   ((strings (()))) |}])
;;

let%expect_test "many parser - single value fails => entire parse fails" =
  let parser = Parser.Record.make (module Many_query) in
  let projection = Parser.eval parser in
  let query =
    String.Map.of_alist_exn
      [ "ints", [ "1"; "2"; "3.1" (* <-- Not an int! *) ]
      ; "floats", [ "3.1"; "1.2"; "2.3" ]
      ]
  in
  (* Parsing query... *)
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path = [] });
  [%expect
    {|
   ("Error while parsing record field:"
     (error_message (Failure "Int.of_string: \"3.1\""))
     (field_name ints)
     (unparseable_components (
       (path ())
       (query (
         (floats (3.1 1.2 2.3))
         (ints   (1   2   3.1))))))) |}]
;;

let%expect_test "many parser - works on empty list" =
  let parser = Parser.Record.make (module Many_query) in
  let projection = Parser.eval parser in
  let query = String.Map.of_alist_exn [ "ints", []; "floats", [ "3.1"; "1.2"; "2.3" ] ] in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query
    ~sexp_of_t:Many_query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      ((ints ()) (floats (3.1 1.2 2.3))) |}]);
  [%expect
    {|
    -1,1 +1,1
    -|((floats (3.1 1.2 2.3)) (ints ()))
    +|((floats (3.1 1.2 2.3))) |}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────────────────┐
    │ All urls                                        │
    ├─────────────────────────────────────────────────┤
    │ /?floats=<multiple<float>>&ints=<multiple<int>> │
    └─────────────────────────────────────────────────┘

    (Record
     (label_declarations
      ((floats (From_query_many (value_parser Float)))
       (ints (From_query_many (value_parser Int)))))
     (path_order ())) |}]
;;

module Many1_query = struct
  type t =
    { ints : int list
    ; floats : float list
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parsers in
    function
    | Ints -> from_query_many_at_least_1 int (* <-- many1!  *)
    | Floats -> from_query_many float
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.T []
end

let%expect_test "many1 parser - fails on empty list" =
  let parser = Parser.Record.make (module Many1_query) in
  let projection = Parser.eval parser in
  let query = String.Map.of_alist_exn [ "ints", []; "floats", [ "3.1"; "1.2"; "2.3" ] ] in
  (* Parsing query... *)
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path = [] });
  [%expect
    {|
   ("Error while parsing record field:"
     (error_message
      "Error! [from_query_many_at_least_1] expected at least one element in the list.")
     (field_name ints)
     (unparseable_components (
       (path ()) (query ((floats (3.1 1.2 2.3)) (ints ())))))) |}]
;;

let%expect_test "many1 parser - works on non-empty list" =
  let parser = Parser.Record.make (module Many1_query) in
  let projection = Parser.eval parser in
  (* Parsing query... *)
  let query =
    String.Map.of_alist_exn
      [ "ints", [ "1"; "2"; "3" ]; "floats", [ "3.1"; "1.2"; "2.3" ] ]
  in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query
    ~sexp_of_t:Many1_query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      ((ints (1 2 3)) (floats (3.1 1.2 2.3))) |}]);
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────────────────┐
    │ All urls                                        │
    ├─────────────────────────────────────────────────┤
    │ /?floats=<multiple<float>>&ints=<multiple<int>> │
    └─────────────────────────────────────────────────┘

    (Record
     (label_declarations
      ((floats (From_query_many (value_parser Float)))
       (ints (Project (From_query_many (value_parser Int))))))
     (path_order ())) |}]
;;

let%expect_test "Value parser project" =
  let module Username = struct
    type t = string [@@deriving sexp, compare, equal]

    let to_string = Fn.id
    let of_string = Fn.id
  end
  in
  let module Query = struct
    type t =
      { game_id : Game_id.t
      ; players : Username.t list
      ; watchers : Username.t list
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Parsers in
      function
      | Game_id ->
        project int ~parse_exn:Game_id.of_int ~unparse:Game_id.to_int
        |> from_query_required
      | Players ->
        (* Combining many(project) *)
        from_query_many
          (project string ~parse_exn:Username.of_string ~unparse:Username.to_string)
      | Watchers ->
        (* Combining project(many) *)
        Parser.project
          (from_query_many string)
          ~parse_exn:(List.map ~f:Username.of_string)
          ~unparse:(List.map ~f:Username.to_string)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query =
    String.Map.of_alist_exn
      [ "game_id", [ "10" ]; "players", [ "foo"; "bar" ]; "watchers", [ "baz"; "bam" ] ]
  in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () ->
      [%expect {|
        ((game_id 10) (players (foo bar)) (watchers (baz bam))) |}]);
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /?game_id=<project<int>>&players=<multiple<project<string>>>&watchers=<multiple<string>> │
    └──────────────────────────────────────────────────────────────────────────────────────────┘

    (Record
     (label_declarations
      ((game_id (From_query_required (value_parser (Project Int))))
       (players (From_query_many (value_parser (Project String))))
       (watchers (Project (From_query_many (value_parser String))))))
     (path_order ())) |}]
;;

let%expect_test "project parse_exn fails" =
  let module Game_id = struct
    type t = int [@@deriving sexp, compare, equal]

    let to_int = Fn.id
    let of_int _ = raise_s (Sexp.Atom "Can't parse!")
  end
  in
  let module Query = struct
    type t = { game_id : Game_id.t } [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Parsers in
      function
      | Game_id ->
        project int ~parse_exn:Game_id.of_int ~unparse:Game_id.to_int
        |> from_query_required
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query = String.Map.of_alist_exn [ "game_id", [ "10" ] ] in
  (* Parsing query... *)
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path = [] });
  [%expect
    {|
   ("Error while parsing record field:"
     (error_message "Can't parse!")
     (field_name    game_id)
     (unparseable_components ((path ()) (query ((game_id (10))))))) |}]
;;

let%expect_test "Field parser project" =
  let module Query = struct
    type t =
      { foo : Game_id.t
      ; bar : Game_id.t option
      ; baz : Game_id.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Parsers in
      function
      | Foo ->
        Parser.project
          (from_query_required int)
          ~parse_exn:Game_id.of_int
          ~unparse:Game_id.to_int
      | Bar ->
        Parser.project
          (from_query_optional int)
          ~parse_exn:Game_id.of_int
          ~unparse:Game_id.to_int
      | Baz ->
        Parser.project
          (from_query_optional_with_default ~equal:Int.equal int ~default:42)
          ~parse_exn:Game_id.of_int
          ~unparse:Game_id.to_int
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query = String.Map.of_alist_exn [ "foo", [ "10" ]; "bar", [ "12" ] ] in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      ((foo 10) (bar (12)) (baz 42)) |}]);
  (* Diff between old query and new query (default field is included) *)
  [%expect {| |}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────────────────────┐
    │ All urls                                            │
    ├─────────────────────────────────────────────────────┤
    │ /?bar=<optional<int>>&baz=<optional<int>>&foo=<int> │
    └─────────────────────────────────────────────────────┘

    (Record
     (label_declarations
      ((bar (Project (From_query_optional (value_parser Int))))
       (baz (Project (From_query_optional_with_default (value_parser Int))))
       (foo (Project (From_query_required (value_parser Int))))))
     (path_order ())) |}]
;;

module Default_when_missing_query = struct
  type t =
    { foo : int
    ; bar : int
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parsers in
    function
    | Foo -> from_query_optional_with_default ~equal:Int.equal int ~default:100
    | Bar -> from_query_optional_with_default ~equal:Int.equal int ~default:123
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.T []
end

let%expect_test "default missing field" =
  let parser = Parser.Record.make (module Default_when_missing_query) in
  let projection = Parser.eval parser in
  (* field "bar" is missing, but "foo" is present. *)
  let query = String.Map.of_alist_exn [ "foo", [ "42" ] ] in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query
    ~sexp_of_t:Default_when_missing_query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      ((foo 42) (bar 123)) |}]);
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────────────────────────┐
    │ All urls                                  │
    ├───────────────────────────────────────────┤
    │ /?bar=<optional<int>>&foo=<optional<int>> │
    └───────────────────────────────────────────┘

    (Record
     (label_declarations
      ((bar (From_query_optional_with_default (value_parser Int)))
       (foo (From_query_optional_with_default (value_parser Int)))))
     (path_order ())) |}]
;;

let%expect_test "default missing field fails if underlying parser fails." =
  let parser = Parser.Record.make (module Default_when_missing_query) in
  let projection = Parser.eval parser in
  (* fields "bar" and "baz" are missing; "foo" is present, but "foo" fails to parse. *)
  let query = String.Map.of_alist_exn [ "foo", [ "not an int!" ] ] in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path = [] });
  [%expect
    {|
    ("Error while parsing record field:"
      (error_message (Failure "Int.of_string: \"not an int!\""))
      (field_name foo)
      (unparseable_components ((path ()) (query ((foo ("not an int!"))))))) |}]
;;

let%expect_test "field is present, but has no values" =
  let parser = Parser.Record.make (module Default_when_missing_query) in
  let projection = Parser.eval parser in
  (* fields "bar" and "baz" are missing; "foo" is present, but "foo" fails to parse. *)
  let query = String.Map.of_alist_exn [ "foo", [] ] in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path = [] });
  [%expect
    {|
    ("Error while parsing record field:"
      (error_message "Expected a value in query field, but nothing was present")
      (field_name foo)
      (unparseable_components ((path ()) (query ((foo ())))))) |}]
;;

module Fallback_query = struct
  type t =
    { foo : int
    ; bar : int
    ; qux : int list
    ; game_id : Game_id.t
    ; game_ids : Game_id.t list
    ; game_ids2 : Game_id.t list
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parsers in
    function
    | Foo -> fallback int ~fallback:42 |> from_query_required
    | Bar -> fallback int ~fallback:123 |> from_query_required
    | Qux -> from_query_many (fallback int ~fallback:321)
    | Game_id ->
      fallback
        (project
           int
           ~parse_exn:(fun _ -> raise_s (Sexp.Atom "error!"))
           ~unparse:Game_id.to_int)
        ~fallback:(Game_id.of_int 42)
      |> from_query_required
    | Game_ids ->
      from_query_many
        (project
           (fallback int ~fallback:100)
           ~parse_exn:Game_id.to_int
           ~unparse:Game_id.to_int)
    | Game_ids2 ->
      fallback
        (project
           int
           ~parse_exn:(fun _ -> raise_s (Sexp.Atom "error!"))
           ~unparse:Game_id.to_int)
        ~fallback:(Game_id.of_int 42)
      |> from_query_many
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.T []
end

let%expect_test "fallback proper behavior" =
  let module Query = Fallback_query in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query =
    String.Map.of_alist_exn
      [ "foo", [ "1" ]
      ; "bar", [ "not an int!" ]
      ; "qux", [ "1"; "hi!"; "3" ]
      ; "game_id", [ "not a game id!" ]
      ; "game_ids", [ "1"; "2"; "3"; "hi"; "5"; "6" ]
      ; "game_ids2", [ "1"; "2"; "3"; "hi"; "5"; "6" ]
      ]
  in
  let result = projection.parse_exn { query; path = [] } in
  print_s (Query.sexp_of_t result.result);
  [%expect
    {|
    ((foo 1) (bar 123) (qux (1 321 3)) (game_id 42) (game_ids (1 2 3 100 5 6))
     (game_ids2 (42 42 42 42 42 42))) |}];
  let { Components.query = unparsed; _ } = projection.unparse result in
  diff_queries query unparsed;
  [%expect
    {|
    -1,6 +1,6
    -|((bar     ("not an int!"))
    +|((bar     (123))
       (foo     (1))
    -| (game_id ("not a game id!"))
    +| (game_id (42))
    -| (game_ids (1 2 3 hi 5 6))
    +| (game_ids (1 2 3 100 5 6))
    -| (game_ids2 (1 2 3 hi 5 6))
    +| (game_ids2 (42 42 42 42 42 42))
    -| (qux (1 hi! 3)))
    +| (qux (1 321 3))) |}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /?bar=<fallback<int>>&foo=<fallback<int>>&game_id=<fallback<project<int>>>&game_ids2=<mu │
    │ ltiple<fallback<project<int>>>>&game_ids=<multiple<project<fallback<int>>>>&qux=<multipl │
    │ e<fallback<int>>>                                                                        │
    └──────────────────────────────────────────────────────────────────────────────────────────┘

    (Record
     (label_declarations
      ((bar (From_query_required (value_parser (Fallback Int))))
       (foo (From_query_required (value_parser (Fallback Int))))
       (game_id (From_query_required (value_parser (Fallback (Project Int)))))
       (game_ids (From_query_many (value_parser (Project (Fallback Int)))))
       (game_ids2 (From_query_many (value_parser (Fallback (Project Int)))))
       (qux (From_query_many (value_parser (Fallback Int))))))
     (path_order ())) |}]
;;

let%expect_test "Fallback does not fix missing fields." =
  let module Query = Fallback_query in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  (* Field "bar" is missing. *)
  let query =
    String.Map.of_alist_exn
      [ "foo", [ "1" ]; "baz", [ "1"; "hi!"; "3" ]; "qux", [ "1"; "hi!"; "3" ] ]
  in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path = [] });
  [%expect {|
    ("Uri_parsing.Parser.Missing_key(\"bar\")") |}]
;;

let%expect_test "Both fallback and default may have different values" =
  let module Query = struct
    type t = { foo : int } [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Parsers in
      function
      | Foo ->
        from_query_optional_with_default
          ~equal:Int.equal
          (fallback int ~fallback:200)
          ~default:100
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let failing_query = String.Map.of_alist_exn [ "foo", [ "not an int" ] ] in
  let missing_query = String.Map.empty in
  let result = projection.parse_exn { query = failing_query; path = [] } in
  print_s (Query.sexp_of_t result.result);
  [%expect {|
    ((foo 200)) |}];
  let { Components.query = unparsed; _ } = projection.unparse result in
  diff_queries failing_query unparsed;
  [%expect {|
    -1,1 +1,1
    -|((foo ("not an int")))
    +|((foo (200))) |}];
  let result = projection.parse_exn { query = missing_query; path = [] } in
  print_s (Query.sexp_of_t result.result);
  [%expect {|
    ((foo 100)) |}];
  let { Components.query = unparsed; _ } = projection.unparse result in
  diff_queries missing_query unparsed;
  [%expect {| |}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────┐
    │ All urls                        │
    ├─────────────────────────────────┤
    │ /?foo=<optional<fallback<int>>> │
    └─────────────────────────────────┘

    (Record
     (label_declarations
      ((foo (From_query_optional_with_default (value_parser (Fallback Int))))))
     (path_order ())) |}]
;;

let%expect_test "optional field" =
  let module Query = struct
    type t =
      { foo : int option
      ; bar : float option
      ; baz : int list option
      ; qux : Game_id.t list option
      ; bam : Game_id.t list option
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Parsers in
      function
      | Foo -> from_query_optional int
      | Bar -> from_query_optional float
      | Baz ->
        Parser.project
          (from_query_many int)
          ~parse_exn:(function
            | [] -> None
            | x -> Some x)
          ~unparse:(function
            | None -> []
            | Some x -> x)
      | Qux ->
        Parser.project
          (from_query_many
             (project int ~parse_exn:Game_id.of_int ~unparse:Game_id.to_int))
          ~parse_exn:(function
            | [] -> None
            | x -> Some x)
          ~unparse:(function
            | None -> []
            | Some x -> x)
      | Bam ->
        Parser.project
          (from_query_many int)
          ~parse_exn:(function
            | [] -> None
            | x -> Some x)
          ~unparse:(function
            | None -> []
            | Some x -> x)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query =
    String.Map.of_alist_exn
      [ "qux", [ "1"; "2"; "3" ]; "bam", [ "1"; "2"; "3" ]; "baz", [] ]
  in
  let result = projection.parse_exn { query; path = [] } in
  print_s (Query.sexp_of_t result.result);
  [%expect {|
    ((foo ()) (bar ()) (baz ()) (qux ((1 2 3))) (bam ((1 2 3)))) |}];
  let { Components.query = unparsed; _ } = projection.unparse result in
  diff_queries query unparsed;
  [%expect
    {|
    -1,1 +1,2
    -|((bam (1 2 3)) (baz ()) (qux (1 2 3)))
    +|((bam (1 2 3))
    +| (qux (1 2 3))) |}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /?bam=<multiple<int>>&bar=<optional<float>>&baz=<multiple<int>>&foo=<optional<int>>&qux= │
    │ <multiple<project<int>>>                                                                 │
    └──────────────────────────────────────────────────────────────────────────────────────────┘

    (Record
     (label_declarations
      ((bam (Project (From_query_many (value_parser Int))))
       (bar (From_query_optional (value_parser Float)))
       (baz (Project (From_query_many (value_parser Int))))
       (foo (From_query_optional (value_parser Int)))
       (qux (Project (From_query_many (value_parser (Project Int)))))))
     (path_order ())) |}]
;;

let%test_module "quickcheck" =
  (module struct
    module Query = struct
      module Player_id = struct
        type t = int [@@deriving sexp, equal, quickcheck]

        let to_string = Int.to_string
        let of_string = Int.of_string
        let to_int = Fn.id
        let of_int = Fn.id
      end

      module Point = struct
        type t =
          { x : int
          ; y : int
          }
        [@@deriving typed_fields, sexp, equal, quickcheck]

        let parser_for_field : type a. a Typed_field.t -> a Parser.t =
          let open Parsers in
          function
          | X -> from_query_required int
          | Y -> from_query_required int
        ;;

        module Path_order = Path_order (Typed_field)

        let path_order = Path_order.T []
      end

      type t =
        | Main of
            { int_field : int
            ; int_list_field : int list
            ; float_list_field :
                (float list
                 [@quickcheck.generator Generator.list Generator.float_without_nan])
            ; sexpable : Point.t
            ; stringable : Player_id.t
            ; nested : Point.t
            ; list_projection : Player_id.t list
            ; int_list_with_fallback : int list
            ; at_least_one_int :
                (int list[@quickcheck.generator Generator.list_non_empty Generator.int])
            ; path_int : int
            ; path_bool_without_name : bool
            ; remaining_path : int list
            } [@typed_fields]
        | Secondary of int
      [@@deriving typed_variants, sexp, equal, quickcheck]

      module Anon_main = struct
        let fallback_value = 100

        module Typed_field =
          Typed_variant.Typed_variant_anonymous_records.Typed_field_of_main

        let parser_for_field : type a. a Typed_field.t -> a Parser.t =
          let open Parsers in
          function
          | Int_field -> from_query_required int
          | Int_list_field -> int |> from_query_many
          | Float_list_field -> float |> from_query_many
          | Sexpable -> sexpable (module Point) |> from_query_required
          | Stringable -> stringable (module Player_id) |> from_query_required
          | Nested -> Parser.Record.make (module Point)
          | List_projection ->
            project int ~parse_exn:Player_id.of_int ~unparse:Player_id.to_int
            |> from_query_many
          | Int_list_with_fallback ->
            from_query_many (fallback int ~fallback:fallback_value)
          | At_least_one_int -> from_query_many_at_least_1 int
          | Path_int -> with_prefix [ "path_int" ] (from_path int)
          | Path_bool_without_name -> with_prefix [] (from_path bool)
          | Remaining_path -> with_prefix [ "remaining" ] (from_remaining_path int)
        ;;

        let path_generator =
          let int_generator = Generator.int in
          let bool_generator = Generator.bool in
          let remaining_generator = Generator.int |> Generator.list in
          Generator.map3
            int_generator
            bool_generator
            remaining_generator
            ~f:(fun int_ bool_ remaining ->
              [ "main"; "path_int"; Int.to_string int_; Bool.to_string bool_; "remaining" ]
              @ List.map remaining ~f:Int.to_string)
        ;;

        module Path_order = Path_order (Typed_field)

        let path_order = Path_order.T [ Path_int; Path_bool_without_name; Remaining_path ]

        let generator_for_field
          : type a. a Typed_field.t -> string list Generator.t option
          =
          let open Generator in
          function
          | Int_field ->
            Some (map quickcheck_generator_int ~f:(fun x -> [ Int.to_string x ]))
          | Int_list_field -> Some (map quickcheck_generator_int ~f:Int.to_string |> list)
          | Float_list_field ->
            (* No roundtrip guarantees on nans. *)
            Some
              (map
                 (list quickcheck_generator_float)
                 ~f:
                   (List.filter_map ~f:(fun x ->
                      match Float.is_nan x with
                      | true -> None
                      | false -> Some (Float.to_string x))))
          | Sexpable ->
            Some
              (map
                 (map2 size size ~f:(fun a b -> a, b))
                 ~f:(fun (x, y) ->
                   let s = Point.sexp_of_t { Point.x; y } |> Sexp.to_string in
                   [ s ]))
          | Stringable ->
            Some (map quickcheck_generator_int ~f:(fun x -> [ Int.to_string x ]))
          | Nested -> None
          | List_projection -> Some (map quickcheck_generator_int ~f:Int.to_string |> list)
          | Int_list_with_fallback ->
            Some
              (either
                 (map quickcheck_generator_int ~f:Int.to_string)
                 quickcheck_generator_string
               |> map ~f:(function First x | Second x -> x)
               |> list)
          | At_least_one_int ->
            Some (map quickcheck_generator_int ~f:Int.to_string |> list_non_empty)
          | Path_int -> None
          | Path_bool_without_name -> None
          | Remaining_path -> None
        ;;

        let string_list_equal = List.equal String.equal
        let is_int_string possible_int = Int.of_string_opt possible_int |> Option.is_some

        let equal_values_for_field
          : type a. a Typed_field.t -> (string list -> string list -> bool) option
          = function
            | Int_field -> Some string_list_equal
            | Int_list_field -> Some string_list_equal
            | Float_list_field -> Some string_list_equal
            | Sexpable -> Some string_list_equal
            | Stringable -> Some string_list_equal
            | Nested -> None
            | List_projection -> Some string_list_equal
            | Int_list_with_fallback ->
              (* This equality function is weird, but it makes

                 ["1"; "not an int"; "3"; "4"] equal to ["1"; "100"; "3"; "4"]

                 which is useful to make check that the fallback did its work!
              *)
              Some
                (List.equal (fun a b ->
                   match is_int_string a with
                   | true ->
                     (* This fixes strings that happen to be ints with leading 0's (e.g. "05") *)
                     String.equal (Int.of_string a |> Int.to_string) b
                   | false -> String.equal (Int.to_string fallback_value) b))
            | At_least_one_int -> Some string_list_equal
            | Path_int -> None
            | Path_bool_without_name -> None
            | Remaining_path -> None
        ;;
      end

      let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
        | Main ->
          Parser.with_prefix
            [ "main" ]
            (Parser.Record.make ~namespace:[] (module Anon_main))
        | Secondary ->
          Parser.with_prefix [ "secondary" ] (Parser.from_path Value_parser.int)
      ;;
    end

    let%quick_test "round-trip generated Query.t" =
      fun (t : Query.t) ->
      let parser = Parser.Variant.make ~namespace:[] (module Query) in
      let projection = Parser.eval parser in
      let { Components.query = serialized; path } =
        projection.unparse { Parse_result.result = t; remaining = Components.empty }
      in
      let result = projection.parse_exn { query = serialized; path } in
      assert (Query.equal t result.result)
    ;;

    let generator =
      let query_generator =
        let open Generator in
        let query_without_nested =
          List.fold
            Query.Anon_main.Typed_field.Packed.all
            ~init:(Generator.return String.Map.empty)
            ~f:(fun acc { f = T f } ->
              match Query.Anon_main.generator_for_field f with
              | None -> acc
              | Some generator ->
                Generator.map2 acc generator ~f:(fun acc field_generator ->
                  Map.set
                    acc
                    ~key:(Query.Anon_main.Typed_field.name f)
                    ~data:field_generator))
        in
        let x_generator = Generator.int >>| fun x -> [ Int.to_string x ] in
        let y_generator = Generator.int >>| fun x -> [ Int.to_string x ] in
        Generator.map3 query_without_nested x_generator y_generator ~f:(fun query x y ->
          query |> Map.set ~key:"nested.x" ~data:x |> Map.set ~key:"nested.y" ~data:y)
      in
      let query_with_random_extra_fields =
        let random_value = quickcheck_generator_string |> Generator.list_non_empty in
        let random_field_names =
          quickcheck_generator_string |> Generator.list_non_empty
        in
        let open Generator.Let_syntax in
        let%bind field_names = random_field_names in
        let%bind query_generator = query_generator in
        let%map random_value = random_value in
        List.fold field_names ~init:query_generator ~f:(fun acc field_name ->
          match Map.mem (acc : _ String.Map.t) field_name with
          | true -> acc
          | false -> Map.set (acc : _ String.Map.t) ~key:field_name ~data:random_value)
      in
      let main_query_and_path =
        Generator.map2
          query_with_random_extra_fields
          Query.Anon_main.path_generator
          ~f:(fun query path -> query, path)
      in
      let secondary_generator =
        Generator.map Generator.int ~f:(fun index ->
          String.Map.empty, [ "secondary"; Int.to_string index ])
      in
      Generator.bind Generator.bool ~f:(fun is_main ->
        if is_main then main_query_and_path else secondary_generator)
    ;;

    let maps_equal original unparsed =
      let equal_values_for_field =
        List.fold
          Query.Anon_main.Typed_field.Packed.all
          ~init:String.Map.empty
          ~f:(fun acc { f = T f } ->
            let key = Query.Anon_main.Typed_field.name f in
            let equal = Query.Anon_main.equal_values_for_field f in
            match equal with
            | None -> acc
            | Some equal -> Map.set acc ~key ~data:equal)
      in
      Map.for_alli
        (unparsed : _ String.Map.t)
        ~f:(fun ~key ~data:unparsed_data ->
          let is_equal =
            match Map.find (original : _ String.Map.t) key with
            | None -> false
            | Some original_data ->
              (match Map.find (equal_values_for_field : _ String.Map.t) key with
               | None -> List.equal String.equal original_data unparsed_data
               | Some equal -> equal original_data unparsed_data)
          in
          if not is_equal
          then
            print_s
              [%message
                "Field is not equal!"
                  (key : string)
                  (original : string list String.Map.t)
                  (unparsed : string list String.Map.t)];
          is_equal)
    ;;

    let%quick_test "attempt to parse generated queries" =
      fun ((query, path) :
             (string list String.Map.t * string list
              [@generator generator] [@shrinker.disable])) ->
        let parser = Parser.Variant.make ~namespace:[] (module Query) in
        let projection = Parser.eval parser in
        let result = projection.parse_exn { query; path } in
        let { Components.query = unparsed_query; path = unparsed_path } =
          projection.unparse result
        in
        assert (maps_equal query unparsed_query);
        assert (List.equal String.equal unparsed_path path)
    ;;
  end)
;;

let%expect_test "path order has duplicate values" =
  let module Query = struct
    type t =
      { foo : string
      ; bar : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Value_parser in
      let open Parser in
      function
      | Foo -> with_prefix [ "foo" ] (from_path string)
      | Bar -> with_prefix [ "bar" ] (from_path int)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T [ Foo; Bar; Foo ]
  end
  in
  let parser = Parser.Record.make (module Query) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌────────────────────────┬──────────────────────────────────────┐
    │ Check name             │ Error message                        │
    ├────────────────────────┼──────────────────────────────────────┤
    │ Sane path orders check │ "Path order cannot have duplicates!" │
    └────────────────────────┴──────────────────────────────────────┘ |}]
;;

let%expect_test "path field does not have a path order" =
  let module Query = struct
    type t =
      { foo : string
      ; bar : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Value_parser in
      let open Parser in
      function
      | Foo -> with_prefix [ "foo" ] (from_path string)
      | Bar -> with_prefix [ "bar" ] (from_path int)
    ;;

    (* Foo and Bar do not have a path order. *)
    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Query) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌────────────────────────┬───────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name             │ Error message                                                                         │
    ├────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────┤
    │ Sane path orders check │ ("Each path parser must be present in path order. The following fields were missing:" │
    │                        │  (missing_fields (Foo Bar)))                                                          │
    └────────────────────────┴───────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

module Path_query = struct
  type t =
    { foo : string
    ; bar : int
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Value_parser in
    let open Parser in
    function
    | Foo -> with_prefix [ "foo" ] (from_path string)
    | Bar -> with_prefix [ "bar" ] (from_path int)
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.T [ Bar; Foo ]
end

let%expect_test "url path queries" =
  let parser = Parser.Record.make (module Path_query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  let path = [ "bar"; "1234"; "foo"; "hello" ] in
  let result = projection.parse_exn { query; path } in
  print_s (Path_query.sexp_of_t result.result);
  [%expect {| ((foo hello) (bar 1234)) |}];
  let { Components.query = _; path = unparsed_path } = projection.unparse result in
  diff_paths path unparsed_path;
  [%expect {||}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /bar/<int>/foo/<string> │
    └─────────────────────────┘

    (Record
     (label_declarations
      ((bar (With_prefix (prefix (bar)) (t (From_path Int))))
       (foo (With_prefix (prefix (foo)) (t (From_path String))))))
     (path_order (bar foo))) |}]
;;

let%expect_test "Path with url that does not match" =
  let parser = Parser.Record.make (module Path_query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  (* "/foo/" is missing! *)
  let path = [ "bar"; "1234" ] in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn { query; path });
  [%expect
    {|
    ("Error while parsing record field:"
      (error_message (
        "Did not recognize url during parsing! Expected path to match a [with_prefix] prefix, but path did not match prefix!"
        (prefix (foo))
        (components (
          (path  ())
          (query ())))))
      (field_name foo)
      (unparseable_components (
        (path  ())
        (query ())))) |}]
;;

let%expect_test "slash escaping" =
  let parser = Parser.Record.make (module Path_query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  let path = [ "bar"; "1234"; "foo"; "hi%2Fworld%2Fthese%2Fare%2Fslashes" ] in
  let result = projection.parse_exn { query; path } in
  print_s (Path_query.sexp_of_t result.result);
  [%expect {| ((foo hi/world/these/are/slashes) (bar 1234)) |}];
  let { Components.query = _; path = unparsed_path } = projection.unparse result in
  diff_paths path unparsed_path;
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /bar/<int>/foo/<string> │
    └─────────────────────────┘

    (Record
     (label_declarations
      ((bar (With_prefix (prefix (bar)) (t (From_path Int))))
       (foo (With_prefix (prefix (foo)) (t (From_path String))))))
     (path_order (bar foo))) |}]
;;

let%expect_test "path with different length prefixes" =
  let module Query = struct
    type t =
      { foo : string
      ; bar : int
      ; baz : bool
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Value_parser in
      let open Parser in
      function
      | Foo -> with_prefix [ "foo"; "goes_here" ] (from_path string)
      | Bar -> with_prefix [ "bar" ] (from_path int)
      | Baz -> from_path bool
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T [ Baz; Bar; Foo ]
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  let path = [ "true"; "bar"; "100"; "foo"; "goes_here"; "hi" ] in
  let result = projection.parse_exn { query; path } in
  print_s (Query.sexp_of_t result.result);
  [%expect {| ((foo hi) (bar 100) (baz true)) |}];
  let { Components.query = _; path = unparsed_path } = projection.unparse result in
  diff_paths path unparsed_path;
  [%expect {||}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────┐
    │ All urls                                 │
    ├──────────────────────────────────────────┤
    │ /<bool>/bar/<int>/foo/goes_here/<string> │
    └──────────────────────────────────────────┘

    (Record
     (label_declarations
      ((bar (With_prefix (prefix (bar)) (t (From_path Int))))
       (baz (From_path Bool))
       (foo (With_prefix (prefix (foo goes_here)) (t (From_path String))))))
     (path_order (baz bar foo))) |}]
;;

let%expect_test "url path queries" =
  let parser = Parser.Record.make (module Path_query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  let path = [ "bar"; "1234"; "foo"; "hello" ] in
  let result = projection.parse_exn { query; path } in
  print_s (Path_query.sexp_of_t result.result);
  [%expect {| ((foo hello) (bar 1234)) |}];
  let { Components.query = _; path = unparsed_path } = projection.unparse result in
  diff_paths path unparsed_path;
  [%expect {||}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /bar/<int>/foo/<string> │
    └─────────────────────────┘

    (Record
     (label_declarations
      ((bar (With_prefix (prefix (bar)) (t (From_path Int))))
       (foo (With_prefix (prefix (foo)) (t (From_path String))))))
     (path_order (bar foo))) |}]
;;

let%expect_test "position matters" =
  let module Query = struct
    type t =
      { username : string
      ; comment_id : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Value_parser in
      let open Parser in
      function
      | Username -> with_prefix [ "username" ] (from_path string)
      | Comment_id -> with_prefix [ "comment_id" ] (from_path int)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T [ Username; Comment_id ]
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  (* Username is "comment_id" which is the same prefix used for the comment id. *)
  let path = [ "username"; "comment_id"; "comment_id"; "1234" ] in
  let result = projection.parse_exn { query; path } in
  print_s (Query.sexp_of_t result.result);
  [%expect {| ((username comment_id) (comment_id 1234)) |}];
  let { Components.query = _; path = unparsed_path } = projection.unparse result in
  diff_paths path unparsed_path;
  [%expect {||}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────┐
    │ All urls                            │
    ├─────────────────────────────────────┤
    │ /username/<string>/comment_id/<int> │
    └─────────────────────────────────────┘

    (Record
     (label_declarations
      ((comment_id (With_prefix (prefix (comment_id)) (t (From_path Int))))
       (username (With_prefix (prefix (username)) (t (From_path String))))))
     (path_order (username comment_id))) |}]
;;

let%expect_test "path with value parser project" =
  let module Query = struct
    type t =
      { foo : string
      ; bar : Game_id.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Value_parser in
      let open Parser in
      function
      | Foo -> with_prefix [ "foo" ] (from_path string)
      | Bar ->
        (* path (Value_parser.project) *)
        with_prefix
          [ "bar" ]
          (from_path
             (Value_parser.project int ~parse_exn:Game_id.of_int ~unparse:Game_id.to_int))
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T [ Bar; Foo ]
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  let path = [ "bar"; "42"; "foo"; "hello" ] in
  let result = projection.parse_exn { query; path } in
  print_s (Query.sexp_of_t result.result);
  [%expect {| ((foo hello) (bar 42)) |}];
  let { Components.query = _; path = unparsed_path } = projection.unparse result in
  diff_paths path unparsed_path;
  [%expect {||}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────┐
    │ All urls                         │
    ├──────────────────────────────────┤
    │ /bar/<project<int>>/foo/<string> │
    └──────────────────────────────────┘

    (Record
     (label_declarations
      ((bar (With_prefix (prefix (bar)) (t (From_path (Project Int)))))
       (foo (With_prefix (prefix (foo)) (t (From_path String))))))
     (path_order (bar foo))) |}]
;;

let%expect_test "path with field parser project" =
  let module Query = struct
    type t =
      { foo : string
      ; bar : Game_id.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Value_parser in
      let open Parser in
      function
      | Foo -> with_prefix [ "foo" ] (from_path string)
      | Bar ->
        (* Parser.project (path) *)
        with_prefix
          [ "bar" ]
          (Parser.project
             (from_path int)
             ~parse_exn:Game_id.of_int
             ~unparse:Game_id.to_int)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T [ Bar; Foo ]
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  let path = [ "bar"; "42"; "foo"; "hello" ] in
  let result = projection.parse_exn { query; path } in
  print_s (Query.sexp_of_t result.result);
  [%expect {| ((foo hello) (bar 42)) |}];
  let { Components.query = _; path = unparsed_path } = projection.unparse result in
  diff_paths path unparsed_path;
  [%expect {||}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /bar/<int>/foo/<string> │
    └─────────────────────────┘

    (Record
     (label_declarations
      ((bar (With_prefix (prefix (bar)) (t (Project (From_path Int)))))
       (foo (With_prefix (prefix (foo)) (t (From_path String))))))
     (path_order (bar foo))) |}]
;;

let%expect_test "path and remaining path working together" =
  let module Query = struct
    type t =
      { foo : int
      ; bar : int list
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Parser in
      let open Value_parser in
      function
      | Foo -> with_prefix [ "foo" ] (from_path int)
      | Bar -> with_prefix [ "remaining" ] (from_remaining_path int)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T [ Foo; Bar ]
  end
  in
  let parser = Parser.Record.make (module Query) in
  let projection = Parser.eval parser in
  let query = String.Map.empty in
  let path = [ "foo"; "42"; "remaining"; "1"; "2"; "3"; "4" ] in
  let result = projection.parse_exn { query; path } in
  print_s (Query.sexp_of_t result.result);
  [%expect {| ((foo 42) (bar (1 2 3 4))) |}];
  let { Components.query = _; path = unparsed_path } = projection.unparse result in
  diff_paths path unparsed_path;
  [%expect {||}];
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────┐
    │ All urls                             │
    ├──────────────────────────────────────┤
    │ /foo/<int>/remaining/<multiple<int>> │
    └──────────────────────────────────────┘

    (Record
     (label_declarations
      ((bar (With_prefix (prefix (remaining)) (t (From_remaining_path Int))))
       (foo (With_prefix (prefix (foo)) (t (From_path Int))))))
     (path_order (foo bar))) |}]
;;

let%expect_test "variants test" =
  let module Query = struct
    type t =
      | Foo
      | Bar
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Foo -> Parser.unit
      | Bar -> Parser.unit
    ;;
  end
  in
  let parser = Parser.Variant.make (module Query) in
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "foo" ]
    ~query:String.Map.empty
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      Foo |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "bar" ]
    ~query:String.Map.empty
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      Bar |}]);
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────┐
    │ All urls │
    ├──────────┤
    │ /bar     │
    │ /foo     │
    └──────────┘

    (Variant
     (constructor_declarations
      ((bar (With_prefix (prefix (bar)) (t Unit)))
       (foo (With_prefix (prefix (foo)) (t Unit)))))
     (patterns
      ((bar ((pattern ((Match bar))) (needed_match Prefix)))
       (foo ((pattern ((Match foo))) (needed_match Prefix)))))
     (override_namespace ())) |}]
;;

let%expect_test "variants with renaming" =
  let module Settings = struct
    type t =
      { volume : float
      ; something_else : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t =
      let open Value_parser in
      function
      | Volume -> Parser.from_query_required float
      | Something_else -> Parser.from_query_required int
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let module Query = struct
    type t =
      | Homepage
      | Settings of Settings.t
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Homepage -> Parser.with_remaining_path [] Parser.unit
      | Settings -> Parser.Record.make ~namespace:[] (module Settings)
    ;;
  end
  in
  let parser = Parser.Variant.make ~namespace:[] (module Query) in
  let projection = Parser.eval parser in
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────────────────────────────┐
    │ All urls                                      │
    ├───────────────────────────────────────────────┤
    │ /                                             │
    │ /settings?something_else=<int>&volume=<float> │
    └───────────────────────────────────────────────┘

    (Variant
     (constructor_declarations
      ((homepage (With_remaining_path (needed_path ()) (t Unit)))
       (settings
        (With_prefix (prefix (settings))
         (t
          (Record (override_namespace ())
           (label_declarations
            ((something_else (From_query_required (value_parser Int)))
             (volume (From_query_required (value_parser Float)))))
           (path_order ())))))))
     (patterns
      ((homepage ((pattern ()) (needed_match All)))
       (settings ((pattern ((Match settings))) (needed_match Prefix)))))
     (override_namespace (()))) |}];
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:String.Map.empty
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      Homepage |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "settings" ]
    ~query:(String.Map.of_alist_exn [ "volume", [ "1.2" ]; "something_else", [ "123" ] ])
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () ->
      [%expect {|
      (Settings ((volume 1.2) (something_else 123))) |}]);
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────────────────────────────┐
    │ All urls                                      │
    ├───────────────────────────────────────────────┤
    │ /                                             │
    │ /settings?something_else=<int>&volume=<float> │
    └───────────────────────────────────────────────┘

    (Variant
     (constructor_declarations
      ((homepage (With_remaining_path (needed_path ()) (t Unit)))
       (settings
        (With_prefix (prefix (settings))
         (t
          (Record (override_namespace ())
           (label_declarations
            ((something_else (From_query_required (value_parser Int)))
             (volume (From_query_required (value_parser Float)))))
           (path_order ())))))))
     (patterns
      ((homepage ((pattern ()) (needed_match All)))
       (settings ((pattern ((Match settings))) (needed_match Prefix)))))
     (override_namespace (()))) |}]
;;

let%expect_test "/user/comments, /user/posts, and /feed" =
  let module User = struct
    type t =
      | Comments
      | Posts
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Comments -> Parser.unit
      | Posts -> Parser.unit
    ;;
  end
  in
  let module Url = struct
    type t =
      | User of User.t
      | Feed
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | User -> Parser.Variant.make (module User)
      | Feed -> Parser.unit
    ;;
  end
  in
  let parser = Parser.Variant.make (module Url) in
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────┐
    │ All urls       │
    ├────────────────┤
    │ /feed          │
    │ /user/comments │
    │ /user/posts    │
    └────────────────┘

    (Variant
     (constructor_declarations
      ((feed (With_prefix (prefix (feed)) (t Unit)))
       (user
        (With_prefix (prefix (user))
         (t
          (Variant
           (constructor_declarations
            ((comments (With_prefix (prefix (comments)) (t Unit)))
             (posts (With_prefix (prefix (posts)) (t Unit)))))
           (patterns
            ((comments ((pattern ((Match comments))) (needed_match Prefix)))
             (posts ((pattern ((Match posts))) (needed_match Prefix)))))
           (override_namespace ())))))))
     (patterns
      ((feed ((pattern ((Match feed))) (needed_match Prefix)))
       (user ((pattern ((Match user))) (needed_match Prefix)))))
     (override_namespace ())) |}];
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "user"; "comments" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
      (User Comments) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "user"; "posts" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
      (User Posts) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "feed" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
      Feed |}])
;;

let%expect_test "/user/:user_id/comment/:comment_id" =
  let module Nested = struct
    type t = Comment of int [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Comment -> Parser.from_path Value_parser.int
    ;;
  end
  in
  let module Url = struct
    type t =
      { user_id : int
      ; nested : Nested.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | User_id -> Parser.with_prefix [ "user" ] (Parser.from_path Value_parser.int)
      | Nested -> Parser.Variant.make (module Nested)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T [ User_id; Nested ]
  end
  in
  let parser = Parser.Record.make (module Url) in
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────────┐
    │ All urls                  │
    ├───────────────────────────┤
    │ /user/<int>/comment/<int> │
    └───────────────────────────┘

    (Record
     (label_declarations
      ((nested
        (Variant
         (constructor_declarations
          ((comment (With_prefix (prefix (comment)) (t (From_path Int))))))
         (patterns
          ((comment ((pattern ((Match comment))) (needed_match Prefix)))))
         (override_namespace ())))
       (user_id (With_prefix (prefix (user)) (t (From_path Int))))))
     (path_order (user_id nested))) |}];
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "user"; "123"; "comment"; "321" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
      ((user_id 123) (nested (Comment 321))) |}])
;;

let%expect_test "/user/:user_id/comment/:comment_id without nesting" =
  let module Url = struct
    type t =
      { user_id : int
      ; comment_id : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | User_id -> Parser.with_prefix [ "user" ] (Parser.from_path Value_parser.int)
      | Comment_id -> Parser.with_prefix [ "comment" ] (Parser.from_path Value_parser.int)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T [ User_id; Comment_id ]
  end
  in
  let parser = Parser.Record.make (module Url) in
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────────┐
    │ All urls                  │
    ├───────────────────────────┤
    │ /user/<int>/comment/<int> │
    └───────────────────────────┘

    (Record
     (label_declarations
      ((comment_id (With_prefix (prefix (comment)) (t (From_path Int))))
       (user_id (With_prefix (prefix (user)) (t (From_path Int))))))
     (path_order (user_id comment_id))) |}];
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "user"; "123"; "comment"; "321" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
      ((user_id 123) (comment_id 321)) |}])
;;

let%expect_test "/user/:user_id/comment/:comment_id?foo=123&bar=200 , \
                 /user/:user_id/posts/:post_id?bam=123&baz=2.3 and \"/\" "
  =
  let module Comment = struct
    type t =
      { foo : int
      ; bar : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Foo -> Parser.from_query_required Value_parser.int
      | Bar -> Parser.from_query_required Value_parser.int
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let module Post = struct
    type t =
      { bam : int
      ; baz : float
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Bam -> Parser.from_query_required Value_parser.int
      | Baz -> Parser.from_query_required Value_parser.float
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let module User = struct
    type t =
      | Comment of
          { comment_id : int
          ; comment : Comment.t
          } [@typed_fields]
      | Post of
          { post_id : int
          ; post : Post.t
          } [@typed_fields]
    [@@deriving typed_variants, sexp, equal]

    module Anon_comment = struct
      module Typed_field =
        Typed_variant.Typed_variant_anonymous_records.Typed_field_of_comment

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | Comment_id -> Parser.from_path Value_parser.int
        | Comment -> Parser.Record.make ~namespace:[] (module Comment)
      ;;

      module Path_order = Path_order (Typed_field)

      let path_order = Path_order.T [ Comment_id ]
    end

    module Anon_post = struct
      module Typed_field =
        Typed_variant.Typed_variant_anonymous_records.Typed_field_of_post

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | Post_id -> Parser.from_path Value_parser.int
        | Post -> Parser.Record.make ~namespace:[] (module Post)
      ;;

      module Path_order = Path_order (Typed_field)

      let path_order = Path_order.T [ Post_id ]
    end

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Comment -> Parser.Record.make ~namespace:[] (module Anon_comment)
      | Post -> Parser.Record.make ~namespace:[] (module Anon_post)
    ;;
  end
  in
  let module Url = struct
    type t =
      | User of
          { user_id : int
          ; user : User.t
          } [@typed_fields]
      | Homepage
    [@@deriving typed_variants, sexp, equal]

    module Anon_user = struct
      module Typed_field =
        Typed_variant.Typed_variant_anonymous_records.Typed_field_of_user

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | User_id -> Parser.from_path Value_parser.int
        | User -> Parser.Variant.make ~namespace:[] (module User)
      ;;

      module Path_order = Path_order (Typed_field)

      let path_order = Path_order.T [ User_id; User ]
    end

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | User ->
        Parser.with_prefix
          [ "user" ]
          (Parser.Record.make ~namespace:[] (module Anon_user))
      | Homepage -> Parser.with_remaining_path [] Parser.unit
    ;;
  end
  in
  let parser = Parser.Variant.make (module Url) in
  let projection = Parser.eval parser in
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────────────────────────────┐
    │ All urls                                      │
    ├───────────────────────────────────────────────┤
    │ /                                             │
    │ /user/<int>/comment/<int>?bar=<int>&foo=<int> │
    │ /user/<int>/post/<int>?bam=<int>&baz=<float>  │
    └───────────────────────────────────────────────┘

    (Variant
     (constructor_declarations
      ((homepage (With_remaining_path (needed_path ()) (t Unit)))
       (user
        (With_prefix (prefix (user))
         (t
          (Record (override_namespace ())
           (label_declarations
            ((user
              (Variant
               (constructor_declarations
                ((comment
                  (With_prefix (prefix (comment))
                   (t
                    (Record (override_namespace ())
                     (label_declarations
                      ((comment
                        (Record (override_namespace ())
                         (label_declarations
                          ((bar (From_query_required (value_parser Int)))
                           (foo (From_query_required (value_parser Int)))))
                         (path_order ())))
                       (comment_id (From_path Int))))
                     (path_order (comment_id))))))
                 (post
                  (With_prefix (prefix (post))
                   (t
                    (Record (override_namespace ())
                     (label_declarations
                      ((post
                        (Record (override_namespace ())
                         (label_declarations
                          ((bam (From_query_required (value_parser Int)))
                           (baz (From_query_required (value_parser Float)))))
                         (path_order ())))
                       (post_id (From_path Int))))
                     (path_order (post_id))))))))
               (patterns
                ((comment ((pattern ((Match comment))) (needed_match Prefix)))
                 (post ((pattern ((Match post))) (needed_match Prefix)))))
               (override_namespace (()))))
             (user_id (From_path Int))))
           (path_order (user_id user))))))))
     (patterns
      ((homepage ((pattern ()) (needed_match All)))
       (user ((pattern ((Match user))) (needed_match Prefix)))))
     (override_namespace ())) |}];
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "user"; "123"; "comment"; "432" ]
    ~query:(String.Map.of_alist_exn [ "foo", [ "12" ]; "bar", [ "32" ] ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
      (User (user_id 123)
       (user (Comment (comment_id 432) (comment ((foo 12) (bar 32)))))) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "user"; "201"; "post"; "445" ]
    ~query:(String.Map.of_alist_exn [ "bam", [ "12" ]; "baz", [ "32." ] ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
      (User (user_id 201) (user (Post (post_id 445) (post ((bam 12) (baz 32)))))) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
      Homepage |}]);
  [%expect {||}]
;;

let%expect_test "variant directly on query" =
  let module Query = struct
    type t = Foo of int [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Foo -> Parser.from_query_required ~key:"q" Value_parser.int
    ;;
  end
  in
  let parser = Parser.Variant.make ~namespace:[] (module Query) in
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "foo" ]
    ~query:(String.Map.of_alist_exn [ "q", [ "123" ] ])
    ~sexp_of_t:Query.sexp_of_t
    ~expect:(fun () -> [%expect {|
      (Foo 123) |}]);
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────┐
    │ All urls     │
    ├──────────────┤
    │ /foo?q=<int> │
    └──────────────┘

    (Variant
     (constructor_declarations
      ((foo
        (With_prefix (prefix (foo))
         (t (From_query_required (override_key q) (value_parser Int)))))))
     (patterns ((foo ((pattern ((Match foo))) (needed_match Prefix)))))
     (override_namespace (()))) |}]
;;

let%expect_test "fully-qualified variant(record) field" =
  let module Nested2 = struct
    type t =
      { qux : int
      ; baz : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Qux -> Parser.from_query_required Value_parser.int
      | Baz -> Parser.from_query_required Value_parser.int
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let module Nested = struct
    type t =
      { foo : Nested2.t
      ; bar : Nested2.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Foo -> Parser.Record.make (module Nested2)
      | Bar -> Parser.Record.make (module Nested2)
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let module Url = struct
    type t =
      | A of Nested.t
      | B of Nested.t
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | A -> Parser.Record.make ~namespace:[ "namespace1" ] (module Nested)
      | B -> Parser.Record.make ~namespace:[ "name"; "space"; "2" ] (module Nested)
    ;;
  end
  in
  let parser = Parser.Variant.make ~namespace:[ "topmost" ] (module Url) in
  let projection = Parser.eval parser in
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /a?topmost.namespace1.bar.baz=<int>&topmost.namespace1.bar.qux=<int>&topmost.namespace1. │
    │ foo.baz=<int>&topmost.namespace1.foo.qux=<int>                                           │
    │ /b?topmost.name.space.2.bar.baz=<int>&topmost.name.space.2.bar.qux=<int>&topmost.name.sp │
    │ ace.2.foo.baz=<int>&topmost.name.space.2.foo.qux=<int>                                   │
    └──────────────────────────────────────────────────────────────────────────────────────────┘

    (Variant
     (constructor_declarations
      ((a
        (With_prefix (prefix (a))
         (t
          (Record (override_namespace (namespace1))
           (label_declarations
            ((bar
              (Record
               (label_declarations
                ((baz (From_query_required (value_parser Int)))
                 (qux (From_query_required (value_parser Int)))))
               (path_order ())))
             (foo
              (Record
               (label_declarations
                ((baz (From_query_required (value_parser Int)))
                 (qux (From_query_required (value_parser Int)))))
               (path_order ())))))
           (path_order ())))))
       (b
        (With_prefix (prefix (b))
         (t
          (Record (override_namespace (name space 2))
           (label_declarations
            ((bar
              (Record
               (label_declarations
                ((baz (From_query_required (value_parser Int)))
                 (qux (From_query_required (value_parser Int)))))
               (path_order ())))
             (foo
              (Record
               (label_declarations
                ((baz (From_query_required (value_parser Int)))
                 (qux (From_query_required (value_parser Int)))))
               (path_order ())))))
           (path_order ())))))))
     (patterns
      ((a ((pattern ((Match a))) (needed_match Prefix)))
       (b ((pattern ((Match b))) (needed_match Prefix)))))
     (override_namespace ((topmost)))) |}];
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "a" ]
    ~query:
      (String.Map.of_alist_exn
         [ "topmost.namespace1.foo.qux", [ "12" ]
         ; "topmost.namespace1.foo.baz", [ "13" ]
         ; "topmost.namespace1.bar.qux", [ "14" ]
         ; "topmost.namespace1.bar.baz", [ "15" ]
         ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect {|
        (A ((foo ((qux 12) (baz 13))) (bar ((qux 14) (baz 15))))) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "b" ]
    ~query:
      (String.Map.of_alist_exn
         [ "topmost.name.space.2.foo.qux", [ "12" ]
         ; "topmost.name.space.2.foo.baz", [ "13" ]
         ; "topmost.name.space.2.bar.qux", [ "14" ]
         ; "topmost.name.space.2.bar.baz", [ "15" ]
         ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect {|
        (B ((foo ((qux 12) (baz 13))) (bar ((qux 14) (baz 15))))) |}])
;;

let%expect_test "conflicting keys result in error" =
  let module Url = struct
    type t =
      { foo : int
      ; bar : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Foo -> Parser.from_query_required ~key:"foo" Value_parser.int
      | Bar -> Parser.from_query_required ~key:"foo" Value_parser.int
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end
  in
  let parser = Parser.Record.make (module Url) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
   Error with parser.
   ┌──────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
   │ Check name           │ Error message                                                                            │
   ├──────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
   │ Duplicate keys check │ ("Duplicate query keys found! This probably occurred due at least one [~key] being renam │
   │                      │ ed to the same string in a [from_query_*] parser. Another possibility is that a renamed  │
   │                      │ key conflicted with an inferred parser."                                                 │
   │                      │  (duplicate_query_keys_by_url ((/?foo=<int>&foo=<int> (foo)))))                          │
   └──────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "conflicting paths" =
  let module Nested = struct
    type t =
      | C
      | BC
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | C -> Parser.with_prefix [ "c" ] Parser.unit
      | BC -> Parser.with_prefix [ "b"; "c" ] Parser.unit
    ;;
  end
  in
  let module Url = struct
    type t =
      | A of Nested.t
      | AB of Nested.t
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | A -> Parser.with_prefix [ "a" ] (Parser.Variant.make (module Nested))
      | AB -> Parser.with_prefix [ "a"; "b" ] (Parser.Variant.make (module Nested))
    ;;
  end
  in
  let parser = Parser.Variant.make (module Url) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
   Error with parser.
   ┌──────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
   │ Check name           │ Error message                                                                            │
   ├──────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
   │ Duplicate urls check │ ("Ambiguous, duplicate urls expressed in parser! This was probably caused due to conflic │
   │                      │ ting renames with [with_prefix] or [with_remaining_path]."                               │
   │                      │  (duplicate_urls (/a/b/c)))                                                              │
   └──────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Non-record, non-variant" =
  let module Url = struct
    type t = int [@@deriving sexp, equal]

    let parser : t Parser.t =
      Parser.project
        (Parser.from_query_required ~key:"foo" Value_parser.int)
        ~parse_exn:Fn.id
        ~unparse:Fn.id
    ;;
  end
  in
  let parser = Url.parser in
  let projection = Parser.eval parser in
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────┐
    │ All urls    │
    ├─────────────┤
    │ /?foo=<int> │
    └─────────────┘

    (Project (From_query_required (override_key foo) (value_parser Int))) |}];
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "foo", [ "123" ] ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
      123 |}])
;;

let%expect_test "insufficient information fails can be detected before parsing occurs" =
  let module Url = struct
    type t = int

    let parser : t Parser.t =
      Parser.project
        (Parser.from_query_required Value_parser.int)
        ~parse_exn:Fn.id
        ~unparse:Fn.id
    ;;
  end
  in
  let parser = Url.parser in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
      Error with parser.
      ┌──────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
      │ Check name                               │ Error message                                                                            │
      ├──────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
      │ Enough information needed to parse check │ "Could not infer query key! In most cases, the keys of the key of a query field can be i │
      │                                          │ nferred from the labels of the record it's in or the constructor of the variant it's in. │
      │                                          │  However, the [from_query_*] parser's key name could not be inferred. You can fix this i │
      │                                          │ n two ways. 1. Wrap your type around a record/variant OR 2. explicitly assign a key thro │
      │                                          │ ugh the optional [?key] parameter."                                                      │
      └──────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "/library, /library/nyc, library/nyc/book, library/nyc/book/dune, \
                 library/nyc/movie/ and library/nyc/movie/dune"
  =
  let module Book = struct
    type t =
      | Book_search
      | Book_view of string
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Book_search -> Parser.with_remaining_path [] Parser.unit
      | Book_view -> Parser.with_prefix [] (Parser.from_path Value_parser.string)
    ;;
  end
  in
  let module Movie = struct
    type t =
      | Movie_search
      | Movie_view of string
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Movie_search -> Parser.with_remaining_path [] Parser.unit
      | Movie_view -> Parser.with_prefix [] (Parser.from_path Value_parser.string)
    ;;
  end
  in
  let module Library = struct
    type t =
      | Content_search
      | Book of Book.t
      | Movie of Movie.t
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Content_search -> Parser.with_remaining_path [] Parser.unit
      | Book -> Parser.Variant.make (module Book)
      | Movie -> Parser.Variant.make (module Movie)
    ;;
  end
  in
  let module Url = struct
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
  in
  let parser = Parser.Variant.make (module Url) in
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
    └──────────────────────────────────┘ |}];
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "library" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
      Library_search |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "library"; "nyc" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect {| (Library_page (library_name nyc) (library_contents Content_search)) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "library"; "nyc"; "book" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {| (Library_page (library_name nyc) (library_contents (Book Book_search))) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "library"; "nyc"; "book"; "dune" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {| (Library_page (library_name nyc) (library_contents (Book (Book_view dune)))) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "library"; "nyc"; "movie" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {| (Library_page (library_name nyc) (library_contents (Movie Movie_search))) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "library"; "nyc"; "movie"; "dune" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
      (Library_page (library_name nyc)
       (library_contents (Movie (Movie_view dune)))) |}])
;;

let%expect_test "[with_remaining_path] blocks parsers after it." =
  let module Nested = struct
    type t =
      | Foo
      | Bar
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Foo -> Parser.unit
      | Bar -> Parser.unit
    ;;
  end
  in
  let module Url1 = struct
    type t = A of Nested.t [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | A -> Parser.with_remaining_path [ "a" ] (Parser.Variant.make (module Nested))
    ;;
  end
  in
  let module Url2 = struct
    type t = A of int [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | A -> Parser.with_remaining_path [ "a" ] (Parser.from_path Value_parser.int)
    ;;
  end
  in
  let parser = Parser.Variant.make (module Url1) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌───────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name                                        │ Error message                                                                            │
    ├───────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Remaining path does not block other parsers check │ "Error! There cannot be path parsers inside of a [with_remaining_path] combinator! The r │
    │                                                   │ eason for this is that there won't be any values after that parser!"                     │
    └───────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}];
  let parser = Parser.Variant.make (module Url2) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌───────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name                                        │ Error message                                                                            │
    ├───────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Remaining path does not block other parsers check │ "Error! There cannot be path parsers inside of a [with_remaining_path] combinator! The r │
    │                                                   │ eason for this is that there won't be any values after that parser!"                     │
    └───────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Backwards compatibility" =
  let module Url1 = struct
    type t = A [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | A -> Parser.unit
    ;;
  end
  in
  let module Url2 = struct
    type t =
      | A
      | B
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | A -> Parser.unit
      | B -> Parser.unit
    ;;
  end
  in
  let module Url3 = struct
    type t = C [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | C -> Parser.unit
    ;;
  end
  in
  let parser1 = Parser.Variant.make (module Url1) in
  Parser.check_ok_and_print_urls_or_errors parser1;
  [%expect
    {|
    URL parser looks good!
    ┌──────────┐
    │ All urls │
    ├──────────┤
    │ /a       │
    └──────────┘ |}];
  let parser2 = Parser.Variant.make (module Url2) in
  Parser.check_ok_and_print_urls_or_errors parser2;
  [%expect
    {|
    URL parser looks good!
    ┌──────────┐
    │ All urls │
    ├──────────┤
    │ /a       │
    │ /b       │
    └──────────┘ |}];
  let parser3 = Parser.Variant.make (module Url3) in
  Parser.check_ok_and_print_urls_or_errors parser3;
  [%expect
    {|
    URL parser looks good!
    ┌──────────┐
    │ All urls │
    ├──────────┤
    │ /c       │
    └──────────┘ |}]
;;

let%expect_test "ambiguous look ahead" =
  let module Url = struct
    type t =
      | A of int
      | B of string
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | A -> Parser.with_prefix [] (Parser.from_path Value_parser.int)
      | B -> Parser.with_prefix [] (Parser.from_path Value_parser.string)
    ;;
  end
  in
  let parser = Parser.Variant.make (module Url) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌─────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name                                              │ Error message                                                                            │
    ├─────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Ambiguous choices for picking variant constructor check │ ("Duplicate patterns found!"                                                             │
    │                                                         │  (duplicate_patterns (((pattern ()) (needed_match Prefix)))))                            │
    │ Duplicate urls check                                    │ ("Ambiguous, duplicate urls expressed in parser! This was probably caused due to conflic │
    │                                                         │ ting renames with [with_prefix] or [with_remaining_path]."                               │
    │                                                         │  (duplicate_urls (/<string>)))                                                           │
    └─────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Weird lookahead urls" =
  let module Url = struct
    type t =
      | Foo of
          { a : int
          ; b : int
          } [@typed_fields]
      | Bar of
          { c : int
          ; d : int
          } [@typed_fields]
    [@@deriving typed_variants, sexp, equal]

    module Anon_for_foo = struct
      module Typed_field =
        Typed_variant.Typed_variant_anonymous_records.Typed_field_of_foo

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | A -> Parser.from_path Value_parser.int
        | B -> Parser.with_prefix [ "b" ] (Parser.from_path Value_parser.int)
      ;;

      module Path_order = Path_order (Typed_field)

      let path_order = Path_order.T [ A; B ]
    end

    module Anon_for_bar = struct
      module Typed_field =
        Typed_variant.Typed_variant_anonymous_records.Typed_field_of_bar

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | C -> Parser.from_path Value_parser.int
        | D -> Parser.with_prefix [ "d" ] (Parser.from_path Value_parser.int)
      ;;

      module Path_order = Path_order (Typed_field)

      let path_order = Path_order.T [ C; D ]
    end

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Foo -> Parser.Record.make (module Anon_for_foo)
      | Bar -> Parser.Record.make (module Anon_for_bar)
    ;;
  end
  in
  let parser = Parser.Variant.make (module Url) in
  show_structure parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────┐
    │ All urls       │
    ├────────────────┤
    │ /<int>/b/<int> │
    │ /<int>/d/<int> │
    └────────────────┘

    (Variant
     (constructor_declarations
      ((bar
        (Record
         (label_declarations
          ((c (From_path Int))
           (d (With_prefix (prefix (d)) (t (From_path Int))))))
         (path_order (c d))))
       (foo
        (Record
         (label_declarations
          ((a (From_path Int))
           (b (With_prefix (prefix (b)) (t (From_path Int))))))
         (path_order (a b))))))
     (patterns
      ((bar ((pattern (Ignore (Match d))) (needed_match Prefix)))
       (foo ((pattern (Ignore (Match b))) (needed_match Prefix)))))
     (override_namespace ())) |}];
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "12"; "b"; "132" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {| (Foo (a 12) (b 132)) |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "12"; "d"; "132" ]
    ~query:String.Map.empty
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {| (Bar (c 12) (d 132)) |}])
;;

module Record = struct
  type t =
    { a : int
    ; b : int
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
    | A -> Parser.from_query_required Value_parser.int |> Parser.with_prefix [ "a" ]
    | B -> Parser.from_query_required Value_parser.int |> Parser.with_prefix [ "b" ]
  ;;

  module Path_order = Parser.Record.Path_order (Typed_field)

  let path_order = Path_order.T [ A; B ]
end

let%expect_test "optional" =
  let module Url = struct
    type t = Record.t option [@@deriving sexp, equal]

    let parser = Parser.optional_query_fields (Parser.Record.make (module Record))
  end
  in
  Parser.check_ok_and_print_urls_or_errors Url.parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────┐
    │ All urls             │
    ├──────────────────────┤
    │ /                    │
    │ /a/b?a=<int>&b=<int> │
    └──────────────────────┘ |}];
  let projection = Parser.eval Url.parser in
  expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {| |}])
    projection
    ~path:[ "a"; "b" ]
    ~query:(String.Map.of_alist_exn [ "a", [ "1" ]; "b", [ "2" ] ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {|
   (((a 1) (b 2))) |}]);
  expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {||}])
    projection
    ~path:[ "a"; "b" ]
    ~query:(String.Map.of_alist_exn [ "a", [ "1" ] ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () -> [%expect {| () |}])
;;

let%expect_test "optional continues after failure" =
  let module Url = struct
    type t =
      { first : Record.t option
      ; second : Record.t option
      ; third : Record.t
      ; last : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | First -> Parser.optional_query_fields (Parser.Record.make (module Record))
      | Second -> Parser.optional_query_fields (Parser.Record.make (module Record))
      | Third -> Parser.Record.make (module Record)
      | Last -> Parser.from_path Value_parser.int
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T [ First; Second; Third; Last ]
  end
  in
  let parser = Parser.Record.make (module Url) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /a/b/<int>?third.a=<int>&third.b=<int>                                                   │
    │ /a/b/a/b/<int>?first.a=<int>&first.b=<int>&third.a=<int>&third.b=<int>                   │
    │ /a/b/a/b/<int>?second.a=<int>&second.b=<int>&third.a=<int>&third.b=<int>                 │
    │ /a/b/a/b/a/b/<int>?first.a=<int>&first.b=<int>&second.a=<int>&second.b=<int>&third.a=<in │
    │ t>&third.b=<int>                                                                         │
    └──────────────────────────────────────────────────────────────────────────────────────────┘ |}];
  let projection = Parser.eval parser in
  expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {| |}])
    projection
    ~path:[ "a"; "b"; "a"; "b"; "a"; "b"; "123" ]
    ~query:
      (String.Map.of_alist_exn
         [ "first.a", [ "0" ]
         ; "first.b", [ "0" ]
         ; "second.a", [ "0" ]
         ; "second.b", [ "0" ]
         ; "third.a", [ "0" ]
         ; "third.b", [ "0" ]
         ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
   ((first (((a 0) (b 0)))) (second (((a 0) (b 0)))) (third ((a 0) (b 0)))
    (last 123)) |}]);
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    projection.parse_exn
      { query =
          String.Map.of_alist_exn
            [ "first.a", [ "0" ]
            ; "first.b", [ "0" ]
            ; "second.a", [ "0" ] (*  ; "second.b", [ "0" ]  <--- missing *)
            ; "third.a", [ "0" ]
            ; "third.b", [ "0" ]
            ]
      ; path = [ "a"; "b"; "a"; "b"; "a"; "b"; "123" ]
      });
  [%expect
    {|
    ("Error while parsing record field:"
      (error_message (Failure "Int.of_string: \"a\""))
      (field_name last)
      (unparseable_components ((path (a b 123)) (query ((second.a (0))))))) |}];
  expect_output_and_identity_roundtrip
    ~expect_diff:(fun () -> [%expect {| |}])
    projection
    ~path:[ "a"; "b"; "a"; "b"; "123" ]
    ~query:
      (String.Map.of_alist_exn
         [ "first.a", [ "0" ]
         ; "first.b", [ "0" ]
         ; "second.a", [ "0" ] (*  ; "second.b", [ "0" ]  <--- missing *)
         ; "third.a", [ "0" ]
         ; "third.b", [ "0" ]
         ])
    ~sexp_of_t:Url.sexp_of_t
    ~expect:(fun () ->
      [%expect
        {|
   ((first (((a 0) (b 0)))) (second ()) (third ((a 0) (b 0))) (last 123)) |}])
;;

let%expect_test "uri projection" =
  let module Url = struct
    type t =
      { a : int
      ; b : int
      ; c : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> Parser.from_path Value_parser.int
      | B -> Parser.from_query_required Value_parser.int
      | C -> Parser.from_query_required Value_parser.int
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T [ A ]
  end
  in
  let parser = Parser.Record.make (module Url) in
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────────────┐
    │ All urls               │
    ├────────────────────────┤
    │ /<int>?b=<int>&c=<int> │
    └────────────────────────┘ |}];
  let projection = Parser.eval_for_uri parser in
  let result = projection.parse_exn (Uri.of_string "/23?b=1&c=2") in
  print_s [%message (result.result : Url.t)];
  [%expect {| (result.result ((a 23) (b 1) (c 2))) |}];
  let unparsed_uri = projection.unparse result in
  print_endline (Uri.to_string unparsed_uri);
  [%expect {| 23?b=1&c=2 |}]
;;

let%expect_test "[name]" =
  let module Url = struct
    type t = int

    let parser : t Parser.t =
      Parser.from_query_required
        ~key:"q"
        (Value_parser.project
           ~parse_exn:Fn.id
           ~unparse:Fn.id
           (Value_parser.fallback Value_parser.int ~fallback:1))
    ;;
  end
  in
  Parser.check_ok_and_print_urls_or_errors Url.parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────┐
    │ All urls                     │
    ├──────────────────────────────┤
    │ /?q=<project<fallback<int>>> │
    └──────────────────────────────┘ |}];
  let module Named_url = struct
    type t = int

    let parser : t Parser.t =
      Parser.from_query_required
        ~key:"q"
        (Value_parser.name
           "my_type"
           (Value_parser.project
              ~parse_exn:Fn.id
              ~unparse:Fn.id
              (Value_parser.fallback Value_parser.int ~fallback:1)))
    ;;
  end
  in
  Parser.check_ok_and_print_urls_or_errors Named_url.parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────┐
    │ All urls      │
    ├───────────────┤
    │ /?q=<my_type> │
    └───────────────┘ |}];
  let projection = Parser.eval Named_url.parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "q", [ "123" ] ])
    ~sexp_of_t:Int.sexp_of_t
    ~expect:(fun () -> [%expect {|
   123 |}])
;;

let%expect_test "[regression] exponential url shapes" =
  let module Url = struct
    module T = struct
      type t =
        { a : string option
        ; b : string option
        ; c : string option
        ; d : string option
        ; e : string option
        }
      [@@deriving typed_fields, sexp, equal]

      let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
        | A -> Parser.from_query_optional Value_parser.string
        | B -> Parser.from_query_optional Value_parser.string
        | C -> Parser.from_query_optional Value_parser.string
        | D -> Parser.from_query_optional Value_parser.string
        | E -> Parser.from_query_optional Value_parser.string
      ;;

      module Path_order = Parser.Record.Path_order (Typed_field)

      let path_order = Path_order.T []
    end

    include T

    let parser = Parser.Record.make (module T)
  end
  in
  let versioned_parser = Versioned_parser.first_parser Url.parser in
  Versioned_parser.check_ok_and_print_urls_or_errors versioned_parser;
  (* 2^n shapes where n is the number of optional query fields. *)
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /?a=<optional<string>>&b=<optional<string>>&c=<optional<string>>&d=<optional<string>>&e= │
    │ <optional<string>>                                                                       │
    └──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
