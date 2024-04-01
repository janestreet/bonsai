open! Core
open Uri_parsing
module Id = String

(* $MDX part-begin=string_parser *)
let (parser : string Parser.t) = Parser.from_path Value_parser.string

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
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

(* $MDX part-end *)
(* $MDX part-begin=int_parser *)
let (parser : int Parser.t) = Parser.from_query_required ~key:"q" Value_parser.int

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────┐
    │ All urls  │
    ├───────────┤
    │ /?q=<int> │
    └───────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=many_id_parser *)
let (parser : Id.t list Parser.t) =
  Parser.from_remaining_path (Value_parser.sexpable (module Id))
  |> Parser.with_prefix [ "With"; "a"; "Prefix" ]
;;

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────┐
    │ All urls                            │
    ├─────────────────────────────────────┤
    │ /With/a/Prefix/<multiple<sexpable>> │
    └─────────────────────────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=forum_search_params *)
module Search_params = struct
  type t =
    { query : string
    ; author_id : Id.t option
    ; categories : Id.t list
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
    | Query -> Parser.from_path Value_parser.string
    | Author_id -> Parser.from_query_optional (Value_parser.sexpable (module Id))
    | Categories -> Parser.from_query_many (Value_parser.sexpable (module Id))
  ;;

  module Path_order = Parser.Record.Path_order (Typed_field)

  let path_order = Path_order.T [ Query ]
end

let search_params_parser = Parser.Record.make (module Search_params)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors search_params_parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                 │
    ├──────────────────────────────────────────────────────────────────────────┤
    │ /<string>?author_id=<optional<sexpable>>&categories=<multiple<sexpable>> │
    └──────────────────────────────────────────────────────────────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=forum_admin_page *)
module Admin_page = struct
  type t =
    | Settings
    | Edit_user of Id.t
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Settings -> Parser.unit
    | Edit_user -> Parser.from_path (Value_parser.sexpable (module Id))
  ;;
end

let admin_page_parser = Parser.Variant.make (module Admin_page)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors admin_page_parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────┐
    │ All urls              │
    ├───────────────────────┤
    │ /edit_user/<sexpable> │
    │ /settings             │
    └───────────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=forum_user_page *)

module User_page = struct
  module Subpage = struct
    type t =
      | Profile
      | Posts
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Profile -> Parser.unit
      | Posts -> Parser.unit
    ;;
  end

  type t =
    { user_id : Id.t
    ; subpage : Subpage.t
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
    | User_id -> Parser.from_path (Value_parser.sexpable (module Id))
    | Subpage -> Parser.Variant.make (module Subpage)
  ;;

  module Path_order = Parser.Record.Path_order (Typed_field)

  let path_order = Path_order.T [ User_id; Subpage ]
end

let user_page_parser = Parser.Record.make (module User_page)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors user_page_parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────┐
    │ All urls            │
    ├─────────────────────┤
    │ /<sexpable>/posts   │
    │ /<sexpable>/profile │
    └─────────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=forum_url *)
module Url = struct
  type t =
    | Homepage
    | Discussion of Id.t
    | Search of Search_params.t
    | User of User_page.t
    | Admin of Admin_page.t
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.end_of_path Parser.unit
    | Discussion -> Parser.from_path (Value_parser.sexpable (module Id))
    | Search -> search_params_parser
    | User -> user_page_parser
    | Admin -> admin_page_parser
  ;;
end

let parser = Parser.Variant.make (module Url)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /                                                                                        │
    │ /admin/edit_user/<sexpable>                                                              │
    │ /admin/settings                                                                          │
    │ /discussion/<sexpable>                                                                   │
    │ /search/<string>?search.author_id=<optional<sexpable>>&search.categories=<multiple<sexpa │
    │ ble>>                                                                                    │
    │ /user/<sexpable>/posts                                                                   │
    │ /user/<sexpable>/profile                                                                 │
    └──────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=forum_old *)
module Old_url = struct
  type t =
    | Homepage
    | Post of Id.t
    | Search of string
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.end_of_path Parser.unit
    | Post -> Parser.from_path (Value_parser.sexpable (module Id))
    | Search -> Parser.from_path Value_parser.string
  ;;
end

let old_parser = Parser.Variant.make (module Old_url)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors old_parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────┐
    │ All urls         │
    ├──────────────────┤
    │ /                │
    │ /post/<sexpable> │
    │ /search/<string> │
    └──────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=forum_versioned *)
let v1_parser = Versioned_parser.first_parser old_parser

let v2_parser =
  Versioned_parser.new_parser parser ~previous:v1_parser ~f:(function
    | Homepage -> Homepage
    | Post id -> Discussion id
    | Search query -> Search { query; author_id = None; categories = [] })
;;

let%expect_test _ =
  Versioned_parser.check_ok_and_print_urls_or_errors v2_parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /                                                                                        │
    │ /admin/edit_user/<sexpable>                                                              │
    │ /admin/settings                                                                          │
    │ /discussion/<sexpable>                                                                   │
    │ /search/<string>?search.author_id=<optional<sexpable>>&search.categories=<multiple<sexpa │
    │ ble>>                                                                                    │
    │ /user/<sexpable>/posts                                                                   │
    │ /user/<sexpable>/profile                                                                 │
    └──────────────────────────────────────────────────────────────────────────────────────────┘

           |
    falls back to
           |
           v

    URL parser looks good!
    ┌──────────────────┐
    │ All urls         │
    ├──────────────────┤
    │ /                │
    │ /post/<sexpable> │
    │ /search/<string> │
    └──────────────────┘
    |}]
;;
(* $MDX part-end *)
