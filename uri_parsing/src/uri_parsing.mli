open! Core

module Projection : sig
  (** Abstraction used heavily in this module to parse/unparse from 'a to 'b *)
  type ('a, 'b) t =
    { parse_exn : 'a -> 'b
    ; unparse : 'b -> 'a
    }
end

(** This module contains primitive parsers. Not useful on their own, but when combined
    with the parsers in the [Parser] module they become useful. *)
module Value_parser : sig
  (** A ['a Value_parser.t] will parse [string] -> ['a] and unparse ['a] -> [string] *)
  type 'a t

  val int : int t
  val string : string t
  val float : float t
  val bool : bool t
  val stringable : (module Stringable with type t = 'a) -> 'a t
  val sexpable : (module Sexpable with type t = 'a) -> 'a t
  val binable_via_base64 : (module Bin_prot.Binable.S with type t = 'a) -> 'a t

  (** [base64_encoded] will decode the URL component before parsing, and encode the URL
      component after unparsing. *)
  val base64_encoded : 'a t -> 'a t

  (** Turns a parser of type 'a to a parser of type 'b.

      Properties:
      {ul {- If the parse_exn function fails, so does the entire parser.}} *)
  val project : 'a t -> parse_exn:('a -> 'b) -> unparse:('b -> 'a) -> 'b t

  (** [fallback parser value] parses into [value] every time that [parser] fails. *)
  val fallback : 'a t -> fallback:'a -> 'a t

  (** Displays a different name when shown in url. e.g. instead of:

      {[
        project (fallback string)
      ]}

      appearing as  "/?q=<project<fallback<string>>>"

      you could do:
      {[
        name "username" (project (fallback string))
      ]}
      and it would appear as "/?q=<username>"
  *)
  val name : string -> 'a t -> 'a t
end

(** Represents parts of a URL.

    Each kind of parser consumes from this "pool" of parts of a url.

    Parsers that consume from the query will lookup the query map, parse, and consume
    the element from the query.

    Parsers that consume from the path will read the path in sequential order, from the
    beginning of the path to the end of the path. Once a path parser, is done parsing,
    it will remove its consumed elements from the path, and the next path parser will
    continue where it left off. *)
module Components : sig
  type t =
    { path : string list (** "foo/bar" -> ["foo"; "bar"] *)
    ; query : string list String.Map.t
    (** "?foo=1&bar=2" -> String.Map.of_alist_exn ["foo", ["1"]; "bar", ["2"]] *)
    }
  [@@deriving sexp]

  val empty : t
end

module Parse_result : sig
  (** [result] is the result of the parsing operation. [remaining] is whatever components
      were left unparsed after parsing was completed. *)
  type 'a t =
    { result : 'a
    ; remaining : Components.t
    }

  val create : 'a -> 'a t
end

module Parser : sig
  (** ['a t] represents a parser that can parse a URL into 'a and unparse 'a into a URL. *)
  type 'a t

  (** Make an expect test with this function! It will make your life easier!

      Runs static checks, and shows all of the url shapes that the parser can parse. It
      lets you know if a URL shape sneakily changes or if there is any ambiguity in
      your parser. It tries its best to suggest possible fixes too. *)
  val check_ok_and_print_urls_or_errors : 'a t -> unit

  (** "Evaluates" a ['a t] into a projection that parses to/from [Components.t] to
      ['a Parse_result.t]. *)
  val eval : 'a t -> (Components.t, 'a Parse_result.t) Projection.t

  (** Like [eval] but parses/unparses from/into a [Uri.t] *)
  val eval_for_uri : 'a t -> (Uri.t, 'a Parse_result.t) Projection.t

  (** Returns a list of the shapes of all the URLs given that the parser can parse. *)
  val all_urls : 'a t -> string list

  (** Like [Value_parser.project], but works at the [Parser.t] level. *)
  val project : 'a t -> parse_exn:('a -> 'b) -> unparse:('b -> 'a) -> 'b t

  (** Runs the given ['a t], if at any point its given parse fails due to a
      missing query_field, this fill parse into [None], otherwise, this will parse
      into [Some (* What 'a t would've parsed to *)].

      When this parses to [None], the components that will be given to the next parse will
      be the same ones that [optional_query_fields] received, as if it were a no-op regardless
      if a subset of the parser suceeded.
  *)
  val optional_query_fields : 'a t -> 'a option t

  (** Looks up a key from components. If it's missing, parsing fails.

      The key is determined with the following priority:
      1. If [?key] is given, then the given key is used.
      2. If the parser is inside of a record or variant parser, then the field
      name/constructor name that the parser name is in will be the key that is used.
      3. Key can't be inferred and an exception will be raised.

      If you use [check_ok_and_print_urls_or_errors], your parser will statically fail
      in an expect test, rather than failing on runtime.
  *)
  val from_query_required : ?key:string -> 'a Value_parser.t -> 'a t

  (** Like [from_query_required], but parses into [None] if key is missing from URL's
      query. *)
  val from_query_optional : ?key:string -> 'a Value_parser.t -> 'a option t

  (** Like [from_query_required], but parses into the ['a] default value that's given to
      it.

      if [equal] is passed over, when the unparsing occurs, if
      [equal curr_value default_value], then the key/value pair is not included in the
      URL. *)
  val from_query_optional_with_default
    :  ?key:string
    -> equal:('a -> 'a -> bool)
    -> 'a Value_parser.t
    -> default:'a
    -> 'a t

  (** Like [from_query_required], but will parse into a list of values rather than just
      one. (e.g. ?foo=1,2,3,4 => [1; 2; 3; 4]) with
      [from_query_many ~key:"foo" Value_parser.int]

      [from_query_many ~key:"q" Value_parser.int] parses into:

      "?q=1,2,3" => [1; 2; 3]
      "?q=1" => [1]
      "?q" => []
      "?" => []
  *)
  val from_query_many : ?key:string -> 'a Value_parser.t -> 'a list t

  (** Like [from_query_many], but fails if there is not a single element in the list. *)
  val from_query_many_at_least_1 : ?key:string -> 'a Value_parser.t -> 'a list t

  (** No-op parser, does not consume anything. Useful for Variants that don't have a
      payload. *)
  val unit : unit t

  (** Reads the next value that is available in the path, parses it, and removes it from
      the path so that the next parsers move on an parse the remainder of the path. *)
  val from_path : 'a Value_parser.t -> 'a t

  (** [from_paths] equivalent of [from_query_many]. Like [from_path] except that instead
      of just taking one element from the path list, it will take everything from the
      path list and run the given ['a Value_parser.t] through it.*)
  val from_remaining_path : 'a Value_parser.t -> 'a list t

  (** [with_prefix prefix next] will fail if the path that it receives does not start
      with [prefix]. If there is a prefix match, then [next] will continue parsing. *)
  val with_prefix : string list -> 'a t -> 'a t

  (** [with_remaining_path] is just like [with_prefix] just that instead of only needing
      the string list that it receives to match like a prefix, the entire path needs to
      equal the string that its given. No more path parsing is allowed inside of the
      parser that's given to [with_remaining_path].
  *)
  val with_remaining_path : string list -> 'a t -> 'a t

  (** Fails to parse if path is not empty. If path is empty, its input parser continues
      parsing. [end_of_path next] is equivalent to [with_remaining_path [] next]. *)
  val end_of_path : 'a t -> 'a t

  module Variant : sig
    module type S = sig
      (** You can derive this module with [@@deriving typed_variants] on t. *)
      module Typed_variant : Typed_variants_lib.S

      (** Given a typed variant, it should return the parser that you want to use for
          that variant's constructor. For example:

          {[

            module My_url = struct
              type t =
                | Foo
                | Bar of int
              [@@deriving typed_variants, sexp, equal]

              let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
                | Foo -> Parser.unit
                | Bar -> Parser.from_path Value_parser.int
            end

            let my_parser = Variant.make (module My_url)
          ]}


          Which variant constructor is used for a given url? The following hierarchy
          exists:

          1. If the given parser has a "constant" part of a path, like [with_prefix] or
          [with_remaining_path], then the very next one in the parse chain is used.
          2. If the given parser does not have any "constant" parts of a path, then the
          constructors name is used.
          3. If two constructors, both match, the one with most specificity is used (i.e
          longest match first and if tie, then from_remaining_path wins over
          with_prefix). Furthermore, if there is a "tie", then this is a static error.
      *)
      val parser_for_variant : 'a Typed_variant.t -> 'a t
    end

    (** Makes a parser for 'a where 'a is a Variant.

        For documentation/examples on the ?namespace optional parameter, please read the
        documentation on Record.make
    *)
    val make
      :  ?namespace:string list
      -> (module S with type Typed_variant.derived_on = 'a)
      -> 'a t
  end

  module Record : sig
    (** Path order is one of the scarier parts of the API and is explained further in the
        explanation for [S.path_order]. *)
    module Path_order (M : Typed_fields_lib.S) : sig
      type 'a t' =
        | [] : unit t'
        | ( :: ) : 'a M.t * 'b t' -> ('a -> 'b) t'

      type t = T : 'a t' -> t
    end

    module type S = sig
      (** You can derive this module with [@@deriving typed_fields] on t. *)
      module Typed_field : Typed_fields_lib.S

      (**
         Given a typed field (you can get this by [@@deriving typed_fields] on t),
         it should return the parser that you want to use for that field. For example:

         {[
           module My_url = struct
             type t =
               { foo : int
               ; bar : int option
               } [@@deriving typed_fields, sexp, equal]

             let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
               | Foo -> Parser.from_query_required Value_parser.int
               | Bar -> Parser.from_query_optional Value_parser.int

             let path_order = []
           end

           let my_parser = Record.make (module My_url)
         ]}
      *)
      val parser_for_field : 'a Typed_field.t -> 'a t

      (** Path order is one of the more uglier, verbose parts of the API, but it solves
          a problem that's hard to solve otherwise: Which order to parse record fields?

          1. A record has multiple fields, so it needs to run multiple parsers.
          2. Some parsers need to parse, and more imporantly "consume" things from the
          path.
          3. Because of this, ordering matters, (e.g. "<int>/<string>" is different from
          "<string>/int")

          [@@deriving typed_fields] gives enough information to solve these questions by
          the order that the fields are declared in, but it makes overriding things
          weird + prone to mysteriously change if the ordering of the fields in the type
          definition is changed/new fields are added in the future.

          [path_order] allows you to explicitly state the order in which the path
          fields' parsers should run:

          {[
            module My_url = struct
              type t =
                { foo : int
                ; bar : string
                } [@@deriving typed_fields, sexp, equal]

              let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
                | Foo -> Parser.from_path Value_parser.int
                | Bar -> Parser.from_path Value_parser.string


              module Path_order = Record.Path_order (Typed_field)
              let path_order = Path_order.T [Foo; Bar]
            end
          ]}

          [Foo; Bar] -> "/<int>/<string>"
          [Bar; Foo] -> "/<string>/<int>"

          The reason that you need to call a functor is complicated, but it's like
          syntactic sugar for: [{Typed_field.Packed.f = T Foo}; {f = T Bar}]

          Rules! (No need to remember these rules if you have an expect test with
          [check_ok_and_print_urls_or_errors])
          - Only the fields that need things from path should be in [path_order].
          - All of the fields that need things from path must be in [path_order].
          - No duplicates allowed.
      *)
      val path_order : Path_order(Typed_field).t
    end

    (** Makes a ['a t] where 'a is a record.

        When dealing with nested records, namespaces are automatically inferred, but can
        be overriden with the optional namespace parameter.

        Example:
        {[
          module Nested = struct
            type t =
              { foo : int
              ; bar : int
              }
            [@@deriving typed_fields, sexp, equal]

            let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
              | Foo -> Parser.from_query_required Value_parser.int
              | Bar -> Parser.from_query_required Value_parser.int
            ;;

            let path_order = []
          end

          module My_url = struct
            type t =
              { a : Nested.t
              ; b : Nested.t
              }
            [@@deriving typed_fields, sexp, equal]

            let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
              | A -> Parser.Record.make (module Nested)
              | B -> Parser.Record.make (module Nested)
            ;;

            let path_order = []
          end

          let%expect_test _ =
            let parser = Parser.Record.make (module My_url) in
            Parser.check_ok_and_print_urls_or_errors parser;
            [%expect
              {|
    URL parser looks good!
    ┌───────────────────────────────────────────────────┐
    │ All urls                                          │
    ├───────────────────────────────────────────────────┤
    │ /?a.bar=<int>&a.foo=<int>&b.bar=<int>&b.foo=<int> │
    └───────────────────────────────────────────────────┘ |}]
          ;;

          (*Doing this instead results in: *)
          let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
            | A -> Parser.Record.make ~namespace:[ "my_a" ] (module Nested)
            | B -> Parser.Record.make ~namespace:[ "my_b" ] (module Nested)
          ;;

          let%expect_test _ =
            Parser.check_ok_and_print_urls_or_errors parser;
            [%expect
              {|
    URL parser looks good!
    ┌───────────────────────────────────────────────────────────────┐
    │ All urls                                                      │
    ├───────────────────────────────────────────────────────────────┤
    │ /?my_a.bar=<int>&my_a.foo=<int>&my_b.bar=<int>&my_b.foo=<int> │
    └───────────────────────────────────────────────────────────────┘ |}]
        ]}
    *)
    val make
      :  ?namespace:string list
      -> (module S with type Typed_field.derived_on = 'a)
      -> 'a t
  end

  val sexp_of_t : 'a t -> Sexp.t
end

module Versioned_parser : sig
  type 'a t

  (** Like [Parser.check_ok_and_print_urls_or_errors] but for [Versioned_parser]. *)
  val check_ok_and_print_urls_or_errors : 'a t -> unit

  (** Are you migrating your site to use [Url_var]'s [Typed] API and you don't want to
      break your existing links? Use [of_non_typed_parser] instead of [first_parser]. *)
  val of_non_typed_parser : (Components.t, 'a) Projection.t -> 'a t

  (** Your site's first URL. Analogous to a list with one element. *)
  val first_parser : 'a Parser.t -> 'a t

  (** Need to change your URL? Use [new_parser] to "fallback" to [previous] if the new
      parser can't recognize a URL. If [previous] success and parses into [result], then
      [f result] will be returned. Analogous to [cons] in a list. *)
  val new_parser : 'new_ Parser.t -> previous:'prev t -> f:('prev -> 'new_) -> 'new_ t

  (** Like [Parser.eval] but for a ['a Versioned_parser.t]. *)
  val eval : 'a t -> (Components.t, 'a Parse_result.t) Projection.t

  (** Like [Parser.eval_for_uri] but for ['a Versioned_parser.t] *)
  val eval_for_uri : 'a t -> (Uri.t, 'a Parse_result.t) Projection.t

  (** Like [Parser.all_urls], but for ['a Versioned_parser.t]. *)
  val all_urls : 'a t -> string list
end
