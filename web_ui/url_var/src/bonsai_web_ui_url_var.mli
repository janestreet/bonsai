open! Core
open! Bonsai_web

module Components : sig
  type t =
    { path : string
    ; query : string list String.Map.t
    ; fragment : string option
    }
  [@@deriving sexp, equal]

  val create
    :  ?path:string
    -> ?query:string list String.Map.t
    -> ?fragment:string option
    -> unit
    -> t


  (** Creates a URI with the the path, query, and fragment added to the URI. *)
  val to_path_and_query : t -> Uri.t

  val of_uri : Uri.t -> t
  val empty : t
end

module Original_components := Components

module type T = sig
  type t [@@deriving sexp, equal]
end

(** Types that implement [S] can be parsed from a url and unparsed into a url. *)
module type S = sig
  include T

  val parse_exn : Components.t -> t
  val unparse : t -> Components.t
end

module type S_via_sexp = sig
  type t [@@deriving sexp, equal]
end

(** Types that are sexpable can be turned a URL by storing the value as a sexp in a query
    parameter. *)
module Literally_just_a_gigantic_sexp (M : S_via_sexp) : S with type t = M.t

type 'a t

(** Creating a url-var involves passing in a module that implements [S], but also a
    fallback value which will be used if [S.parse_exn] throws. *)
val create_exn : (module S with type t = 'a) -> fallback:'a -> 'a t

(** [set] updates the contents of the url-var as well as the current browser
    location. When [how] is `Push (which is the default), it will add
    a new entry to the top of the browser's history stack, but `Replace
    will cause it to replace the top entry of the history stack with the new URL. *)
val set : ?how:[ `Push | `Replace ] -> 'a t -> 'a -> unit

(** [update] is like [set], but gives you access to the previous value of the url right
    before setting it.*)
val update : ?how:[ `Push | `Replace ] -> 'a t -> f:('a -> 'a) -> unit

val update_effect : ?how:[ `Push | `Replace ] -> 'a t -> f:('a -> 'a) -> unit Effect.t
val get : 'a t -> 'a
val value : 'a t -> 'a Value.t
val incr : 'a t -> 'a Ui_incr.t

(** By asking for the [Url_var.t]'s effect, you get a function that can be easily threaded
    through your components and triggered inside an action-application or inside of an
    event listener. *)
val set_effect : ?how:[ `Push | `Replace ] -> 'a t -> 'a -> unit Effect.t

type 'a url_var = 'a t

(** This module allows you to define your Url_var out of an OCaml type similar to the
    Typed Forms API. The module provides nice features like static analysis of your
    parser, ambiguity checking and functions to make backwards compatibility easier.
    Please read [../README.mdx] first to make reading this MLI easier.

    Most of this module lives in [lib/uri_parsing], a separate library. This is weird, but
    since URL Var depends on html5_history, it can only be ran in a JavaScript/browser
    context. This split allows you to run the same parsing/unparsing logic on both the
    server and client which is useful if you're thinking about generating url's/doing http
    server route handling. *)
module Typed : sig
  module Components : sig
    type t = Uri_parsing.Components.t

    val empty : t

    val to_original_components
      :  ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
      -> t
      -> Components.t

    val of_original_components
      :  ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
      -> Components.t
      -> t
  end

  module Value_parser = Uri_parsing.Value_parser
  module Parser = Uri_parsing.Parser

  module Versioned_parser : sig
    type 'a t = 'a Uri_parsing.Versioned_parser.t

    (** Your site's first URL. Analogous to a list with one element. *)
    val first_parser : 'a Parser.t -> 'a t

    (** Are you migrating your site to use [Url_var]'s [Typed] API and you don't want to
        break your existing links? Use [of_non_typed_parser] instead of [first_parser]. *)
    val of_non_typed_parser
      :  ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
      -> parse_exn:(Original_components.t -> 'a)
      -> unparse:('a -> Original_components.t)
      -> unit
      -> 'a t

    (** Need to change your URL? Use [new_parser] to "fallback" to [previous] if the new
        parser can't recognize a URL. If [previous] success and parses into [result], then
        [f result] will be returned. Analogous to [cons] in a list. *)
    val new_parser : 'new_ Parser.t -> previous:'prev t -> f:('prev -> 'new_) -> 'new_ t

    (** Like [Parser.check_ok_and_print_urls_or_errors] but for [Versioned_parser]. *)
    val check_ok_and_print_urls_or_errors : 'a t -> unit
  end

  (** [make (module My_url) my_url_parser ~fallback] is a [My_url.t Url_var] that
      parses/unparses the current url into My_url.t, and when parsing fails it will return

      [fallback] is used to potentially give out custom error messages if the parser
      fails. Please don't write parsing logic in fallback that might fail since there's no
      fallback for the fallback (i.e. if [fallback] fails, the page would crash.)

      [redirect] is handy if you have an old URL shape that you want to immediately
      redirect into a new URL (e.g. A change in your site might involve changing URLs like
      "my-site.com/post/best-aquatic-plants" to
      "my-site.com/user/capybara_lover_123/post/best-aquatic-plants"). Rather than
      breaking references to the aquatic plants blog, it'd be nice to redirect them to the
      new page.

      Unfortunately the halting problem remains unsolved, so we only have reasonably naive
      limit on the number of redirects before redirects stop.
  *)
  val make
    :  ?on_fallback_raises:'a
    -> ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
    -> (module T with type t = 'a)
    -> 'a Versioned_parser.t
    -> fallback:(Exn.t -> Original_components.t -> 'a)
    -> 'a url_var

  module Projection = Uri_parsing.Projection

  (** [make_projection] makes extracting the parsing/unparsing logic alone without it
      being attached to an actual URL-var. This might be useful if you're writing custom
      tests of your own! *)
  val make_projection
    :  ?on_fallback_raises:'a
    -> ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
    -> 'a Versioned_parser.t
    -> fallback:(Exn.t -> Original_components.t -> 'a)
    -> (Original_components.t, 'a) Projection.t

  (** Creates a URL of everything after the domain name. This might be useful if you're
      sending URLs in emails or need to automatically create URLs. *)
  val to_url_string
    :  ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
    -> 'a Parser.t
    -> 'a
    -> string
end

module For_testing : sig
  module Parse_result = Uri_parsing.Parse_result

  module Projection : sig
    type 'a t

    val make
      :  ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
      -> 'a Typed.Parser.t
      -> 'a t

    val make_of_versioned_parser
      :  ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
      -> 'a Typed.Versioned_parser.t
      -> 'a t

    val parse_exn : 'a t -> Typed.Components.t -> 'a Parse_result.t
    val unparse : 'a t -> 'a Parse_result.t -> Typed.Components.t
  end
end
