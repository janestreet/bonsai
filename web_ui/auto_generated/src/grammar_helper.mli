open! Core

module Tags : sig
  type t

  val strip_tags : 'a Sexp_grammar.with_tag_list -> 'a
  val collect_and_strip_tags : 'a Sexp_grammar.with_tag_list -> 'a * t
  val find_doc_tag : t -> string option
  val is_empty : t -> bool
end
