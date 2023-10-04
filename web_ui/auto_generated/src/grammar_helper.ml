open! Core

module Tags = struct
  type t = Sexp.t String.Map.t

  let rec strip_tags : type a. a Sexp_grammar.with_tag_list -> a = function
    | Sexp_grammar.No_tag a -> a
    | Tag { grammar; _ } -> strip_tags grammar
  ;;

  let collect_and_strip_tags (type a) (grammar : a Sexp_grammar.with_tag_list) =
    let rec recurse accum = function
      | Sexp_grammar.No_tag a -> a, accum
      | Tag { key; value; grammar } -> recurse (Map.set accum ~key ~data:value) grammar
    in
    recurse String.Map.empty grammar
  ;;

  let find_doc_tag t =
    let%map.Option sexp = Map.find t Ppx_sexp_conv_lib.Sexp_grammar.doc_comment_tag in
    match sexp with
    | Sexp.Atom str -> str
    | List _ -> Sexp.to_string_hum sexp
  ;;

  let is_empty = Map.is_empty
end
