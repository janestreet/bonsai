open! Core
open Bonsai_test
open Bonsai.For_open
open Bonsai.Let_syntax
module Collate_map_with_score = Bonsai_web_ui_query_box.Collate_map_with_score

let reference_implementation
      ~preprocess
      ~score
      ~query_is_as_strict:_
      ~to_result
      input
      query
  =
  let%arr input = input
  and query = query in
  Map.to_alist input
  |> List.map ~f:(fun (key, data) ->
    let preprocessed = preprocess ~key ~data in
    let score = score query preprocessed in
    (score, key), to_result preprocessed ~key ~data)
  |> List.filter ~f:(fun ((score, _), _) -> score <> 0)
  |> List.sort
       ~compare:(Comparable.lift [%compare: int] ~f:(fun ((score, _), _) -> score))
  |> List.map ~f:(fun ((_score, key), data) -> key, data)
;;

let real_implementation ~preprocess ~score ~query_is_as_strict ~to_result input query =
  let%sub map =
    Collate_map_with_score.collate
      (module String)
      ~preprocess
      ~score
      ~query_is_as_strict
      ~to_result
      input
      query
  in
  let%arr map = map in
  Map.to_alist map |> List.map ~f:(fun ((_score, key), data) -> key, data)
;;

(* This is just like the real implementation, but with an extra transformation
   through [Bonsai.assoc], to check that the map comparator is well-behaved. *)
let real_implementation2 ~preprocess ~score ~query_is_as_strict ~to_result input query =
  let%sub map =
    Collate_map_with_score.collate
      (module String)
      ~preprocess
      ~score
      ~query_is_as_strict
      ~to_result
      input
      query
  in
  let%sub map =
    Bonsai.assoc
      (module Collate_map_with_score.Scored_key.M (String))
      map
      ~f:(fun _key data -> return data)
  in
  let%arr map = map in
  Map.to_alist map |> List.map ~f:(fun ((_score, key), data) -> key, data)
;;

let checked_implementation ~preprocess ~score ~query_is_as_strict ~to_result input query =
  let%sub reference =
    reference_implementation ~preprocess ~score ~query_is_as_strict ~to_result input query
  in
  let%sub real =
    real_implementation ~preprocess ~score ~query_is_as_strict ~to_result input query
  in
  let%sub real2 =
    real_implementation2 ~preprocess ~score ~query_is_as_strict ~to_result input query
  in
  let%arr reference = reference
  and real = real
  and real2 = real2 in
  if not ([%equal: (string * string) list] real reference)
  then
    raise_s
      [%message
        "real2 and reference are different"
          (real : (string * string) list)
          (reference : (string * string) list)];
  if not ([%equal: (string * string) list] real2 reference)
  then
    raise_s
      [%message
        "real2 and reference are different"
          (real2 : (string * string) list)
          (reference : (string * string) list)];
  real
;;

let fuzzy_search_component input query =
  let%sub query =
    let%arr query = query in
    query, Fuzzy_search.Query.create query
  in
  checked_implementation
    input
    query
    ~preprocess:(fun ~key ~data -> [%string "%{key} %{data}"])
    ~score:(fun (_, query) item -> Fuzzy_search.score query ~item)
    ~query_is_as_strict:(fun (q, _) ~as_:(as_, _) -> String.is_substring q ~substring:as_)
    ~to_result:(fun preprocessed ~key:_ ~data:_ -> preprocessed)
;;

let%expect_test _ =
  let query_var = Bonsai.Var.create "" in
  let input = String.Map.of_alist_exn [ "abc", "def"; "abd", "ghi"; "zabd", "jkl" ] in
  let c = fuzzy_search_component (Value.return input) (Bonsai.Var.value query_var) in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = (string * string) list [@@deriving sexp_of]
         end))
      c
  in
  Handle.show handle;
  [%expect {| ((abc "abc def") (abd "abd ghi") (zabd "zabd jkl")) |}];
  Bonsai.Var.set query_var "abd";
  Handle.show handle;
  [%expect {| ((abd "abd ghi") (abc "abc def") (zabd "zabd jkl")) |}];
  Bonsai.Var.set query_var "ab d";
  Handle.show handle;
  [%expect {| ((abc "abc def") (abd "abd ghi") (zabd "zabd jkl")) |}];
  Bonsai.Var.set query_var "ab dg";
  Handle.show handle;
  [%expect {| ((abd "abd ghi")) |}];
  Bonsai.Var.set query_var "ab d";
  Handle.show handle;
  [%expect {| ((abc "abc def") (abd "abd ghi") (zabd "zabd jkl")) |}];
  Bonsai.Var.set query_var "mmm";
  Handle.show handle;
  [%expect {| () |}];
  Bonsai.Var.set query_var "";
  Handle.show handle;
  [%expect {| ((abc "abc def") (abd "abd ghi") (zabd "zabd jkl")) |}];
  Bonsai.Var.set query_var "a";
  Handle.show handle;
  [%expect {| ((abc "abc def") (abd "abd ghi") (zabd "zabd jkl")) |}]
;;

let%expect_test _ =
  let query_var = Bonsai.Var.create "" in
  let input_var =
    Bonsai.Var.create
      (String.Map.of_alist_exn [ "abc", "def"; "abd", "ghi"; "zabd", "jkl" ])
  in
  let c =
    fuzzy_search_component (Bonsai.Var.value input_var) (Bonsai.Var.value query_var)
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = (string * string) list [@@deriving sexp_of]
         end))
      c
  in
  Handle.show handle;
  [%expect {| ((abc "abc def") (abd "abd ghi") (zabd "zabd jkl")) |}];
  Bonsai.Var.set
    input_var
    (String.Map.of_alist_exn [ "123", "456"; "???", "<<<"; "|||", "}}}" ]);
  Handle.show handle;
  [%expect {| ((123 "123 456") (??? "??? <<<") (||| "||| }}}")) |}]
;;

module Action = struct
  type entry_string = string [@@deriving quickcheck, sexp]

  let entry_strings = [ "abcdef"; "abc"; "abdef"; "abf"; "def"; "bf"; "fedcba" ]
  let quickcheck_generator_entry_string = Quickcheck.Generator.of_list entry_strings

  type query_string = string [@@deriving quickcheck, sexp]

  let quickcheck_generator_query_string =
    let char = Quickcheck.Generator.of_list [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ] in
    let%bind.Quickcheck.Generator length = Int.gen_incl 0 7 in
    let%map.Quickcheck.Generator chars = List.gen_with_length length char in
    String.of_char_list chars
  ;;

  type t =
    | Set of entry_string * entry_string
    | Remove of entry_string
    | Set_query of query_string
    | Show
  [@@deriving quickcheck, sexp]
end

let%quick_test _ =
  fun (actions : Action.t list) ->
  let query_var = Bonsai.Var.create "" in
  let input_var = Bonsai.Var.create String.Map.empty in
  let c =
    fuzzy_search_component (Bonsai.Var.value input_var) (Bonsai.Var.value query_var)
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = (string * string) list [@@deriving sexp_of]
         end))
      c
  in
  List.iter actions ~f:(function
    | Set (key, data) -> Bonsai.Var.update input_var ~f:(Map.set ~key ~data)
    | Remove key -> Bonsai.Var.update input_var ~f:(fun map -> Map.remove map key)
    | Set_query query -> Bonsai.Var.set query_var query
    | Show -> Handle.recompute_view handle)
;;
