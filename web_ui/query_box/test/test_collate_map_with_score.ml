open! Core
open Bonsai_test
open Bonsai.For_open
open Bonsai.Let_syntax
module Collate_map_with_score = Bonsai_web_ui_query_box.Collate_map_with_score

module Common (M : sig
    module Key : sig
      type t [@@deriving sexp_of]

      val get_original_key : t -> string

      include Comparator.S with type t := t
    end

    val collate
      :  preprocess:(key:string -> data:'v -> 'preprocessed)
      -> score:('query -> 'preprocessed -> int)
      -> query_is_as_strict:('query -> as_:'query -> bool)
      -> to_result:('preprocessed -> key:string -> data:'v -> 'result)
      -> (string, 'v, 'cmp) Map.t Value.t
      -> 'query Value.t
      -> 'result Map.M(Key).t Computation.t
  end) =
struct
  let%expect_test _ =
    let query_var = Bonsai.Var.create "" in
    let input = String.Map.of_alist_exn [ "abc", "def"; "abd", "ghi"; "zabd", "jkl" ] in
    let c =
      let%sub query =
        let%arr query = Bonsai.Var.value query_var in
        query, Fuzzy_search.Query.create query
      in
      M.collate
        (Value.return input)
        query
        ~preprocess:(fun ~key ~data -> [%string "%{key} %{data}"])
        ~score:(fun (_, query) item -> Fuzzy_search.score query ~item)
        ~query_is_as_strict:(fun (q, _) ~as_:(as_, _) ->
          String.is_substring q ~substring:as_)
        ~to_result:(fun preprocessed ~key:_ ~data:_ -> preprocessed)
    in
    let handle =
      Handle.create
        (Result_spec.sexp
           (module struct
             type t = string Map.M(M.Key).t

             let sexp_of_t t =
               Map.sexp_of_m__t
                 (module struct
                   type t = M.Key.t

                   let sexp_of_t t = String.sexp_of_t (M.Key.get_original_key t)
                 end)
                 String.sexp_of_t
                 t
             ;;
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

end

(* The fast implementation. *)
module _ = Common (struct
    module Key = struct
      include Collate_map_with_score.Scored_key.M (String)

      let get_original_key (_, s) = s
    end

    let collate = Collate_map_with_score.collate
  end)

(* The naive reference implementation. *)
module _ = Common (struct
    module Key = struct
      module T = struct
        type t = int * string [@@deriving compare, sexp_of]
      end

      include T
      include Comparator.Make (T)

      let get_original_key (_, s) = s
    end

    let collate ~preprocess ~score ~query_is_as_strict:_ ~to_result input query =
      let%arr input = input
      and query = query in
      Map.to_alist input
      |> List.map ~f:(fun (key, data) ->
        let preprocessed = preprocess ~key ~data in
        let score = score query preprocessed in
        (score, key), to_result preprocessed ~key ~data)
      |> List.filter ~f:(fun ((score, _), _) -> score <> 0)
      |> Map.of_alist_exn (module Key)
    ;;
  end)
