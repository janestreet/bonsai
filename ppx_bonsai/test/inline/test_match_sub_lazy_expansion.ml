open! Core
open Ppxlib
open Test_util

let locality = Ppx_let_expander.Locality.global
let loc = Location.none

let print_expr expr =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    expr
  |> print_expr
;;

let print_diff expr1 expr2 =
  let expect_output_without_pattern_ids () =
    Expect_test_helpers_base.expect_test_output ()
    |> Re.replace_string
         ~all:true
         Re.(seq [ str "_"; repn digit 3 (Some 3); str "_" ] |> compile)
         ~by:"_REPLACED_ID_"
  in
  print_expr expr1;
  let output1 = expect_output_without_pattern_ids () in
  print_expr expr2;
  let output2 = expect_output_without_pattern_ids () in
  Expect_test_patdiff.print_patdiff output1 output2
;;

(* These tests verify that adding [%lazy] before a [match%sub] is equivalent to wrapping
   all of the match branches with calls to [Let_syntax.delay]. *)

let%expect_test "automatically delays a single branch" =
  let c_old =
    [%expr
      match EXPR with
      | A -> Let_syntax.delay graph ~f:(fun graph -> BRANCH)]
  in
  let c_new =
    [%expr
      match [%lazy] EXPR with
      | A -> BRANCH]
  in
  print_expr c_new;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return EXPR)
        ~f:(fun __pattern_syntax__001_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((Let_syntax.map
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__001_ ~f:(function | A -> ()))
                     [@nontail ]))
                  ~f:(fun _ -> ((Let_syntax.delay graph ~f:(fun graph -> BRANCH))
                        [@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}];
  print_diff c_old c_new;
  [%expect {| |}]
;;

let%expect_test "automatically delays multiple branches" =
  let c_old =
    [%expr
      match EXPR with
      | A -> Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
      | B -> Let_syntax.delay graph ~f:(fun graph -> BRANCH2)]
  in
  let c_new =
    [%expr
      match [%lazy] EXPR with
      | A -> BRANCH1
      | B -> BRANCH2]
  in
  print_expr c_new;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return EXPR)
        ~f:(fun __pattern_syntax__004_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__004_
                              ~f:(function | A -> 0 | B -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
                          | ((1)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH2)
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}];
  print_diff c_old c_new;
  [%expect {| |}]
;;

let%expect_test "function application still parses correctly" =
  let c_old =
    [%expr
      match f a b c with
      | A -> Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
      | B -> Let_syntax.delay graph ~f:(fun graph -> BRANCH2)]
  in
  let c_new =
    [%expr
      match [%lazy] f a b c with
      | A -> BRANCH1
      | B -> BRANCH2]
  in
  print_expr c_new;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return (f a b c))
        ~f:(fun __pattern_syntax__007_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__007_
                              ~f:(function | A -> 0 | B -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
                          | ((1)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH2)
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}];
  print_diff c_old c_new;
  [%expect {| |}]
;;

let%expect_test "constructors with arguments still parse correctly" =
  let c_old =
    [%expr
      match Some x with
      | A -> Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
      | B -> Let_syntax.delay graph ~f:(fun graph -> BRANCH2)]
  in
  let c_new =
    [%expr
      match [%lazy] Some x with
      | A -> BRANCH1
      | B -> BRANCH2]
  in
  print_expr c_new;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return (Some x))
        ~f:(fun __pattern_syntax__010_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__010_
                              ~f:(function | A -> 0 | B -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
                          | ((1)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH2)
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}];
  print_diff c_old c_new;
  [%expect {| |}]
;;

let%expect_test "variants with arguments still parse correctly" =
  let c_old =
    [%expr
      match `P x with
      | A -> Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
      | B -> Let_syntax.delay graph ~f:(fun graph -> BRANCH2)]
  in
  let c_new =
    [%expr
      match [%lazy] `P x with
      | A -> BRANCH1
      | B -> BRANCH2]
  in
  print_expr c_new;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return (`P x))
        ~f:(fun __pattern_syntax__013_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__013_
                              ~f:(function | A -> 0 | B -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
                          | ((1)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH2)
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}];
  print_diff c_old c_new;
  [%expect {| |}]
;;

let%expect_test "tuples still parse correctly" =
  let c_old =
    [%expr
      match EXPR1, EXPR2 with
      | A -> Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
      | B -> Let_syntax.delay graph ~f:(fun graph -> BRANCH2)]
  in
  let c_new =
    [%expr
      match [%lazy] EXPR1, EXPR2 with
      | A -> BRANCH1
      | B -> BRANCH2]
  in
  print_expr c_new;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.return
           (let __let_syntax__018_ = ((EXPR1)
              [@ppxlib.enter_value __ppx_bonsai_tuple__016_])[@@ppxlib.do_not_enter_value
                                                               ]
            and __let_syntax__019_ = ((EXPR2)
              [@ppxlib.enter_value __ppx_bonsai_tuple__017_])[@@ppxlib.do_not_enter_value
                                                               ] in
            Let_syntax.map2
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__018_ __let_syntax__019_
              ~f:(fun __ppx_bonsai_tuple__016_ __ppx_bonsai_tuple__017_ ->
                    (__ppx_bonsai_tuple__016_, __ppx_bonsai_tuple__017_))))
        ~f:(fun __pattern_syntax__022_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__022_
                              ~f:(function | A -> 0 | B -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH1)
                          | ((1)[@merlin.hide ]) ->
                              Let_syntax.delay graph ~f:(fun graph -> BRANCH2)
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}];
  print_diff c_old c_new;
  [%expect {| |}]
;;

let%expect_test "change a graph name (single branch)" =
  let c_old =
    [%expr
      match EXPR with
      | A -> Let_syntax.delay bad_graph_name ~f:(fun bad_graph_name -> BRANCH)]
  in
  let c_new =
    [%expr
      match [%lazy bad_graph_name] EXPR with
      | A -> BRANCH]
  in
  print_expr c_new;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return EXPR)
        ~f:(fun __pattern_syntax__037_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((Let_syntax.map
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__037_ ~f:(function | A -> ()))
                     [@nontail ]))
                  ~f:(fun _ ->
                        ((Let_syntax.delay bad_graph_name
                            ~f:(fun bad_graph_name -> BRANCH))
                        [@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}];
  print_diff c_old c_new;
  [%expect {| |}]
;;

let%expect_test "change graph name (multiple branches)" =
  let c_old =
    [%expr
      match EXPR with
      | A -> Let_syntax.delay bad_graph_name ~f:(fun bad_graph_name -> BRANCH1)
      | B -> Let_syntax.delay bad_graph_name ~f:(fun bad_graph_name -> BRANCH2)]
  in
  let c_new =
    [%expr
      match [%lazy bad_graph_name] EXPR with
      | A -> BRANCH1
      | B -> BRANCH2]
  in
  print_expr c_new;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return EXPR)
        ~f:(fun __pattern_syntax__040_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__040_
                              ~f:(function | A -> 0 | B -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              Let_syntax.delay bad_graph_name
                                ~f:(fun bad_graph_name -> BRANCH1)
                          | ((1)[@merlin.hide ]) ->
                              Let_syntax.delay bad_graph_name
                                ~f:(fun bad_graph_name -> BRANCH2)
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}];
  print_diff c_old c_new;
  [%expect {| |}]
;;
