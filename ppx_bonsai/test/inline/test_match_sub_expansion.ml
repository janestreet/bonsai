open! Core
open Ppxlib
open Test_util

let locality = Ppx_let_expander.Locality.global
let loc = Location.none

let test expr =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    expr
  |> print_expr
;;

(* This test shows the difference in expansion when there are match%sub arms with and
   without type variables. We already have implicit tests for this, but this test shows
   that we generate additional expansion code for the match arms that have bound
   variables. *)

let%expect_test "No bound variables" =
  test
    [%expr
      match EXPR with
      | Foo1 _ -> ARM1
      | Foo2 _ -> ARM2];
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
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__001_
                              ~f:(function
                                  | Foo1 ((_)[@merlin.focus ]) -> 0
                                  | Foo2 ((_)[@merlin.focus ]) -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) -> ARM1
                          | ((1)[@merlin.hide ]) -> ARM2
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "1 bound variables" =
  test
    [%expr
      match EXPR with
      | Foo1 a_variable -> ARM1
      | Foo2 a_variable -> ARM2];
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return EXPR)
        ~f:(fun __pattern_syntax__002_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__002_
                              ~f:(function
                                  | Foo1 a_variable -> 0
                                  | Foo2 a_variable -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
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
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 0;
                                                 pos_cnum = (-1);
                                                 pos_bol = 0
                                               } __pattern_syntax__002_
                                         ~f:((function
                                              | Foo1 __pattern_syntax__003_ ->
                                                  __pattern_syntax__003_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun a_variable -> ((ARM1)[@nontail ])))
                              [@nontail ])
                          | ((1)[@merlin.hide ]) ->
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
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 0;
                                                 pos_cnum = (-1);
                                                 pos_bol = 0
                                               } __pattern_syntax__002_
                                         ~f:((function
                                              | Foo2 __pattern_syntax__004_ ->
                                                  __pattern_syntax__004_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun a_variable -> ((ARM2)[@nontail ])))
                              [@nontail ])
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}]
;;
