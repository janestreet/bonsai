open Core
open Ppxlib
open Test_util

let locality = Ppx_let_expander.Locality.global
let loc = Location.none

let%expect_test "single let%sub " =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } ((MY_EXPR)[@ppxlib.enter_value a]) ~f:(fun a -> MY_BODY))
    [@nontail ])
    |}]
;;

let%expect_test "single let%sub - location in scope" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_in_scope)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub ~here ((MY_EXPR)[@ppxlib.enter_value a])
        ~f:(fun a -> MY_BODY))
    [@nontail ])
    |}]
;;

let%expect_test "single pattern sub with modul" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:(Some { txt = Longident.Lident "X"; loc = Location.none })
    ~locality
    [%expr
      let a = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    ((X.Let_syntax.Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } ((MY_EXPR)[@ppxlib.enter_value a]) ~f:(fun a -> MY_BODY))
    [@nontail ])
    |}]
;;

let assert_fails_with_syntax_error ~f =
  try
    ignore (f () : expression);
    assert false
  with
  | ex ->
    Location.Error.of_exn ex
    |> (fun a -> Option.value_exn a)
    |> Location.Error.message
    |> print_endline
;;

let%expect_test "double pattern let%sub" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.sub Location_of_callsite)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      [%expr
        let a = MY_EXPR_1
        and b = MY_EXPR_2 in
        MY_BODY]);
  [%expect {| let%sub should not be used with 'and'. |}]
;;

let%expect_test "disallow let%sub mutable" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.sub Location_of_callsite)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      [%expr
        let mutable a = MY_EXPR_1 in
        MY_BODY]);
  [%expect {| let%sub should not be used with 'mutable'. |}]
;;

let%expect_test "single pattern sub open" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a = MY_EXPR_1 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } ((MY_EXPR_1)[@ppxlib.enter_value a]) ~f:(fun a -> MY_BODY))
    [@nontail ])
    |}]
;;

let%expect_test "double pattern map open" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.sub Location_of_callsite)
      Ppx_let_expander.Extension_kind.default_open
      ~modul:None
      ~locality
      [%expr
        let a = MY_EXPR_1
        and b = MY_EXPR_2 in
        MY_BODY]);
  [%expect {| let%sub should not be used with 'and'. |}]
;;

let%expect_test "while%sub is banned" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.sub Location_of_callsite)
      Ppx_let_expander.Extension_kind.default_open
      ~modul:None
      ~locality
      [%expr
        while a = MY_EXPR_1 do
          MY_BODY
        done]);
  [%expect {| while%sub is not supported |}]
;;

let%expect_test "if%sub is supported" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr if MY_EXPR_1 then BODY_1 else BODY_2]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return MY_EXPR_1)
        ~f:(fun __pattern_syntax__005_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__005_
                              ~f:(function | true -> 0 | false -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) -> BODY_1
                          | ((1)[@merlin.hide ]) -> BODY_2
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "very simple match%sub" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match MY_EXPR_1 with
      | a -> BODY_1]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return MY_EXPR_1) ~f:(fun a -> BODY_1))
    [@nontail ])
    |}]
;;

let%expect_test "destructuring let%sub" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c } = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } MY_EXPR
        ~f:(fun __pattern_syntax__007_ ->
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
                               } __pattern_syntax__007_
                         ~f:(function
                             | (_, { b = _; c = __pattern_syntax__010_ }) ->
                                 __pattern_syntax__010_))[@merlin.hide ]))
                  ~f:(fun c ->
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
                                         } __pattern_syntax__007_
                                   ~f:(function
                                       | (_,
                                          { b = __pattern_syntax__009_; c = _ })
                                           -> __pattern_syntax__009_))
                               [@merlin.hide ]))
                            ~f:(fun b ->
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
                                                   } __pattern_syntax__007_
                                             ~f:(function
                                                 | (__pattern_syntax__008_,
                                                    { b = _; c = _ }) ->
                                                     __pattern_syntax__008_))
                                         [@merlin.hide ]))
                                      ~f:(fun a -> ((MY_BODY)[@nontail ])))
                                  [@nontail ])))
                        [@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "destructuring let%sub (location in scope)" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_in_scope)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c } = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub ~here MY_EXPR
        ~f:(fun __pattern_syntax__011_ ->
              ((Let_syntax.sub ~here
                  (Let_syntax.return
                     ((Let_syntax.map ~here __pattern_syntax__011_
                         ~f:(function
                             | (_, { b = _; c = __pattern_syntax__014_ }) ->
                                 __pattern_syntax__014_))[@merlin.hide ]))
                  ~f:(fun c ->
                        ((Let_syntax.sub ~here
                            (Let_syntax.return
                               ((Let_syntax.map ~here __pattern_syntax__011_
                                   ~f:(function
                                       | (_,
                                          { b = __pattern_syntax__013_; c = _ })
                                           -> __pattern_syntax__013_))
                               [@merlin.hide ]))
                            ~f:(fun b ->
                                  ((Let_syntax.sub ~here
                                      (Let_syntax.return
                                         ((Let_syntax.map ~here
                                             __pattern_syntax__011_
                                             ~f:(function
                                                 | (__pattern_syntax__012_,
                                                    { b = _; c = _ }) ->
                                                     __pattern_syntax__012_))
                                         [@merlin.hide ]))
                                      ~f:(fun a -> ((MY_BODY)[@nontail ])))
                                  [@nontail ])))
                        [@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "destructuring let%sub (comparing location of callsite vs location in \
                 scope)"
  =
  let test location_behavior =
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.sub location_behavior)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      [%expr
        let a, { b; c } = MY_EXPR in
        MY_BODY]
    |> Pprintast.string_of_expression
    |> Re.replace_string
         (Re.compile
            (Re.seq
               [ Re.str "__pattern_syntax__"; Re.digit; Re.digit; Re.digit; Re.str "_" ]))
         ~by:"__pattern_syntax__ID_REPLACED_IN_TEST_"
  in
  Expect_test_patdiff.print_patdiff (test Location_of_callsite) (test Location_in_scope);
  [%expect
    {|
    === DIFF HUNK ===
    -|((Let_syntax.sub
    -|    ~here:{
    -|            Ppx_here_lib.pos_fname = "_none_";
    -|            pos_lnum = 0;
    -|            pos_cnum = (-1);
    -|            pos_bol = 0
    -|          } MY_EXPR
    +|((Let_syntax.sub ~here MY_EXPR
          ~f:(fun __pattern_syntax__ID_REPLACED_IN_TEST_ ->
    -|          ((Let_syntax.sub
    -|              ~here:{
    -|                      Ppx_here_lib.pos_fname = "_none_";
    -|                      pos_lnum = 0;
    -|                      pos_cnum = (-1);
    -|                      pos_bol = 0
    -|                    }
    +|          ((Let_syntax.sub ~here
    -|              (Let_syntax.return
    -|                 ((Let_syntax.map
    -|                     ~here:{
    -|                             Ppx_here_lib.pos_fname = "_none_";
    -|                             pos_lnum = 0;
    -|                             pos_cnum = (-1);
    -|                             pos_bol = 0
    -|                           } __pattern_syntax__ID_REPLACED_IN_TEST_
    +|              (Let_syntax.return
    +|                 ((Let_syntax.map ~here __pattern_syntax__ID_REPLACED_IN_TEST_
                           ~f:(function
                               | (_, { b = _; c = __pattern_syntax__ID_REPLACED_IN_TEST_ }) ->
                                   __pattern_syntax__ID_REPLACED_IN_TEST_))[@merlin.hide ]))
                    ~f:(fun c ->
    -|                    ((Let_syntax.sub
    -|                        ~here:{
    -|                                Ppx_here_lib.pos_fname = "_none_";
    -|                                pos_lnum = 0;
    -|                                pos_cnum = (-1);
    -|                                pos_bol = 0
    -|                              }
    +|                    ((Let_syntax.sub ~here
    -|                        (Let_syntax.return
    -|                           ((Let_syntax.map
    -|                               ~here:{
    -|                                       Ppx_here_lib.pos_fname = "_none_";
    -|                                       pos_lnum = 0;
    -|                                       pos_cnum = (-1);
    -|                                       pos_bol = 0
    -|                                     } __pattern_syntax__ID_REPLACED_IN_TEST_
    +|                        (Let_syntax.return
    +|                           ((Let_syntax.map ~here __pattern_syntax__ID_REPLACED_IN_TEST_
                                     ~f:(function
                                         | (_,
                                            { b = __pattern_syntax__ID_REPLACED_IN_TEST_; c = _ })
                                             -> __pattern_syntax__ID_REPLACED_IN_TEST_))
                                 [@merlin.hide ]))
                              ~f:(fun b ->
    -|                              ((Let_syntax.sub
    -|                                  ~here:{
    -|                                          Ppx_here_lib.pos_fname = "_none_";
    -|                                          pos_lnum = 0;
    -|                                          pos_cnum = (-1);
    -|                                          pos_bol = 0
    -|                                        }
    +|                              ((Let_syntax.sub ~here
    -|                                  (Let_syntax.return
    -|                                     ((Let_syntax.map
    -|                                         ~here:{
    -|                                                 Ppx_here_lib.pos_fname =
    -|                                                   "_none_";
    -|                                                 pos_lnum = 0;
    -|                                                 pos_cnum = (-1);
    -|                                                 pos_bol = 0
    -|                                               } __pattern_syntax__ID_REPLACED_IN_TEST_
    +|                                  (Let_syntax.return
    +|                                     ((Let_syntax.map ~here
    +|                                         __pattern_syntax__ID_REPLACED_IN_TEST_
                                               ~f:(function
                                                   | (__pattern_syntax__ID_REPLACED_IN_TEST_,
                                                      { b = _; c = _ }) ->
                                                       __pattern_syntax__ID_REPLACED_IN_TEST_))
                                           [@merlin.hide ]))
                                        ~f:(fun a -> ((MY_BODY)[@nontail ])))
                                    [@nontail ])))
                          [@nontail ])))
                [@nontail ])))
      [@nontail ])
    |}]
;;

let%expect_test "destructuring match%sub" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match MY_EXPR with
      | Choice_1 (a, b) -> CHOICE_1_BODY
      | Choice_2 _ -> CHOICE_2_BODY
      | Choice_3 -> CHOICE_3_BODY]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__023_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__023_
                              ~f:(function
                                  | Choice_1 (a, b) -> 0
                                  | Choice_2 ((_)[@merlin.focus ]) -> 1
                                  | Choice_3 -> 2))[@ocaml.warning "-26-27"])
                  ~branches:3
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
                                               } __pattern_syntax__023_
                                         ~f:((function
                                              | Choice_1
                                                  (_, __pattern_syntax__025_) ->
                                                  __pattern_syntax__025_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun b ->
                                        ((Let_syntax.sub
                                            ~here:{
                                                    Ppx_here_lib.pos_fname =
                                                      "_none_";
                                                    pos_lnum = 0;
                                                    pos_cnum = (-1);
                                                    pos_bol = 0
                                                  }
                                            (Let_syntax.return
                                               ((Let_syntax.map
                                                   ~here:{
                                                           Ppx_here_lib.pos_fname
                                                             = "_none_";
                                                           pos_lnum = 0;
                                                           pos_cnum = (-1);
                                                           pos_bol = 0
                                                         } __pattern_syntax__023_
                                                   ~f:((function
                                                        | Choice_1
                                                            (__pattern_syntax__024_,
                                                             _)
                                                            ->
                                                            __pattern_syntax__024_
                                                        | _ -> assert false)
                                                   [@ocaml.warning "-11"]))
                                               [@merlin.hide ]))
                                            ~f:(fun a -> ((CHOICE_1_BODY)
                                                  [@nontail ])))
                                        [@nontail ])))
                              [@nontail ])
                          | ((1)[@merlin.hide ]) -> CHOICE_2_BODY
                          | ((2)[@merlin.hide ]) -> CHOICE_3_BODY
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "destructuring match%sub (location in scope)" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_in_scope)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match MY_EXPR with
      | Choice_1 (a, b) -> CHOICE_1_BODY
      | Choice_2 _ -> CHOICE_2_BODY
      | Choice_3 -> CHOICE_3_BODY]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub ~here (Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__026_ ->
              ((Let_syntax.switch ~here
                  ~match_:((Let_syntax.map __pattern_syntax__026_
                              ~f:(function
                                  | Choice_1 (a, b) -> 0
                                  | Choice_2 ((_)[@merlin.focus ]) -> 1
                                  | Choice_3 -> 2))[@ocaml.warning "-26-27"])
                  ~branches:3
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              ((Let_syntax.sub ~here
                                  (Let_syntax.return
                                     ((Let_syntax.map ~here
                                         __pattern_syntax__026_
                                         ~f:((function
                                              | Choice_1
                                                  (_, __pattern_syntax__028_) ->
                                                  __pattern_syntax__028_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun b ->
                                        ((Let_syntax.sub ~here
                                            (Let_syntax.return
                                               ((Let_syntax.map ~here
                                                   __pattern_syntax__026_
                                                   ~f:((function
                                                        | Choice_1
                                                            (__pattern_syntax__027_,
                                                             _)
                                                            ->
                                                            __pattern_syntax__027_
                                                        | _ -> assert false)
                                                   [@ocaml.warning "-11"]))
                                               [@merlin.hide ]))
                                            ~f:(fun a -> ((CHOICE_1_BODY)
                                                  [@nontail ])))
                                        [@nontail ])))
                              [@nontail ])
                          | ((1)[@merlin.hide ]) -> CHOICE_2_BODY
                          | ((2)[@merlin.hide ]) -> CHOICE_3_BODY
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "single-case match%sub doesn't call switch" =
  let modul = Some { txt = lident "Module"; loc } in
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul
    ~locality
    [%expr
      match MY_EXPR with
      | Choice_1 x -> CHOICE_1_BODY]
  |> print_expr;
  [%expect
    {|
    ((Module.Let_syntax.Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Module.Let_syntax.Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__029_ ->
              ((Module.Let_syntax.Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Module.Let_syntax.Let_syntax.return
                     ((Module.Let_syntax.Let_syntax.map
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__029_
                         ~f:(function
                             | Choice_1 __pattern_syntax__030_ ->
                                 __pattern_syntax__030_))[@merlin.hide ]))
                  ~f:(fun x -> ((CHOICE_1_BODY)[@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "module-qualified match%sub" =
  let modul = Some { txt = lident "Module"; loc } in
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul
    ~locality
    [%expr
      match MY_EXPR with
      | Choice_1 x -> CHOICE_1_BODY
      | Choice_2 x -> CHOICE_2_BODY]
  |> print_expr;
  [%expect
    {|
    ((Module.Let_syntax.Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Module.Let_syntax.Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__031_ ->
              ((Module.Let_syntax.Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Module.Let_syntax.Let_syntax.map
                              __pattern_syntax__031_
                              ~f:(function | Choice_1 x -> 0 | Choice_2 x -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              ((Module.Let_syntax.Let_syntax.sub
                                  ~here:{
                                          Ppx_here_lib.pos_fname = "_none_";
                                          pos_lnum = 0;
                                          pos_cnum = (-1);
                                          pos_bol = 0
                                        }
                                  (Module.Let_syntax.Let_syntax.return
                                     ((Module.Let_syntax.Let_syntax.map
                                         ~here:{
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 0;
                                                 pos_cnum = (-1);
                                                 pos_bol = 0
                                               } __pattern_syntax__031_
                                         ~f:((function
                                              | Choice_1 __pattern_syntax__032_
                                                  -> __pattern_syntax__032_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun x -> ((CHOICE_1_BODY)[@nontail ])))
                              [@nontail ])
                          | ((1)[@merlin.hide ]) ->
                              ((Module.Let_syntax.Let_syntax.sub
                                  ~here:{
                                          Ppx_here_lib.pos_fname = "_none_";
                                          pos_lnum = 0;
                                          pos_cnum = (-1);
                                          pos_bol = 0
                                        }
                                  (Module.Let_syntax.Let_syntax.return
                                     ((Module.Let_syntax.Let_syntax.map
                                         ~here:{
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 0;
                                                 pos_cnum = (-1);
                                                 pos_bol = 0
                                               } __pattern_syntax__031_
                                         ~f:((function
                                              | Choice_2 __pattern_syntax__033_
                                                  -> __pattern_syntax__033_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun x -> ((CHOICE_2_BODY)[@nontail ])))
                              [@nontail ])
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "type annotations are preserved" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let (_ : int) = EXPR in
      BODY]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } EXPR
        ~f:(fun __pattern_syntax__034_ ->
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
                               } __pattern_syntax__034_
                         ~f:(function | (_ : int) -> ()))[@nontail ]))
                  ~f:(fun _ -> ((BODY)[@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "function%sub" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      function
      | Some a -> EXPR_SOME
      | None -> EXPR_NONE]
  |> print_expr;
  [%expect
    {|
    fun __let_syntax__035_ ->
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return __let_syntax__035_)
          ~f:(fun __pattern_syntax__036_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((Let_syntax.map __pattern_syntax__036_
                                ~f:(function | Some a -> 0 | None -> 1))
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
                                                 } __pattern_syntax__036_
                                           ~f:((function
                                                | Some __pattern_syntax__037_ ->
                                                    __pattern_syntax__037_
                                                | _ -> assert false)
                                           [@ocaml.warning "-11"]))
                                       [@merlin.hide ]))
                                    ~f:(fun a -> ((EXPR_SOME)[@nontail ])))
                                [@nontail ])
                            | ((1)[@merlin.hide ]) -> EXPR_NONE
                            | _ -> assert false))
                [@nontail ])))
      [@nontail ])
    |}]
;;

let%expect_test "function%arr" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      function
      | Some a -> EXPR_SOME
      | None -> EXPR_NONE]
  |> print_expr;
  [%expect
    {|
    fun __let_syntax__038_ ->
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__038_
        ~f:(function | Some a -> EXPR_SOME | None -> EXPR_NONE)
    |}]
;;

let%expect_test "destructuring let%arr uses cutoff" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c; _ } = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff MY_EXPR
         ~equal:(fun
                   (__old_for_cutoff__040_,
                    { b = __old_for_cutoff__042_; c = __old_for_cutoff__044_;_})
                   (__new_for_cutoff__039_,
                    { b = __new_for_cutoff__041_; c = __new_for_cutoff__043_;_})
                   ->
                   (phys_equal __old_for_cutoff__044_ __new_for_cutoff__043_) &&
                     ((phys_equal __old_for_cutoff__042_ __new_for_cutoff__041_)
                        &&
                        (phys_equal __old_for_cutoff__040_ __new_for_cutoff__039_))))
      ~f:(fun (a, { b; c;_}) -> MY_BODY)
    |}]
;;

let%expect_test "let%arr different values, but same names" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let (`Foo (a, _) | `Bar (_, a)) = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff ((MY_EXPR)[@ppxlib.enter_value a])
         ~equal:(fun
                   (`Foo (__old_for_cutoff__049_, _)
                    | `Bar (_, __old_for_cutoff__049_))
                   (`Foo (__new_for_cutoff__048_, _)
                    | `Bar (_, __new_for_cutoff__048_))
                   -> phys_equal __old_for_cutoff__049_ __new_for_cutoff__048_))
      ~f:(fun (`Foo (a, _) | `Bar (_, a)) -> MY_BODY)
    |}]
;;

let%expect_test "destructuring let%arr uses cutoff, if specific fields have ignored \
                 values"
  =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c; d = _; e = _; f = _; g = { h = _; i = { k = _ } } } = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff MY_EXPR
         ~equal:(fun
                   (__old_for_cutoff__052_,
                    { b = __old_for_cutoff__054_; c = __old_for_cutoff__056_;
                      d = _; e = _; f = _; g = { h = _; i = { k = _ } } })
                   (__new_for_cutoff__051_,
                    { b = __new_for_cutoff__053_; c = __new_for_cutoff__055_;
                      d = _; e = _; f = _; g = { h = _; i = { k = _ } } })
                   ->
                   (phys_equal __old_for_cutoff__056_ __new_for_cutoff__055_) &&
                     ((phys_equal __old_for_cutoff__054_ __new_for_cutoff__053_)
                        &&
                        (phys_equal __old_for_cutoff__052_ __new_for_cutoff__051_))))
      ~f:(fun (a, { b; c; d = _; e = _; f = _; g = { h = _; i = { k = _ } } }) ->
            MY_BODY)
    |}]
;;

let%expect_test "let%arr - location-in-scope" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_in_scope)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c; _ } = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr ~here
      (Let_syntax.cutoff ~here MY_EXPR
         ~equal:(fun
                   (__old_for_cutoff__059_,
                    { b = __old_for_cutoff__061_; c = __old_for_cutoff__063_;_})
                   (__new_for_cutoff__058_,
                    { b = __new_for_cutoff__060_; c = __new_for_cutoff__062_;_})
                   ->
                   (phys_equal __old_for_cutoff__063_ __new_for_cutoff__062_) &&
                     ((phys_equal __old_for_cutoff__061_ __new_for_cutoff__060_)
                        &&
                        (phys_equal __old_for_cutoff__059_ __new_for_cutoff__058_))))
      ~f:(fun (a, { b; c;_}) -> MY_BODY)
    |}]
;;

let%expect_test "destructuring let%arr uses cutoff (multiple arms)" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c; _ } = MY_EXPR
      and x, { y; z; _ } = OTHER_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__065_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__066_ = OTHER_EXPR[@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff __let_syntax__065_
         ~equal:(fun
                   (__old_for_cutoff__068_,
                    { b = __old_for_cutoff__070_; c = __old_for_cutoff__072_;_})
                   (__new_for_cutoff__067_,
                    { b = __new_for_cutoff__069_; c = __new_for_cutoff__071_;_})
                   ->
                   (phys_equal __old_for_cutoff__072_ __new_for_cutoff__071_) &&
                     ((phys_equal __old_for_cutoff__070_ __new_for_cutoff__069_)
                        &&
                        (phys_equal __old_for_cutoff__068_ __new_for_cutoff__067_))))
      (Let_syntax.cutoff __let_syntax__066_
         ~equal:(fun
                   (__old_for_cutoff__074_,
                    { y = __old_for_cutoff__076_; z = __old_for_cutoff__078_;_})
                   (__new_for_cutoff__073_,
                    { y = __new_for_cutoff__075_; z = __new_for_cutoff__077_;_})
                   ->
                   (phys_equal __old_for_cutoff__078_ __new_for_cutoff__077_) &&
                     ((phys_equal __old_for_cutoff__076_ __new_for_cutoff__075_)
                        &&
                        (phys_equal __old_for_cutoff__074_ __new_for_cutoff__073_))))
      ~f:(fun (a, { b; c;_}) (x, { y; z;_}) -> MY_BODY)
    |}]
;;

let%expect_test "one arm of destructuring let%arr uses cutoff" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c; _ } = MY_EXPR
      and y = OTHER_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__081_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__082_ = ((OTHER_EXPR)[@ppxlib.enter_value y])[@@ppxlib.do_not_enter_value
                                                                    ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff __let_syntax__081_
         ~equal:(fun
                   (__old_for_cutoff__084_,
                    { b = __old_for_cutoff__086_; c = __old_for_cutoff__088_;_})
                   (__new_for_cutoff__083_,
                    { b = __new_for_cutoff__085_; c = __new_for_cutoff__087_;_})
                   ->
                   (phys_equal __old_for_cutoff__088_ __new_for_cutoff__087_) &&
                     ((phys_equal __old_for_cutoff__086_ __new_for_cutoff__085_)
                        &&
                        (phys_equal __old_for_cutoff__084_ __new_for_cutoff__083_))))
      __let_syntax__082_ ~f:(fun (a, { b; c;_}) y -> MY_BODY)
    |}]
;;

let%expect_test "Destructuring does not happen when there is no ignoring" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c; d } = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            } MY_EXPR ~f:(fun (a, { b; c; d }) -> MY_BODY)
    |}]
;;

let%expect_test "Destructuring does not happen when there is no ignoring (multiple arms)" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a, { b; c; d } = MY_EXPR
      and x, { y; z; w } = OTHER_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__092_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__093_ = OTHER_EXPR[@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            } __let_syntax__092_ __let_syntax__093_
      ~f:(fun (a, { b; c; d }) (x, { y; z; w }) -> MY_BODY)
    |}]
;;

module%test [@name "Destructuring vs. no destructuring criteria."] _ = struct
  let run_test expr =
    expr
    |> Ppx_let_expander.expand
         (Ppx_bonsai_expander.arr Location_of_callsite)
         Ppx_let_expander.Extension_kind.default
         ~modul:None
         ~locality
    |> print_expr
  ;;

  let%expect_test "Single ppat_var does not get destructed." =
    (* This is the most common case and since there is no need for destruction here; this
       one is not destructed. *)
    run_test
      [%expr
        let a = NO_DESTRUCTION in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } ((NO_DESTRUCTION)[@ppxlib.enter_value a]) ~f:(fun a -> BODY)
      |}]
  ;;

  let%expect_test "Single ppat_var does not get destructed (multiple arms)" =
    (* This is the most common case and since there is no need for destruction here; this
       one is not destructed. *)
    run_test
      [%expr
        let a = NO_DESTRUCTION
        and b = ALSO_NO_DESTRUCTION in
        BODY];
    [%expect
      {|
      let __let_syntax__097_ = ((NO_DESTRUCTION)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                          ]
      and __let_syntax__098_ = ((ALSO_NO_DESTRUCTION)[@ppxlib.enter_value b])
      [@@ppxlib.do_not_enter_value ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__097_ __let_syntax__098_ ~f:(fun a b -> BODY)
      |}]
  ;;

  let%expect_test "alias might result in no destruction (multiple arms)" =
    (* This test case shows that despite there being an underscored ignored inside of the
       left handside pattern, the destruction does not occur since there is an alias. *)
    run_test
      [%expr
        let (_ as a) = NO_DESTRUCTION
        and (_ as b) = ALSO_NO_DESTRUCTION in
        BODY];
    [%expect
      {|
      let __let_syntax__101_ = ((NO_DESTRUCTION)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                          ]
      and __let_syntax__102_ = ((ALSO_NO_DESTRUCTION)[@ppxlib.enter_value b])
      [@@ppxlib.do_not_enter_value ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__101_ __let_syntax__102_
        ~f:(fun (_ as a) (_ as b) -> BODY)
      |}]
  ;;

  let%expect_test "alias might result in destruction." =
    (* This test case showcases the true behavior of how aliases are treated. They block
       any "ignores" inside of them, but remain ineffective is something is ignored
       outside of its contents/reach. *)
    run_test
      [%expr
        let (_ as a), _ = DESTRUCT in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.cutoff ((DESTRUCT)[@ppxlib.enter_value a])
           ~equal:(fun ((_ as __old_for_cutoff__106_), _)
                     ((_ as __new_for_cutoff__105_), _) ->
                     phys_equal __old_for_cutoff__106_ __new_for_cutoff__105_))
        ~f:(fun ((_ as a), _) -> BODY)
      |}]
  ;;

  let%expect_test "nested cutoff with ignored fields" =
    run_test
      [%expr
        let { foo; bar = { baz; _ }; _ } = BASE in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.cutoff BASE
           ~equal:(fun
                     { foo = __old_for_cutoff__109_;
                       bar = { baz = __old_for_cutoff__111_;_};_}
                     { foo = __new_for_cutoff__108_;
                       bar = { baz = __new_for_cutoff__110_;_};_}
                     ->
                     (phys_equal __old_for_cutoff__109_ __new_for_cutoff__108_) &&
                       (phys_equal __old_for_cutoff__111_ __new_for_cutoff__110_)))
        ~f:(fun { foo; bar = { baz;_};_} -> BODY)
      |}]
  ;;

  let%expect_test "'arbitrary' deep ignoring of at least one pattern results in \
                   destruction."
    =
    run_test
      [%expr
        let ((b, _) : t), c = DESTRUCT in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.cutoff DESTRUCT
           ~equal:(fun (((__old_for_cutoff__114_, _) : t), __old_for_cutoff__116_)
                     (((__new_for_cutoff__113_, _) : t), __new_for_cutoff__115_) ->
                     (phys_equal __old_for_cutoff__116_ __new_for_cutoff__115_) &&
                       (phys_equal __old_for_cutoff__114_ __new_for_cutoff__113_)))
        ~f:(fun (((b, _) : t), c) -> BODY)
      |}]
  ;;

  let%expect_test "'arbitrary' deep ignoring of at least one pattern results in \
                   destruction. (multiple arms)"
    =
    run_test
      [%expr
        let ((b, _) : t), c = DESTRUCT
        and ((d, _) : t), e = ALSO_DESTRUCT in
        BODY];
    [%expect
      {|
      let __let_syntax__118_ = DESTRUCT[@@ppxlib.do_not_enter_value ]
      and __let_syntax__119_ = ALSO_DESTRUCT[@@ppxlib.do_not_enter_value ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.cutoff __let_syntax__118_
           ~equal:(fun (((__old_for_cutoff__121_, _) : t), __old_for_cutoff__123_)
                     (((__new_for_cutoff__120_, _) : t), __new_for_cutoff__122_) ->
                     (phys_equal __old_for_cutoff__123_ __new_for_cutoff__122_) &&
                       (phys_equal __old_for_cutoff__121_ __new_for_cutoff__120_)))
        (Let_syntax.cutoff __let_syntax__119_
           ~equal:(fun (((__old_for_cutoff__125_, _) : t), __old_for_cutoff__127_)
                     (((__new_for_cutoff__124_, _) : t), __new_for_cutoff__126_) ->
                     (phys_equal __old_for_cutoff__127_ __new_for_cutoff__126_) &&
                       (phys_equal __old_for_cutoff__125_ __new_for_cutoff__124_)))
        ~f:(fun (((b, _) : t), c) (((d, _) : t), e) -> BODY)
      |}]
  ;;

  let%expect_test "'closed' record results in no destruction" =
    run_test
      [%expr
        let { a; b } = NO_DESTRUCTION in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } NO_DESTRUCTION ~f:(fun { a; b } -> BODY)
      |}]
  ;;

  let%expect_test "'closed' record results in no destruction (multiple arms)" =
    run_test
      [%expr
        let { a; b } = NO_DESTRUCTION
        and { c; d } = ALSO_NO_DESTRUCTION in
        BODY];
    [%expect
      {|
      let __let_syntax__131_ = NO_DESTRUCTION[@@ppxlib.do_not_enter_value ]
      and __let_syntax__132_ = ALSO_NO_DESTRUCTION[@@ppxlib.do_not_enter_value ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__131_ __let_syntax__132_
        ~f:(fun { a; b } { c; d } -> BODY)
      |}]
  ;;

  let%expect_test "closed record, named unpacked module, results in no destruction." =
    run_test
      [%expr
        let { a; b = (module X) }, c = NO_DESTRUCTION in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } NO_DESTRUCTION ~f:(fun ({ a; b = (module X)  }, c) -> BODY)
      |}]
  ;;

  let%expect_test "Unnamed module results in destruction." =
    run_test
      [%expr
        let ({ a; b = (module _) } : t), c = DESTRUCT in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.cutoff DESTRUCT
           ~equal:(fun
                     (({ a = __old_for_cutoff__137_; b = (module _)  } : t),
                      __old_for_cutoff__139_)
                     (({ a = __new_for_cutoff__136_; b = (module _)  } : t),
                      __new_for_cutoff__138_)
                     ->
                     (phys_equal __old_for_cutoff__139_ __new_for_cutoff__138_) &&
                       (phys_equal __old_for_cutoff__137_ __new_for_cutoff__136_)))
        ~f:(fun (({ a; b = (module _)  } : t), c) -> BODY)
      |}]
  ;;

  let%expect_test "If everything is ignored, equality function is always true." =
    run_test
      [%expr
        let ({ a = _; b = (module _) } : t), _ = ALWAYS_EQUAL in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.cutoff ALWAYS_EQUAL ~equal:(fun _ _ -> true))
        ~f:(fun (({ a = _; b = (module _)  } : t), _) -> BODY)
      |}]
  ;;

  let%expect_test "Duplicate variables in pattern" =
    (* This test showcases current behavior. Since OCaml does not allow multiple
       "parallel" bindings on the same level, there is an error shown to the user by the
       OCaml compiler and not the ppx.
    *)
    run_test
      [%expr
        let { a; b = a; _ } = EXPR in
        BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.cutoff ((EXPR)[@ppxlib.enter_value a])
           ~equal:(fun { a = __old_for_cutoff__145_; b = __old_for_cutoff__145_;_}
                     { a = __new_for_cutoff__144_; b = __new_for_cutoff__144_;_} ->
                     phys_equal __old_for_cutoff__145_ __new_for_cutoff__144_))
        ~f:(fun { a; b = a;_} -> BODY)
      |}]
  ;;
end

let%expect_test "match%arr inserts cutoff when there are ignored variables" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match EXPR with
      | A (a, _) -> MY_BODY
      | B -> MY_BODY
      | _ -> MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff EXPR
         ~equal:(fun __old_for_cutoff_ppx_bonsai__149_
                   __new_for_cutoff_ppx_bonsai__150_ ->
                   ((match (__old_for_cutoff_ppx_bonsai__149_,
                             __new_for_cutoff_ppx_bonsai__150_)
                     with
                     | (A (__old_for_cutoff__148_, _), A
                        (__new_for_cutoff__147_, _)) ->
                         phys_equal __old_for_cutoff__148_ __new_for_cutoff__147_
                     | (A (__new_for_cutoff__147_, _), _)
                       | (_, A (__new_for_cutoff__147_, _)) -> false
                     | (B, B) -> true
                     | (B, _) | (_, B) -> false
                     | (_, _) -> true)
                   [@ocaml.warning "-8-11"])))
      ~f:(function | A (a, _) -> MY_BODY | B -> MY_BODY | _ -> MY_BODY)
    |}]
;;

let%expect_test "match%arr with different patterns inside same constructor" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match EXPR with
      | A (1, _) -> MY_BODY
      | A (_, 2) -> MY_BODY
      | A (a, _) -> MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff EXPR
         ~equal:(fun __old_for_cutoff_ppx_bonsai__153_
                   __new_for_cutoff_ppx_bonsai__154_ ->
                   ((match (__old_for_cutoff_ppx_bonsai__153_,
                             __new_for_cutoff_ppx_bonsai__154_)
                     with
                     | (A (1, _), A (1, _)) -> true
                     | (A (1, _), _) | (_, A (1, _)) -> false
                     | (A (_, 2), A (_, 2)) -> true
                     | (A (_, 2), _) | (_, A (_, 2)) -> false
                     | (A (__old_for_cutoff__152_, _), A
                        (__new_for_cutoff__151_, _)) ->
                         phys_equal __old_for_cutoff__152_ __new_for_cutoff__151_
                     | (A (__new_for_cutoff__151_, _), _)
                       | (_, A (__new_for_cutoff__151_, _)) -> false)
                   [@ocaml.warning "-8-11"])))
      ~f:(function
          | A (1, _) -> MY_BODY
          | A (_, 2) -> MY_BODY
          | A (a, _) -> MY_BODY)
    |}]
;;

let%expect_test "match%arr doesn't add cutoff when there are no ignored variables" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match EXPR with
      | A (a, b) -> MY_BODY
      | B -> MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            } EXPR ~f:(function | A (a, b) -> MY_BODY | B -> MY_BODY)
    |}]
;;

let%expect_test "match%arr with [or] pattern" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match EXPR with
      | A (t, _) | B (_, t) -> MY_BODY
      | C -> MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff EXPR
         ~equal:(fun __old_for_cutoff_ppx_bonsai__159_
                   __new_for_cutoff_ppx_bonsai__160_ ->
                   ((match (__old_for_cutoff_ppx_bonsai__159_,
                             __new_for_cutoff_ppx_bonsai__160_)
                     with
                     | ((A (__old_for_cutoff__158_, _) | B
                         (_, __old_for_cutoff__158_)),
                        (A (__new_for_cutoff__157_, _) | B
                         (_, __new_for_cutoff__157_))) ->
                         phys_equal __old_for_cutoff__158_ __new_for_cutoff__157_
                     | ((A (__new_for_cutoff__157_, _) | B
                         (_, __new_for_cutoff__157_)),
                        _)
                       | (_,
                          (A (__new_for_cutoff__157_, _) | B
                           (_, __new_for_cutoff__157_)))
                         -> false
                     | (C, C) -> true
                     | (C, _) | (_, C) -> false)
                   [@ocaml.warning "-8-11"])))
      ~f:(function | A (t, _) | B (_, t) -> MY_BODY | C -> MY_BODY)
    |}]
;;

let%expect_test "match%arr with [as] pattern (with no _s in other branches)" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match EXPR with
      | A (t1, _) as t2 -> MY_BODY
      | B -> MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            } EXPR ~f:(function | A (t1, _) as t2 -> MY_BODY | B -> MY_BODY)
    |}]
;;

let%expect_test "match%arr with [as] pattern (with _s in other branches)" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      match EXPR with
      | A (t1, _) as t2 -> MY_BODY
      | B _ -> MY_BODY]
  |> print_expr;
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff EXPR
         ~equal:(fun __old_for_cutoff_ppx_bonsai__165_
                   __new_for_cutoff_ppx_bonsai__166_ ->
                   ((match (__old_for_cutoff_ppx_bonsai__165_,
                             __new_for_cutoff_ppx_bonsai__166_)
                     with
                     | ((A (__old_for_cutoff__162_, _) as __old_for_cutoff__164_),
                        (A (__new_for_cutoff__161_, _) as __new_for_cutoff__163_))
                         ->
                         (phys_equal __old_for_cutoff__164_
                            __new_for_cutoff__163_)
                           &&
                           (phys_equal __old_for_cutoff__162_
                              __new_for_cutoff__161_)
                     | ((A (__new_for_cutoff__161_, _) as __new_for_cutoff__163_),
                        _)
                       | (_,
                          (A (__new_for_cutoff__161_, _) as
                             __new_for_cutoff__163_))
                         -> false
                     | (B _, B _) -> true
                     | (B _, _) | (_, B _) -> false)
                   [@ocaml.warning "-8-11"])))
      ~f:(function | A (t1, _) as t2 -> MY_BODY | B _ -> MY_BODY)
    |}]
;;

module%test [@name "match%arr with tuple payload"] _ = struct
  let test expr =
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.arr Location_of_callsite)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      expr
    |> print_expr
  ;;

  let%expect_test "basic/normal case" =
    test
      [%expr
        match EXPR1, EXPR2 with
        | A, B -> MY_BODY
        | B -> MY_BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (let __let_syntax__169_ = ((EXPR1)
           [@ppxlib.enter_value __ppx_bonsai_tuple__167_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__170_ = ((EXPR2)
           [@ppxlib.enter_value __ppx_bonsai_tuple__168_])[@@ppxlib.do_not_enter_value
                                                            ] in
         Let_syntax.map2
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__169_ __let_syntax__170_
           ~f:(fun __ppx_bonsai_tuple__167_ __ppx_bonsai_tuple__168_ ->
                 (__ppx_bonsai_tuple__167_, __ppx_bonsai_tuple__168_)))
        ~f:(function | (A, B) -> MY_BODY | B -> MY_BODY)
      |}]
  ;;

  let%expect_test "many tuples" =
    test
      [%expr
        match EXPR1, EXPR2, EXPR3, EXPR4, EXPR5 with
        | A, B, C, D, E -> BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (let __let_syntax__178_ = ((EXPR1)
           [@ppxlib.enter_value __ppx_bonsai_tuple__173_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__179_ = ((EXPR2)
           [@ppxlib.enter_value __ppx_bonsai_tuple__174_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__180_ = ((EXPR3)
           [@ppxlib.enter_value __ppx_bonsai_tuple__175_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__181_ = ((EXPR4)
           [@ppxlib.enter_value __ppx_bonsai_tuple__176_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__182_ = ((EXPR5)
           [@ppxlib.enter_value __ppx_bonsai_tuple__177_])[@@ppxlib.do_not_enter_value
                                                            ] in
         Let_syntax.map5
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__178_ __let_syntax__179_ __let_syntax__180_
           __let_syntax__181_ __let_syntax__182_
           ~f:(fun __ppx_bonsai_tuple__173_ __ppx_bonsai_tuple__174_
                 __ppx_bonsai_tuple__175_ __ppx_bonsai_tuple__176_
                 __ppx_bonsai_tuple__177_ ->
                 (__ppx_bonsai_tuple__173_, __ppx_bonsai_tuple__174_,
                   __ppx_bonsai_tuple__175_, __ppx_bonsai_tuple__176_,
                   __ppx_bonsai_tuple__177_)))
        ~f:(function | (A, B, C, D, E) -> BODY)
      |}]
  ;;

  let%expect_test "unreasonable amount of tuples" =
    test
      [%expr
        match
          ( EXPR1
          , EXPR2
          , EXPR3
          , EXPR4
          , EXPR5
          , EXPR6
          , EXPR7
          , EXPR8
          , EXPR9
          , EXPR10
          , EXPR11
          , EXPR12
          , EXPR13
          , EXPR14
          , EXPR15
          , EXPR16
          , EXPR17
          , EXPR18
          , EXPR19
          , EXPR20 )
        with
        | A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T -> BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (let __let_syntax__208_ = ((EXPR1)
           [@ppxlib.enter_value __ppx_bonsai_tuple__188_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__209_ = ((EXPR2)
           [@ppxlib.enter_value __ppx_bonsai_tuple__189_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__210_ = ((EXPR3)
           [@ppxlib.enter_value __ppx_bonsai_tuple__190_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__211_ = ((EXPR4)
           [@ppxlib.enter_value __ppx_bonsai_tuple__191_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__212_ = ((EXPR5)
           [@ppxlib.enter_value __ppx_bonsai_tuple__192_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__213_ = ((EXPR6)
           [@ppxlib.enter_value __ppx_bonsai_tuple__193_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__214_ = ((EXPR7)
           [@ppxlib.enter_value __ppx_bonsai_tuple__194_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__215_ = ((EXPR8)
           [@ppxlib.enter_value __ppx_bonsai_tuple__195_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__216_ = ((EXPR9)
           [@ppxlib.enter_value __ppx_bonsai_tuple__196_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__217_ = ((EXPR10)
           [@ppxlib.enter_value __ppx_bonsai_tuple__197_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__218_ = ((EXPR11)
           [@ppxlib.enter_value __ppx_bonsai_tuple__198_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__219_ = ((EXPR12)
           [@ppxlib.enter_value __ppx_bonsai_tuple__199_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__220_ = ((EXPR13)
           [@ppxlib.enter_value __ppx_bonsai_tuple__200_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__221_ = ((EXPR14)
           [@ppxlib.enter_value __ppx_bonsai_tuple__201_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__222_ = ((EXPR15)
           [@ppxlib.enter_value __ppx_bonsai_tuple__202_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__223_ = ((EXPR16)
           [@ppxlib.enter_value __ppx_bonsai_tuple__203_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__224_ = ((EXPR17)
           [@ppxlib.enter_value __ppx_bonsai_tuple__204_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__225_ = ((EXPR18)
           [@ppxlib.enter_value __ppx_bonsai_tuple__205_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__226_ = ((EXPR19)
           [@ppxlib.enter_value __ppx_bonsai_tuple__206_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__227_ = ((EXPR20)
           [@ppxlib.enter_value __ppx_bonsai_tuple__207_])[@@ppxlib.do_not_enter_value
                                                            ] in
         Let_syntax.map3
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 }
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__208_ __let_syntax__209_ __let_syntax__210_
              __let_syntax__211_ __let_syntax__212_ __let_syntax__213_
              __let_syntax__214_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__215_ __let_syntax__216_ __let_syntax__217_
              __let_syntax__218_ __let_syntax__219_ __let_syntax__220_
              __let_syntax__221_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (Let_syntax.map6
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__222_ __let_syntax__223_ __let_syntax__224_
              __let_syntax__225_ __let_syntax__226_ __let_syntax__227_
              ~f:(fun t0 t1 t2 t3 t4 t5 -> (t0, t1, t2, t3, t4, t5)))
           ~f:(fun
                 (__ppx_bonsai_tuple__188_, __ppx_bonsai_tuple__189_,
                  __ppx_bonsai_tuple__190_, __ppx_bonsai_tuple__191_,
                  __ppx_bonsai_tuple__192_, __ppx_bonsai_tuple__193_,
                  __ppx_bonsai_tuple__194_)
                 (__ppx_bonsai_tuple__195_, __ppx_bonsai_tuple__196_,
                  __ppx_bonsai_tuple__197_, __ppx_bonsai_tuple__198_,
                  __ppx_bonsai_tuple__199_, __ppx_bonsai_tuple__200_,
                  __ppx_bonsai_tuple__201_)
                 (__ppx_bonsai_tuple__202_, __ppx_bonsai_tuple__203_,
                  __ppx_bonsai_tuple__204_, __ppx_bonsai_tuple__205_,
                  __ppx_bonsai_tuple__206_, __ppx_bonsai_tuple__207_)
                 ->
                 (__ppx_bonsai_tuple__188_, __ppx_bonsai_tuple__189_,
                   __ppx_bonsai_tuple__190_, __ppx_bonsai_tuple__191_,
                   __ppx_bonsai_tuple__192_, __ppx_bonsai_tuple__193_,
                   __ppx_bonsai_tuple__194_, __ppx_bonsai_tuple__195_,
                   __ppx_bonsai_tuple__196_, __ppx_bonsai_tuple__197_,
                   __ppx_bonsai_tuple__198_, __ppx_bonsai_tuple__199_,
                   __ppx_bonsai_tuple__200_, __ppx_bonsai_tuple__201_,
                   __ppx_bonsai_tuple__202_, __ppx_bonsai_tuple__203_,
                   __ppx_bonsai_tuple__204_, __ppx_bonsai_tuple__205_,
                   __ppx_bonsai_tuple__206_, __ppx_bonsai_tuple__207_)))
        ~f:(function
            | (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) -> BODY)
      |}]
  ;;

  let%expect_test "tuple & cutoff mix" =
    test
      [%expr
        match EXPR1, EXPR2 with
        | Some _, _ -> BODY
        | None, Some (_, f) -> BODY
        | _ -> BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.cutoff
           (let __let_syntax__253_ = ((EXPR1)
              [@ppxlib.enter_value __ppx_bonsai_tuple__251_])[@@ppxlib.do_not_enter_value
                                                               ]
            and __let_syntax__254_ = ((EXPR2)
              [@ppxlib.enter_value __ppx_bonsai_tuple__252_])[@@ppxlib.do_not_enter_value
                                                               ] in
            Let_syntax.map2
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__253_ __let_syntax__254_
              ~f:(fun __ppx_bonsai_tuple__251_ __ppx_bonsai_tuple__252_ ->
                    (__ppx_bonsai_tuple__251_, __ppx_bonsai_tuple__252_)))
           ~equal:(fun __old_for_cutoff_ppx_bonsai__259_
                     __new_for_cutoff_ppx_bonsai__260_ ->
                     ((match (__old_for_cutoff_ppx_bonsai__259_,
                               __new_for_cutoff_ppx_bonsai__260_)
                       with
                       | ((Some _, _), (Some _, _)) -> true
                       | ((Some _, _), _) | (_, (Some _, _)) -> false
                       | ((None, Some (_, __old_for_cutoff__258_)),
                          (None, Some (_, __new_for_cutoff__257_))) ->
                           phys_equal __old_for_cutoff__258_ __new_for_cutoff__257_
                       | ((None, Some (_, __new_for_cutoff__257_)), _)
                         | (_, (None, Some (_, __new_for_cutoff__257_))) -> false
                       | (_, _) -> true)
                     [@ocaml.warning "-8-11"])))
        ~f:(function
            | (Some _, _) -> BODY
            | (None, Some (_, f)) -> BODY
            | _ -> BODY)
      |}]
  ;;
end

module%test [@name "match%sub with tuple payload"] _ = struct
  let test expr =
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.sub Location_of_callsite)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      expr
    |> print_expr
  ;;

  let%expect_test "basic/normal case" =
    test
      [%expr
        match EXPR1, EXPR2 with
        | A, B -> BODY
        | B -> BODY];
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
             (let __let_syntax__263_ = ((EXPR1)
                [@ppxlib.enter_value __ppx_bonsai_tuple__261_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__264_ = ((EXPR2)
                [@ppxlib.enter_value __ppx_bonsai_tuple__262_])[@@ppxlib.do_not_enter_value
                                                                 ] in
              Let_syntax.map2
                ~here:{
                        Ppx_here_lib.pos_fname = "_none_";
                        pos_lnum = 0;
                        pos_cnum = (-1);
                        pos_bol = 0
                      } __let_syntax__263_ __let_syntax__264_
                ~f:(fun __ppx_bonsai_tuple__261_ __ppx_bonsai_tuple__262_ ->
                      (__ppx_bonsai_tuple__261_, __ppx_bonsai_tuple__262_))))
          ~f:(fun __pattern_syntax__267_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((Let_syntax.map __pattern_syntax__267_
                                ~f:(function | (A, B) -> 0 | B -> 1))
                    [@ocaml.warning "-26-27"]) ~branches:2
                    ~with_:(function
                            | ((0)[@merlin.hide ]) -> BODY
                            | ((1)[@merlin.hide ]) -> BODY
                            | _ -> assert false))
                [@nontail ])))
      [@nontail ])
      |}]
  ;;

  let%expect_test "many tuples" =
    test
      [%expr
        match EXPR1, EXPR2, EXPR3, EXPR4, EXPR5 with
        | A, B, C, D, E -> BODY];
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
             (let __let_syntax__273_ = ((EXPR1)
                [@ppxlib.enter_value __ppx_bonsai_tuple__268_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__274_ = ((EXPR2)
                [@ppxlib.enter_value __ppx_bonsai_tuple__269_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__275_ = ((EXPR3)
                [@ppxlib.enter_value __ppx_bonsai_tuple__270_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__276_ = ((EXPR4)
                [@ppxlib.enter_value __ppx_bonsai_tuple__271_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__277_ = ((EXPR5)
                [@ppxlib.enter_value __ppx_bonsai_tuple__272_])[@@ppxlib.do_not_enter_value
                                                                 ] in
              Let_syntax.map5
                ~here:{
                        Ppx_here_lib.pos_fname = "_none_";
                        pos_lnum = 0;
                        pos_cnum = (-1);
                        pos_bol = 0
                      } __let_syntax__273_ __let_syntax__274_ __let_syntax__275_
                __let_syntax__276_ __let_syntax__277_
                ~f:(fun __ppx_bonsai_tuple__268_ __ppx_bonsai_tuple__269_
                      __ppx_bonsai_tuple__270_ __ppx_bonsai_tuple__271_
                      __ppx_bonsai_tuple__272_ ->
                      (__ppx_bonsai_tuple__268_, __ppx_bonsai_tuple__269_,
                        __ppx_bonsai_tuple__270_, __ppx_bonsai_tuple__271_,
                        __ppx_bonsai_tuple__272_))))
          ~f:(fun __pattern_syntax__283_ ->
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
                                 } __pattern_syntax__283_
                           ~f:(function | (A, B, C, D, E) -> ()))[@nontail ]))
                    ~f:(fun _ -> ((BODY)[@nontail ])))
                [@nontail ])))
      [@nontail ])
      |}]
  ;;

  let%expect_test "Case with ignored patterns at different levels" =
    (* This test shows that [_]'s are focused. *)
    test
      [%expr
        match EXPR with
        | Some _ -> BODY
        | Some (_, f) -> BODY
        | _ -> BODY];
    [%expect
      {|
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return EXPR)
          ~f:(fun __pattern_syntax__284_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((Let_syntax.map __pattern_syntax__284_
                                ~f:(function
                                    | Some ((_)[@merlin.focus ]) -> 0
                                    | Some (((_)[@merlin.focus ]), f) -> 1
                                    | ((_)[@merlin.focus ]) -> 2))
                    [@ocaml.warning "-26-27"]) ~branches:3
                    ~with_:(function
                            | ((0)[@merlin.hide ]) -> BODY
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
                                                 } __pattern_syntax__284_
                                           ~f:((function
                                                | Some (_, __pattern_syntax__285_)
                                                    -> __pattern_syntax__285_
                                                | _ -> assert false)
                                           [@ocaml.warning "-11"]))[@merlin.hide ]))
                                    ~f:(fun f -> ((BODY)[@nontail ])))
                                [@nontail ])
                            | ((2)[@merlin.hide ]) -> BODY
                            | _ -> assert false))
                [@nontail ])))
      [@nontail ])
      |}]
  ;;
end

module%test [@name "match%sub with . case"] _ = struct
  let test expr =
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.sub Location_of_callsite)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      expr
    |> print_expr
  ;;

  let%expect_test "single-case match%sub" =
    test
      [%expr
        match EXPR with
        | _ -> .];
    [%expect
      {|
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return EXPR)
          ~f:(fun __pattern_syntax__286_ ->
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
                                 } __pattern_syntax__286_ ~f:(function | _ -> ()))
                       [@nontail ])) ~f:(fun _ -> ((.)[@nontail ])))
                [@nontail ])))
      [@nontail ])
      |}]
  ;;

  let%expect_test "multi-case match%sub" =
    test
      [%expr
        match EXPR with
        | A _ -> MY_BODY
        | B _ -> .
        | C -> MY_BODY];
    [%expect
      {|
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return EXPR)
          ~f:(fun __pattern_syntax__287_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((Let_syntax.map __pattern_syntax__287_
                                ~f:(function
                                    | A ((_)[@merlin.focus ]) -> 0
                                    | C -> 1
                                    | B ((_)[@merlin.focus ]) -> .))
                    [@ocaml.warning "-26-27"]) ~branches:2
                    ~with_:(function
                            | ((0)[@merlin.hide ]) -> MY_BODY
                            | ((1)[@merlin.hide ]) -> MY_BODY
                            | _ -> assert false))
                [@nontail ])))
      [@nontail ])
      |}]
  ;;
end

module%test [@name "match%sub with when clause"] _ = struct
  let test expr =
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.sub Location_of_callsite)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      expr
    |> print_expr
  ;;

  let%expect_test "single-case match%sub" =
    test
      [%expr
        match EXPR with
        | x when x > 1 -> BODY];
    [%expect
      {|
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return EXPR) ~f:(fun x -> let _ : _ = x in BODY))
      [@nontail ])
      |}]
  ;;

  let%expect_test "pattern without any variables" =
    test
      [%expr
        match EXPR with
        | _ when condition -> BODY];
    [%expect
      {|
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return EXPR)
          ~f:(fun __pattern_syntax__289_ ->
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
                                 } __pattern_syntax__289_ ~f:(function | _ -> ()))
                       [@nontail ])) ~f:(fun _ -> ((BODY)[@nontail ])))
                [@nontail ])))
      [@nontail ])
      |}]
  ;;

  let%expect_test "multi-case match%sub" =
    test
      [%expr
        match EXPR with
        | x when condition1 && x > 1 -> BODY
        | y when condition2 -> BODY
        | z -> BODY];
    [%expect
      {|
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return EXPR)
          ~f:(fun __pattern_syntax__290_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((Let_syntax.map __pattern_syntax__290_
                                ~f:(function
                                    | x when condition1 && (x > 1) -> 0
                                    | y when condition2 -> 1
                                    | z -> 2))[@ocaml.warning "-26-27"])
                    ~branches:3
                    ~with_:(function
                            | ((0)[@merlin.hide ]) ->
                                let x = __pattern_syntax__290_ in
                                ((let _ : _ = x in BODY)[@merlin.focus ])
                            | ((1)[@merlin.hide ]) ->
                                let y = __pattern_syntax__290_ in ((BODY)
                                  [@merlin.focus ])
                            | ((2)[@merlin.hide ]) ->
                                let z = __pattern_syntax__290_ in ((BODY)
                                  [@merlin.focus ])
                            | _ -> assert false))
                [@nontail ])))
      [@nontail ])
      |}]
  ;;

  let%expect_test "multi-case match%sub with multiple variables" =
    test
      [%expr
        match EXPR with
        | x1, x2 when x1 > 1 -> BODY
        | _ -> BODY];
    [%expect
      {|
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return EXPR)
          ~f:(fun __pattern_syntax__291_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((Let_syntax.map __pattern_syntax__291_
                                ~f:(function
                                    | (x1, x2) when x1 > 1 -> 0
                                    | ((_)[@merlin.focus ]) -> 1))
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
                                                 } __pattern_syntax__291_
                                           ~f:((function
                                                | (_, __pattern_syntax__293_) ->
                                                    __pattern_syntax__293_
                                                | _ -> assert false)
                                           [@ocaml.warning "-11"]))[@merlin.hide ]))
                                    ~f:(fun x2 ->
                                          ((Let_syntax.sub
                                              ~here:{
                                                      Ppx_here_lib.pos_fname =
                                                        "_none_";
                                                      pos_lnum = 0;
                                                      pos_cnum = (-1);
                                                      pos_bol = 0
                                                    }
                                              (Let_syntax.return
                                                 ((Let_syntax.map
                                                     ~here:{
                                                             Ppx_here_lib.pos_fname
                                                               = "_none_";
                                                             pos_lnum = 0;
                                                             pos_cnum = (-1);
                                                             pos_bol = 0
                                                           } __pattern_syntax__291_
                                                     ~f:((function
                                                          | (__pattern_syntax__292_,
                                                             _) ->
                                                              __pattern_syntax__292_
                                                          | _ -> assert false)
                                                     [@ocaml.warning "-11"]))
                                                 [@merlin.hide ]))
                                              ~f:(fun x1 ->
                                                    ((let _ : _ = x1 in BODY)
                                                    [@nontail ])))
                                          [@nontail ])))
                                [@nontail ])
                            | ((1)[@merlin.hide ]) -> BODY
                            | _ -> assert false))
                [@nontail ])))
      [@nontail ])
      |}]
  ;;
end

module%test [@name "arrn nesting edge cases"] _ = struct
  let expand_arr expr =
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.arr Location_of_callsite)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      expr
    |> print_expr
  ;;

  let%expect_test "single let%arr, unpunned" =
    expand_arr
      [%expr
        let a = E in
        MY_BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } ((E)[@ppxlib.enter_value a]) ~f:(fun a -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr on 2 things" =
    expand_arr
      [%expr
        let x1 = E1
        and x2 = E2 in
        MY_BODY];
    [%expect
      {|
      let __let_syntax__295_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__296_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__295_ __let_syntax__296_ ~f:(fun x1 x2 -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr on 6 things" =
    expand_arr
      [%expr
        let x1 = E1
        and x2 = E2
        and x3 = E3
        and x4 = E4
        and x5 = E5
        and x6 = E6 in
        MY_BODY];
    [%expect
      {|
      let __let_syntax__299_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__300_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__301_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__302_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__303_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__304_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ] in
      Let_syntax.arr6
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__299_ __let_syntax__300_ __let_syntax__301_
        __let_syntax__302_ __let_syntax__303_ __let_syntax__304_
        ~f:(fun x1 x2 x3 x4 x5 x6 -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr on 7 things" =
    expand_arr
      [%expr
        let x1 = E1
        and x2 = E2
        and x3 = E3
        and x4 = E4
        and x5 = E5
        and x6 = E6
        and x7 = E7 in
        MY_BODY];
    [%expect
      {|
      let __let_syntax__311_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__312_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__313_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__314_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__315_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__316_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__317_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ] in
      Let_syntax.arr7
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__311_ __let_syntax__312_ __let_syntax__313_
        __let_syntax__314_ __let_syntax__315_ __let_syntax__316_ __let_syntax__317_
        ~f:(fun x1 x2 x3 x4 x5 x6 x7 -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr on 8 things" =
    expand_arr
      [%expr
        let x1 = E1
        and x2 = E2
        and x3 = E3
        and x4 = E4
        and x5 = E5
        and x6 = E6
        and x7 = E7
        and x8 = E8 in
        MY_BODY];
    [%expect
      {|
      let __let_syntax__325_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__326_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__327_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__328_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__329_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__330_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__331_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__332_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                               ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.map7
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__325_ __let_syntax__326_ __let_syntax__327_
           __let_syntax__328_ __let_syntax__329_ __let_syntax__330_
           __let_syntax__331_
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        __let_syntax__332_ ~f:(fun (x1, x2, x3, x4, x5, x6, x7) x8 -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr on 14 things" =
    expand_arr
      [%expr
        let x1 = E1
        and x2 = E2
        and x3 = E3
        and x4 = E4
        and x5 = E5
        and x6 = E6
        and x7 = E7
        and x8 = E8
        and x9 = E9
        and x10 = E10
        and x11 = E11
        and x12 = E12
        and x13 = E13
        and x14 = E14 in
        MY_BODY];
    [%expect
      {|
      let __let_syntax__342_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__343_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__344_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__345_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__346_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__347_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__348_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__349_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__350_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__351_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__352_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__353_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__354_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__355_ = ((E14)[@ppxlib.enter_value x14])[@@ppxlib.do_not_enter_value
                                                                 ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.map7
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__342_ __let_syntax__343_ __let_syntax__344_
           __let_syntax__345_ __let_syntax__346_ __let_syntax__347_
           __let_syntax__348_
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        (Let_syntax.map7
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__349_ __let_syntax__350_ __let_syntax__351_
           __let_syntax__352_ __let_syntax__353_ __let_syntax__354_
           __let_syntax__355_
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        ~f:(fun (x1, x2, x3, x4, x5, x6, x7) (x8, x9, x10, x11, x12, x13, x14) ->
              MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr on 13 things" =
    expand_arr
      [%expr
        let x1 = E1
        and x2 = E2
        and x3 = E3
        and x4 = E4
        and x5 = E5
        and x6 = E6
        and x7 = E7
        and x8 = E8
        and x9 = E9
        and x10 = E10
        and x11 = E11
        and x12 = E12
        and x13 = E13 in
        MY_BODY];
    [%expect
      {|
      let __let_syntax__372_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__373_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__374_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__375_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__376_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__377_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__378_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__379_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__380_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__381_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__382_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__383_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__384_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                 ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.map7
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__372_ __let_syntax__373_ __let_syntax__374_
           __let_syntax__375_ __let_syntax__376_ __let_syntax__377_
           __let_syntax__378_
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        (Let_syntax.map6
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__379_ __let_syntax__380_ __let_syntax__381_
           __let_syntax__382_ __let_syntax__383_ __let_syntax__384_
           ~f:(fun t0 t1 t2 t3 t4 t5 -> (t0, t1, t2, t3, t4, t5)))
        ~f:(fun (x1, x2, x3, x4, x5, x6, x7) (x8, x9, x10, x11, x12, x13) ->
              MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr on 50 things" =
    (* This very long test asserts that we can get 3 levels deep into nesting. *)
    expand_arr
      [%expr
        let x1 = E1
        and x2 = E2
        and x3 = E3
        and x4 = E4
        and x5 = E5
        and x6 = E6
        and x7 = E7
        and x8 = E8
        and x9 = E9
        and x10 = E10
        and x11 = E11
        and x12 = E12
        and x13 = E13
        and x14 = E14
        and x15 = E15
        and x16 = E16
        and x17 = E17
        and x18 = E18
        and x19 = E19
        and x20 = E20
        and x21 = E21
        and x22 = E22
        and x23 = E23
        and x24 = E24
        and x25 = E25
        and x26 = E26
        and x27 = E27
        and x28 = E28
        and x29 = E29
        and x30 = E30
        and x31 = E31
        and x32 = E32
        and x33 = E33
        and x34 = E34
        and x35 = E35
        and x36 = E36
        and x37 = E37
        and x38 = E38
        and x39 = E39
        and x40 = E40
        and x41 = E41
        and x42 = E42
        and x43 = E43
        and x44 = E44
        and x45 = E45
        and x46 = E46
        and x47 = E47
        and x48 = E48
        and x49 = E49
        and x50 = E50 in
        MY_BODY];
    [%expect
      {|
      let __let_syntax__400_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__401_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__402_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__403_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__404_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__405_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__406_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__407_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__408_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__409_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__410_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__411_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__412_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__413_ = ((E14)[@ppxlib.enter_value x14])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__414_ = ((E15)[@ppxlib.enter_value x15])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__415_ = ((E16)[@ppxlib.enter_value x16])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__416_ = ((E17)[@ppxlib.enter_value x17])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__417_ = ((E18)[@ppxlib.enter_value x18])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__418_ = ((E19)[@ppxlib.enter_value x19])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__419_ = ((E20)[@ppxlib.enter_value x20])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__420_ = ((E21)[@ppxlib.enter_value x21])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__421_ = ((E22)[@ppxlib.enter_value x22])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__422_ = ((E23)[@ppxlib.enter_value x23])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__423_ = ((E24)[@ppxlib.enter_value x24])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__424_ = ((E25)[@ppxlib.enter_value x25])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__425_ = ((E26)[@ppxlib.enter_value x26])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__426_ = ((E27)[@ppxlib.enter_value x27])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__427_ = ((E28)[@ppxlib.enter_value x28])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__428_ = ((E29)[@ppxlib.enter_value x29])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__429_ = ((E30)[@ppxlib.enter_value x30])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__430_ = ((E31)[@ppxlib.enter_value x31])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__431_ = ((E32)[@ppxlib.enter_value x32])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__432_ = ((E33)[@ppxlib.enter_value x33])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__433_ = ((E34)[@ppxlib.enter_value x34])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__434_ = ((E35)[@ppxlib.enter_value x35])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__435_ = ((E36)[@ppxlib.enter_value x36])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__436_ = ((E37)[@ppxlib.enter_value x37])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__437_ = ((E38)[@ppxlib.enter_value x38])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__438_ = ((E39)[@ppxlib.enter_value x39])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__439_ = ((E40)[@ppxlib.enter_value x40])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__440_ = ((E41)[@ppxlib.enter_value x41])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__441_ = ((E42)[@ppxlib.enter_value x42])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__442_ = ((E43)[@ppxlib.enter_value x43])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__443_ = ((E44)[@ppxlib.enter_value x44])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__444_ = ((E45)[@ppxlib.enter_value x45])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__445_ = ((E46)[@ppxlib.enter_value x46])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__446_ = ((E47)[@ppxlib.enter_value x47])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__447_ = ((E48)[@ppxlib.enter_value x48])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__448_ = ((E49)[@ppxlib.enter_value x49])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__449_ = ((E50)[@ppxlib.enter_value x50])[@@ppxlib.do_not_enter_value
                                                                 ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.map7
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 }
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__400_ __let_syntax__401_ __let_syntax__402_
              __let_syntax__403_ __let_syntax__404_ __let_syntax__405_
              __let_syntax__406_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__407_ __let_syntax__408_ __let_syntax__409_
              __let_syntax__410_ __let_syntax__411_ __let_syntax__412_
              __let_syntax__413_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__414_ __let_syntax__415_ __let_syntax__416_
              __let_syntax__417_ __let_syntax__418_ __let_syntax__419_
              __let_syntax__420_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__421_ __let_syntax__422_ __let_syntax__423_
              __let_syntax__424_ __let_syntax__425_ __let_syntax__426_
              __let_syntax__427_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__428_ __let_syntax__429_ __let_syntax__430_
              __let_syntax__431_ __let_syntax__432_ __let_syntax__433_
              __let_syntax__434_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__435_ __let_syntax__436_ __let_syntax__437_
              __let_syntax__438_ __let_syntax__439_ __let_syntax__440_
              __let_syntax__441_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (Let_syntax.map7
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__442_ __let_syntax__443_ __let_syntax__444_
              __let_syntax__445_ __let_syntax__446_ __let_syntax__447_
              __let_syntax__448_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        __let_syntax__449_
        ~f:(fun
              ((x1, x2, x3, x4, x5, x6, x7), (x8, x9, x10, x11, x12, x13, x14),
               (x15, x16, x17, x18, x19, x20, x21),
               (x22, x23, x24, x25, x26, x27, x28),
               (x29, x30, x31, x32, x33, x34, x35),
               (x36, x37, x38, x39, x40, x41, x42),
               (x43, x44, x45, x46, x47, x48, x49))
              x50 -> MY_BODY)
      |}]
  ;;
end
