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
              } ((MY_EXPR)[@ppxlib.enter_value a])
        ~f:(fun a -> let () = ()[@@merlin.hide ] in MY_BODY))
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
        ~f:(fun a -> let () = ()[@@merlin.hide ] in MY_BODY))
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
              } ((MY_EXPR)[@ppxlib.enter_value a])
        ~f:(fun a -> let () = ()[@@merlin.hide ] in MY_BODY))
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

let%expect_test "double pattern let%sub combines the bindings with [sub_both]" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a = MY_EXPR_1
      and b = MY_EXPR_2 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__004_ = ((MY_EXPR_1)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                   ]
    and __let_syntax__005_ = ((MY_EXPR_2)[@ppxlib.enter_value b])[@@ppxlib.do_not_enter_value
                                                                   ] in
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.sub_both __let_syntax__004_ __let_syntax__005_)
        ~f:(fun __pattern_syntax__006_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__006_
                         ~f:(function
                             | (_, __pattern_syntax__008_) ->
                                 __pattern_syntax__008_))[@merlin.hide ]))
                  ~f:(fun b ->
                        ((Let_syntax.sub
                            ~here:{
                                    Ppx_here_lib.pos_fname = "_none_";
                                    pos_lnum = 0;
                                    pos_cnum = (-1);
                                    pos_bol = 0
                                  }
                            (Let_syntax.return
                               ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                   ~here:{
                                           Ppx_here_lib.pos_fname = "_none_";
                                           pos_lnum = 0;
                                           pos_cnum = (-1);
                                           pos_bol = 0
                                         } __pattern_syntax__006_
                                   ~f:(function
                                       | (__pattern_syntax__007_, _) ->
                                           __pattern_syntax__007_))
                               [@merlin.hide ]))
                            ~f:(fun a ->
                                  let () = ()[@@merlin.hide ] in ((MY_BODY)
                                    [@nontail ])))
                        [@nontail ])))
              [@nontail ])))
      [@nontail ])
    |}]
;;

let%expect_test "triple pattern let%sub" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    [%expr
      let a = MY_EXPR_1
      and b = MY_EXPR_2
      and c = MY_EXPR_3 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__009_ = ((MY_EXPR_1)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                   ]
    and __let_syntax__010_ = ((MY_EXPR_2)[@ppxlib.enter_value b])[@@ppxlib.do_not_enter_value
                                                                   ]
    and __let_syntax__011_ = ((MY_EXPR_3)[@ppxlib.enter_value c])[@@ppxlib.do_not_enter_value
                                                                   ] in
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.sub_both __let_syntax__009_
           (Let_syntax.sub_both __let_syntax__010_ __let_syntax__011_))
        ~f:(fun __pattern_syntax__012_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__012_
                         ~f:(function
                             | (_, (_, __pattern_syntax__015_)) ->
                                 __pattern_syntax__015_))[@merlin.hide ]))
                  ~f:(fun c ->
                        ((Let_syntax.sub
                            ~here:{
                                    Ppx_here_lib.pos_fname = "_none_";
                                    pos_lnum = 0;
                                    pos_cnum = (-1);
                                    pos_bol = 0
                                  }
                            (Let_syntax.return
                               ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                   ~here:{
                                           Ppx_here_lib.pos_fname = "_none_";
                                           pos_lnum = 0;
                                           pos_cnum = (-1);
                                           pos_bol = 0
                                         } __pattern_syntax__012_
                                   ~f:(function
                                       | (_, (__pattern_syntax__014_, _)) ->
                                           __pattern_syntax__014_))
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
                                         ((((Let_syntax.map)
                                             [@alert "-for_ppx_bonsai"])
                                             ~here:{
                                                     Ppx_here_lib.pos_fname =
                                                       "_none_";
                                                     pos_lnum = 0;
                                                     pos_cnum = (-1);
                                                     pos_bol = 0
                                                   } __pattern_syntax__012_
                                             ~f:(function
                                                 | (__pattern_syntax__013_,
                                                    (_, _)) ->
                                                     __pattern_syntax__013_))
                                         [@merlin.hide ]))
                                      ~f:(fun a ->
                                            let () = ()[@@merlin.hide ] in
                                            ((MY_BODY)[@nontail ])))
                                  [@nontail ])))
                        [@nontail ])))
              [@nontail ])))
      [@nontail ])
    |}]
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
              } ((MY_EXPR_1)[@ppxlib.enter_value a])
        ~f:(fun a -> let () = ()[@@merlin.hide ] in MY_BODY))
    [@nontail ])
    |}]
;;

let%expect_test "double pattern sub open combines the bindings with [sub_both]" =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default_open
    ~modul:None
    ~locality
    [%expr
      let a = MY_EXPR_1
      and b = MY_EXPR_2 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__017_ =
      let open! Let_syntax.Open_on_rhs in ((MY_EXPR_1)[@ppxlib.enter_value a])
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__018_ =
      let open! Let_syntax.Open_on_rhs in ((MY_EXPR_2)[@ppxlib.enter_value b])
    [@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.sub_both __let_syntax__017_ __let_syntax__018_)
        ~f:(fun __pattern_syntax__019_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__019_
                         ~f:(function
                             | (_, __pattern_syntax__021_) ->
                                 __pattern_syntax__021_))[@merlin.hide ]))
                  ~f:(fun b ->
                        ((Let_syntax.sub
                            ~here:{
                                    Ppx_here_lib.pos_fname = "_none_";
                                    pos_lnum = 0;
                                    pos_cnum = (-1);
                                    pos_bol = 0
                                  }
                            (Let_syntax.return
                               ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                   ~here:{
                                           Ppx_here_lib.pos_fname = "_none_";
                                           pos_lnum = 0;
                                           pos_cnum = (-1);
                                           pos_bol = 0
                                         } __pattern_syntax__019_
                                   ~f:(function
                                       | (__pattern_syntax__020_, _) ->
                                           __pattern_syntax__020_))
                               [@merlin.hide ]))
                            ~f:(fun a ->
                                  let () = ()[@@merlin.hide ] in ((MY_BODY)
                                    [@nontail ])))
                        [@nontail ])))
              [@nontail ])))
      [@nontail ])
    |}]
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
        ~f:(fun __pattern_syntax__022_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                              __pattern_syntax__022_
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
        ~f:(fun __pattern_syntax__024_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__024_
                         ~f:(function
                             | (_, { b = _; c = __pattern_syntax__027_ }) ->
                                 __pattern_syntax__027_))[@merlin.hide ]))
                  ~f:(fun c ->
                        ((Let_syntax.sub
                            ~here:{
                                    Ppx_here_lib.pos_fname = "_none_";
                                    pos_lnum = 0;
                                    pos_cnum = (-1);
                                    pos_bol = 0
                                  }
                            (Let_syntax.return
                               ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                   ~here:{
                                           Ppx_here_lib.pos_fname = "_none_";
                                           pos_lnum = 0;
                                           pos_cnum = (-1);
                                           pos_bol = 0
                                         } __pattern_syntax__024_
                                   ~f:(function
                                       | (_,
                                          { b = __pattern_syntax__026_; c = _ })
                                           -> __pattern_syntax__026_))
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
                                         ((((Let_syntax.map)
                                             [@alert "-for_ppx_bonsai"])
                                             ~here:{
                                                     Ppx_here_lib.pos_fname =
                                                       "_none_";
                                                     pos_lnum = 0;
                                                     pos_cnum = (-1);
                                                     pos_bol = 0
                                                   } __pattern_syntax__024_
                                             ~f:(function
                                                 | (__pattern_syntax__025_,
                                                    { b = _; c = _ }) ->
                                                     __pattern_syntax__025_))
                                         [@merlin.hide ]))
                                      ~f:(fun a ->
                                            let () = ()[@@merlin.hide ] in
                                            ((MY_BODY)[@nontail ])))
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
        ~f:(fun __pattern_syntax__028_ ->
              ((Let_syntax.sub ~here
                  (Let_syntax.return
                     ((((Let_syntax.map)[@alert "-for_ppx_bonsai"]) ~here
                         __pattern_syntax__028_
                         ~f:(function
                             | (_, { b = _; c = __pattern_syntax__031_ }) ->
                                 __pattern_syntax__031_))[@merlin.hide ]))
                  ~f:(fun c ->
                        ((Let_syntax.sub ~here
                            (Let_syntax.return
                               ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                   ~here __pattern_syntax__028_
                                   ~f:(function
                                       | (_,
                                          { b = __pattern_syntax__030_; c = _ })
                                           -> __pattern_syntax__030_))
                               [@merlin.hide ]))
                            ~f:(fun b ->
                                  ((Let_syntax.sub ~here
                                      (Let_syntax.return
                                         ((((Let_syntax.map)
                                             [@alert "-for_ppx_bonsai"]) ~here
                                             __pattern_syntax__028_
                                             ~f:(function
                                                 | (__pattern_syntax__029_,
                                                    { b = _; c = _ }) ->
                                                     __pattern_syntax__029_))
                                         [@merlin.hide ]))
                                      ~f:(fun a ->
                                            let () = ()[@@merlin.hide ] in
                                            ((MY_BODY)[@nontail ])))
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
    -|                 ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
    -|                     ~here:{
    -|                             Ppx_here_lib.pos_fname = "_none_";
    -|                             pos_lnum = 0;
    -|                             pos_cnum = (-1);
    -|                             pos_bol = 0
    -|                           } __pattern_syntax__ID_REPLACED_IN_TEST_
    +|              (Let_syntax.return
    +|                 ((((Let_syntax.map)[@alert "-for_ppx_bonsai"]) ~here
    +|                     __pattern_syntax__ID_REPLACED_IN_TEST_
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
    -|                           ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
    -|                               ~here:{
    -|                                       Ppx_here_lib.pos_fname = "_none_";
    -|                                       pos_lnum = 0;
    -|                                       pos_cnum = (-1);
    -|                                       pos_bol = 0
    -|                                     } __pattern_syntax__ID_REPLACED_IN_TEST_
    +|                        (Let_syntax.return
    +|                           ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
    +|                               ~here __pattern_syntax__ID_REPLACED_IN_TEST_
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
    -|                                     ((((Let_syntax.map)
    -|                                         [@alert "-for_ppx_bonsai"])
    -|                                         ~here:{
    -|                                                 Ppx_here_lib.pos_fname =
    -|                                                   "_none_";
    -|                                                 pos_lnum = 0;
    -|                                                 pos_cnum = (-1);
    -|                                                 pos_bol = 0
    -|                                               } __pattern_syntax__ID_REPLACED_IN_TEST_
    +|                                  (Let_syntax.return
    +|                                     ((((Let_syntax.map)
    +|                                         [@alert "-for_ppx_bonsai"]) ~here
    +|                                         __pattern_syntax__ID_REPLACED_IN_TEST_
                                               ~f:(function
                                                   | (__pattern_syntax__ID_REPLACED_IN_TEST_,
                                                      { b = _; c = _ }) ->
                                                       __pattern_syntax__ID_REPLACED_IN_TEST_))
                                           [@merlin.hide ]))
                                        ~f:(fun a ->
                                              let () = ()[@@merlin.hide ] in
                                              ((MY_BODY)[@nontail ])))
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
        ~f:(fun __pattern_syntax__040_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                              __pattern_syntax__040_
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
                                     ((((Let_syntax.map)
                                         [@alert "-for_ppx_bonsai"])
                                         ~here:{
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 0;
                                                 pos_cnum = (-1);
                                                 pos_bol = 0
                                               } __pattern_syntax__040_
                                         ~f:((function
                                              | Choice_1
                                                  (_, __pattern_syntax__042_) ->
                                                  __pattern_syntax__042_
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
                                               ((((Let_syntax.map)
                                                   [@alert "-for_ppx_bonsai"])
                                                   ~here:{
                                                           Ppx_here_lib.pos_fname
                                                             = "_none_";
                                                           pos_lnum = 0;
                                                           pos_cnum = (-1);
                                                           pos_bol = 0
                                                         } __pattern_syntax__040_
                                                   ~f:((function
                                                        | Choice_1
                                                            (__pattern_syntax__041_,
                                                             _)
                                                            ->
                                                            __pattern_syntax__041_
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
        ~f:(fun __pattern_syntax__043_ ->
              ((Let_syntax.switch ~here
                  ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                              __pattern_syntax__043_
                              ~f:(function
                                  | Choice_1 (a, b) -> 0
                                  | Choice_2 ((_)[@merlin.focus ]) -> 1
                                  | Choice_3 -> 2))[@ocaml.warning "-26-27"])
                  ~branches:3
                  ~with_:(function
                          | ((0)[@merlin.hide ]) ->
                              ((Let_syntax.sub ~here
                                  (Let_syntax.return
                                     ((((Let_syntax.map)
                                         [@alert "-for_ppx_bonsai"]) ~here
                                         __pattern_syntax__043_
                                         ~f:((function
                                              | Choice_1
                                                  (_, __pattern_syntax__045_) ->
                                                  __pattern_syntax__045_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun b ->
                                        ((Let_syntax.sub ~here
                                            (Let_syntax.return
                                               ((((Let_syntax.map)
                                                   [@alert "-for_ppx_bonsai"])
                                                   ~here __pattern_syntax__043_
                                                   ~f:((function
                                                        | Choice_1
                                                            (__pattern_syntax__044_,
                                                             _)
                                                            ->
                                                            __pattern_syntax__044_
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
        ~f:(fun __pattern_syntax__046_ ->
              ((Module.Let_syntax.Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Module.Let_syntax.Let_syntax.return
                     ((((Module.Let_syntax.Let_syntax.map)
                         [@alert "-for_ppx_bonsai"])
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__046_
                         ~f:(function
                             | Choice_1 __pattern_syntax__047_ ->
                                 __pattern_syntax__047_))[@merlin.hide ]))
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
        ~f:(fun __pattern_syntax__048_ ->
              ((Module.Let_syntax.Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((((Module.Let_syntax.Let_syntax.map)
                              [@alert "-for_ppx_bonsai"]) __pattern_syntax__048_
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
                                     ((((Module.Let_syntax.Let_syntax.map)
                                         [@alert "-for_ppx_bonsai"])
                                         ~here:{
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 0;
                                                 pos_cnum = (-1);
                                                 pos_bol = 0
                                               } __pattern_syntax__048_
                                         ~f:((function
                                              | Choice_1 __pattern_syntax__049_
                                                  -> __pattern_syntax__049_
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
                                     ((((Module.Let_syntax.Let_syntax.map)
                                         [@alert "-for_ppx_bonsai"])
                                         ~here:{
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 0;
                                                 pos_cnum = (-1);
                                                 pos_bol = 0
                                               } __pattern_syntax__048_
                                         ~f:((function
                                              | Choice_2 __pattern_syntax__050_
                                                  -> __pattern_syntax__050_
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
        ~f:(fun __pattern_syntax__051_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 0;
                                 pos_cnum = (-1);
                                 pos_bol = 0
                               } __pattern_syntax__051_
                         ~f:(function | (_ : int) -> ()))[@nontail ]))
                  ~f:(fun _ -> let () = ()[@@merlin.hide ] in ((BODY)[@nontail ])))
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
    fun __let_syntax__052_ ->
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 0;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return __let_syntax__052_)
          ~f:(fun __pattern_syntax__053_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                __pattern_syntax__053_
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
                                       ((((Let_syntax.map)
                                           [@alert "-for_ppx_bonsai"])
                                           ~here:{
                                                   Ppx_here_lib.pos_fname =
                                                     "_none_";
                                                   pos_lnum = 0;
                                                   pos_cnum = (-1);
                                                   pos_bol = 0
                                                 } __pattern_syntax__053_
                                           ~f:((function
                                                | Some __pattern_syntax__054_ ->
                                                    __pattern_syntax__054_
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
    fun __let_syntax__055_ ->
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__055_
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
                   (__old_for_cutoff__057_,
                    { b = __old_for_cutoff__059_; c = __old_for_cutoff__061_;_})
                   (__new_for_cutoff__056_,
                    { b = __new_for_cutoff__058_; c = __new_for_cutoff__060_;_})
                   ->
                   (phys_equal __old_for_cutoff__061_ __new_for_cutoff__060_) &&
                     ((phys_equal __old_for_cutoff__059_ __new_for_cutoff__058_)
                        &&
                        (phys_equal __old_for_cutoff__057_ __new_for_cutoff__056_))))
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
                   (`Foo (__old_for_cutoff__066_, _)
                    | `Bar (_, __old_for_cutoff__066_))
                   (`Foo (__new_for_cutoff__065_, _)
                    | `Bar (_, __new_for_cutoff__065_))
                   -> phys_equal __old_for_cutoff__066_ __new_for_cutoff__065_))
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
                   (__old_for_cutoff__069_,
                    { b = __old_for_cutoff__071_; c = __old_for_cutoff__073_;
                      d = _; e = _; f = _; g = { h = _; i = { k = _ } } })
                   (__new_for_cutoff__068_,
                    { b = __new_for_cutoff__070_; c = __new_for_cutoff__072_;
                      d = _; e = _; f = _; g = { h = _; i = { k = _ } } })
                   ->
                   (phys_equal __old_for_cutoff__073_ __new_for_cutoff__072_) &&
                     ((phys_equal __old_for_cutoff__071_ __new_for_cutoff__070_)
                        &&
                        (phys_equal __old_for_cutoff__069_ __new_for_cutoff__068_))))
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
                   (__old_for_cutoff__076_,
                    { b = __old_for_cutoff__078_; c = __old_for_cutoff__080_;_})
                   (__new_for_cutoff__075_,
                    { b = __new_for_cutoff__077_; c = __new_for_cutoff__079_;_})
                   ->
                   (phys_equal __old_for_cutoff__080_ __new_for_cutoff__079_) &&
                     ((phys_equal __old_for_cutoff__078_ __new_for_cutoff__077_)
                        &&
                        (phys_equal __old_for_cutoff__076_ __new_for_cutoff__075_))))
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
    let __let_syntax__082_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__083_ = OTHER_EXPR[@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff __let_syntax__082_
         ~equal:(fun
                   (__old_for_cutoff__085_,
                    { b = __old_for_cutoff__087_; c = __old_for_cutoff__089_;_})
                   (__new_for_cutoff__084_,
                    { b = __new_for_cutoff__086_; c = __new_for_cutoff__088_;_})
                   ->
                   (phys_equal __old_for_cutoff__089_ __new_for_cutoff__088_) &&
                     ((phys_equal __old_for_cutoff__087_ __new_for_cutoff__086_)
                        &&
                        (phys_equal __old_for_cutoff__085_ __new_for_cutoff__084_))))
      (Let_syntax.cutoff __let_syntax__083_
         ~equal:(fun
                   (__old_for_cutoff__091_,
                    { y = __old_for_cutoff__093_; z = __old_for_cutoff__095_;_})
                   (__new_for_cutoff__090_,
                    { y = __new_for_cutoff__092_; z = __new_for_cutoff__094_;_})
                   ->
                   (phys_equal __old_for_cutoff__095_ __new_for_cutoff__094_) &&
                     ((phys_equal __old_for_cutoff__093_ __new_for_cutoff__092_)
                        &&
                        (phys_equal __old_for_cutoff__091_ __new_for_cutoff__090_))))
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
    let __let_syntax__098_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__099_ = ((OTHER_EXPR)[@ppxlib.enter_value y])[@@ppxlib.do_not_enter_value
                                                                    ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff __let_syntax__098_
         ~equal:(fun
                   (__old_for_cutoff__101_,
                    { b = __old_for_cutoff__103_; c = __old_for_cutoff__105_;_})
                   (__new_for_cutoff__100_,
                    { b = __new_for_cutoff__102_; c = __new_for_cutoff__104_;_})
                   ->
                   (phys_equal __old_for_cutoff__105_ __new_for_cutoff__104_) &&
                     ((phys_equal __old_for_cutoff__103_ __new_for_cutoff__102_)
                        &&
                        (phys_equal __old_for_cutoff__101_ __new_for_cutoff__100_))))
      __let_syntax__099_ ~f:(fun (a, { b; c;_}) y -> MY_BODY)
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
    let __let_syntax__109_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__110_ = OTHER_EXPR[@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            } __let_syntax__109_ __let_syntax__110_
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
      let __let_syntax__114_ = ((NO_DESTRUCTION)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                          ]
      and __let_syntax__115_ = ((ALSO_NO_DESTRUCTION)[@ppxlib.enter_value b])
      [@@ppxlib.do_not_enter_value ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__114_ __let_syntax__115_ ~f:(fun a b -> BODY)
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
      let __let_syntax__118_ = ((NO_DESTRUCTION)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                          ]
      and __let_syntax__119_ = ((ALSO_NO_DESTRUCTION)[@ppxlib.enter_value b])
      [@@ppxlib.do_not_enter_value ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__118_ __let_syntax__119_
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
           ~equal:(fun ((_ as __old_for_cutoff__123_), _)
                     ((_ as __new_for_cutoff__122_), _) ->
                     phys_equal __old_for_cutoff__123_ __new_for_cutoff__122_))
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
                     { foo = __old_for_cutoff__126_;
                       bar = { baz = __old_for_cutoff__128_;_};_}
                     { foo = __new_for_cutoff__125_;
                       bar = { baz = __new_for_cutoff__127_;_};_}
                     ->
                     (phys_equal __old_for_cutoff__126_ __new_for_cutoff__125_) &&
                       (phys_equal __old_for_cutoff__128_ __new_for_cutoff__127_)))
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
           ~equal:(fun (((__old_for_cutoff__131_, _) : t), __old_for_cutoff__133_)
                     (((__new_for_cutoff__130_, _) : t), __new_for_cutoff__132_) ->
                     (phys_equal __old_for_cutoff__133_ __new_for_cutoff__132_) &&
                       (phys_equal __old_for_cutoff__131_ __new_for_cutoff__130_)))
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
      let __let_syntax__135_ = DESTRUCT[@@ppxlib.do_not_enter_value ]
      and __let_syntax__136_ = ALSO_DESTRUCT[@@ppxlib.do_not_enter_value ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.cutoff __let_syntax__135_
           ~equal:(fun (((__old_for_cutoff__138_, _) : t), __old_for_cutoff__140_)
                     (((__new_for_cutoff__137_, _) : t), __new_for_cutoff__139_) ->
                     (phys_equal __old_for_cutoff__140_ __new_for_cutoff__139_) &&
                       (phys_equal __old_for_cutoff__138_ __new_for_cutoff__137_)))
        (Let_syntax.cutoff __let_syntax__136_
           ~equal:(fun (((__old_for_cutoff__142_, _) : t), __old_for_cutoff__144_)
                     (((__new_for_cutoff__141_, _) : t), __new_for_cutoff__143_) ->
                     (phys_equal __old_for_cutoff__144_ __new_for_cutoff__143_) &&
                       (phys_equal __old_for_cutoff__142_ __new_for_cutoff__141_)))
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
      let __let_syntax__148_ = NO_DESTRUCTION[@@ppxlib.do_not_enter_value ]
      and __let_syntax__149_ = ALSO_NO_DESTRUCTION[@@ppxlib.do_not_enter_value ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__148_ __let_syntax__149_
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
                     (({ a = __old_for_cutoff__154_; b = (module _)  } : t),
                      __old_for_cutoff__156_)
                     (({ a = __new_for_cutoff__153_; b = (module _)  } : t),
                      __new_for_cutoff__155_)
                     ->
                     (phys_equal __old_for_cutoff__156_ __new_for_cutoff__155_) &&
                       (phys_equal __old_for_cutoff__154_ __new_for_cutoff__153_)))
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
           ~equal:(fun { a = __old_for_cutoff__162_; b = __old_for_cutoff__162_;_}
                     { a = __new_for_cutoff__161_; b = __new_for_cutoff__161_;_} ->
                     phys_equal __old_for_cutoff__162_ __new_for_cutoff__161_))
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
         ~equal:(fun __old_for_cutoff_ppx_bonsai__166_
                   __new_for_cutoff_ppx_bonsai__167_ ->
                   ((match (__old_for_cutoff_ppx_bonsai__166_,
                             __new_for_cutoff_ppx_bonsai__167_)
                     with
                     | (A (__old_for_cutoff__165_, _), A
                        (__new_for_cutoff__164_, _)) ->
                         phys_equal __old_for_cutoff__165_ __new_for_cutoff__164_
                     | (A (__new_for_cutoff__164_, _), _)
                       | (_, A (__new_for_cutoff__164_, _)) -> false
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
         ~equal:(fun __old_for_cutoff_ppx_bonsai__170_
                   __new_for_cutoff_ppx_bonsai__171_ ->
                   ((match (__old_for_cutoff_ppx_bonsai__170_,
                             __new_for_cutoff_ppx_bonsai__171_)
                     with
                     | (A (1, _), A (1, _)) -> true
                     | (A (1, _), _) | (_, A (1, _)) -> false
                     | (A (_, 2), A (_, 2)) -> true
                     | (A (_, 2), _) | (_, A (_, 2)) -> false
                     | (A (__old_for_cutoff__169_, _), A
                        (__new_for_cutoff__168_, _)) ->
                         phys_equal __old_for_cutoff__169_ __new_for_cutoff__168_
                     | (A (__new_for_cutoff__168_, _), _)
                       | (_, A (__new_for_cutoff__168_, _)) -> false)
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
         ~equal:(fun __old_for_cutoff_ppx_bonsai__176_
                   __new_for_cutoff_ppx_bonsai__177_ ->
                   ((match (__old_for_cutoff_ppx_bonsai__176_,
                             __new_for_cutoff_ppx_bonsai__177_)
                     with
                     | ((A (__old_for_cutoff__175_, _) | B
                         (_, __old_for_cutoff__175_)),
                        (A (__new_for_cutoff__174_, _) | B
                         (_, __new_for_cutoff__174_))) ->
                         phys_equal __old_for_cutoff__175_ __new_for_cutoff__174_
                     | ((A (__new_for_cutoff__174_, _) | B
                         (_, __new_for_cutoff__174_)),
                        _)
                       | (_,
                          (A (__new_for_cutoff__174_, _) | B
                           (_, __new_for_cutoff__174_)))
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
         ~equal:(fun __old_for_cutoff_ppx_bonsai__182_
                   __new_for_cutoff_ppx_bonsai__183_ ->
                   ((match (__old_for_cutoff_ppx_bonsai__182_,
                             __new_for_cutoff_ppx_bonsai__183_)
                     with
                     | ((A (__old_for_cutoff__179_, _) as __old_for_cutoff__181_),
                        (A (__new_for_cutoff__178_, _) as __new_for_cutoff__180_))
                         ->
                         (phys_equal __old_for_cutoff__181_
                            __new_for_cutoff__180_)
                           &&
                           (phys_equal __old_for_cutoff__179_
                              __new_for_cutoff__178_)
                     | ((A (__new_for_cutoff__178_, _) as __new_for_cutoff__180_),
                        _)
                       | (_,
                          (A (__new_for_cutoff__178_, _) as
                             __new_for_cutoff__180_))
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
        (let __let_syntax__186_ = ((EXPR1)
           [@ppxlib.enter_value __ppx_bonsai_tuple__184_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__187_ = ((EXPR2)
           [@ppxlib.enter_value __ppx_bonsai_tuple__185_])[@@ppxlib.do_not_enter_value
                                                            ] in
         ((Let_syntax.map2)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__186_ __let_syntax__187_
           ~f:(fun __ppx_bonsai_tuple__184_ __ppx_bonsai_tuple__185_ ->
                 (__ppx_bonsai_tuple__184_, __ppx_bonsai_tuple__185_)))
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
        (let __let_syntax__195_ = ((EXPR1)
           [@ppxlib.enter_value __ppx_bonsai_tuple__190_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__196_ = ((EXPR2)
           [@ppxlib.enter_value __ppx_bonsai_tuple__191_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__197_ = ((EXPR3)
           [@ppxlib.enter_value __ppx_bonsai_tuple__192_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__198_ = ((EXPR4)
           [@ppxlib.enter_value __ppx_bonsai_tuple__193_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__199_ = ((EXPR5)
           [@ppxlib.enter_value __ppx_bonsai_tuple__194_])[@@ppxlib.do_not_enter_value
                                                            ] in
         ((Let_syntax.map5)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__195_ __let_syntax__196_ __let_syntax__197_
           __let_syntax__198_ __let_syntax__199_
           ~f:(fun __ppx_bonsai_tuple__190_ __ppx_bonsai_tuple__191_
                 __ppx_bonsai_tuple__192_ __ppx_bonsai_tuple__193_
                 __ppx_bonsai_tuple__194_ ->
                 (__ppx_bonsai_tuple__190_, __ppx_bonsai_tuple__191_,
                   __ppx_bonsai_tuple__192_, __ppx_bonsai_tuple__193_,
                   __ppx_bonsai_tuple__194_)))
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
        (let __let_syntax__225_ = ((EXPR1)
           [@ppxlib.enter_value __ppx_bonsai_tuple__205_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__226_ = ((EXPR2)
           [@ppxlib.enter_value __ppx_bonsai_tuple__206_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__227_ = ((EXPR3)
           [@ppxlib.enter_value __ppx_bonsai_tuple__207_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__228_ = ((EXPR4)
           [@ppxlib.enter_value __ppx_bonsai_tuple__208_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__229_ = ((EXPR5)
           [@ppxlib.enter_value __ppx_bonsai_tuple__209_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__230_ = ((EXPR6)
           [@ppxlib.enter_value __ppx_bonsai_tuple__210_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__231_ = ((EXPR7)
           [@ppxlib.enter_value __ppx_bonsai_tuple__211_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__232_ = ((EXPR8)
           [@ppxlib.enter_value __ppx_bonsai_tuple__212_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__233_ = ((EXPR9)
           [@ppxlib.enter_value __ppx_bonsai_tuple__213_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__234_ = ((EXPR10)
           [@ppxlib.enter_value __ppx_bonsai_tuple__214_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__235_ = ((EXPR11)
           [@ppxlib.enter_value __ppx_bonsai_tuple__215_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__236_ = ((EXPR12)
           [@ppxlib.enter_value __ppx_bonsai_tuple__216_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__237_ = ((EXPR13)
           [@ppxlib.enter_value __ppx_bonsai_tuple__217_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__238_ = ((EXPR14)
           [@ppxlib.enter_value __ppx_bonsai_tuple__218_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__239_ = ((EXPR15)
           [@ppxlib.enter_value __ppx_bonsai_tuple__219_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__240_ = ((EXPR16)
           [@ppxlib.enter_value __ppx_bonsai_tuple__220_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__241_ = ((EXPR17)
           [@ppxlib.enter_value __ppx_bonsai_tuple__221_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__242_ = ((EXPR18)
           [@ppxlib.enter_value __ppx_bonsai_tuple__222_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__243_ = ((EXPR19)
           [@ppxlib.enter_value __ppx_bonsai_tuple__223_])[@@ppxlib.do_not_enter_value
                                                            ]
         and __let_syntax__244_ = ((EXPR20)
           [@ppxlib.enter_value __ppx_bonsai_tuple__224_])[@@ppxlib.do_not_enter_value
                                                            ] in
         ((Let_syntax.map3)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 }
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__225_ __let_syntax__226_ __let_syntax__227_
              __let_syntax__228_ __let_syntax__229_ __let_syntax__230_
              __let_syntax__231_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__232_ __let_syntax__233_ __let_syntax__234_
              __let_syntax__235_ __let_syntax__236_ __let_syntax__237_
              __let_syntax__238_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (((Let_syntax.map6)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__239_ __let_syntax__240_ __let_syntax__241_
              __let_syntax__242_ __let_syntax__243_ __let_syntax__244_
              ~f:(fun t0 t1 t2 t3 t4 t5 -> (t0, t1, t2, t3, t4, t5)))
           ~f:(fun
                 (__ppx_bonsai_tuple__205_, __ppx_bonsai_tuple__206_,
                  __ppx_bonsai_tuple__207_, __ppx_bonsai_tuple__208_,
                  __ppx_bonsai_tuple__209_, __ppx_bonsai_tuple__210_,
                  __ppx_bonsai_tuple__211_)
                 (__ppx_bonsai_tuple__212_, __ppx_bonsai_tuple__213_,
                  __ppx_bonsai_tuple__214_, __ppx_bonsai_tuple__215_,
                  __ppx_bonsai_tuple__216_, __ppx_bonsai_tuple__217_,
                  __ppx_bonsai_tuple__218_)
                 (__ppx_bonsai_tuple__219_, __ppx_bonsai_tuple__220_,
                  __ppx_bonsai_tuple__221_, __ppx_bonsai_tuple__222_,
                  __ppx_bonsai_tuple__223_, __ppx_bonsai_tuple__224_)
                 ->
                 (__ppx_bonsai_tuple__205_, __ppx_bonsai_tuple__206_,
                   __ppx_bonsai_tuple__207_, __ppx_bonsai_tuple__208_,
                   __ppx_bonsai_tuple__209_, __ppx_bonsai_tuple__210_,
                   __ppx_bonsai_tuple__211_, __ppx_bonsai_tuple__212_,
                   __ppx_bonsai_tuple__213_, __ppx_bonsai_tuple__214_,
                   __ppx_bonsai_tuple__215_, __ppx_bonsai_tuple__216_,
                   __ppx_bonsai_tuple__217_, __ppx_bonsai_tuple__218_,
                   __ppx_bonsai_tuple__219_, __ppx_bonsai_tuple__220_,
                   __ppx_bonsai_tuple__221_, __ppx_bonsai_tuple__222_,
                   __ppx_bonsai_tuple__223_, __ppx_bonsai_tuple__224_)))
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
           (let __let_syntax__270_ = ((EXPR1)
              [@ppxlib.enter_value __ppx_bonsai_tuple__268_])[@@ppxlib.do_not_enter_value
                                                               ]
            and __let_syntax__271_ = ((EXPR2)
              [@ppxlib.enter_value __ppx_bonsai_tuple__269_])[@@ppxlib.do_not_enter_value
                                                               ] in
            ((Let_syntax.map2)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__270_ __let_syntax__271_
              ~f:(fun __ppx_bonsai_tuple__268_ __ppx_bonsai_tuple__269_ ->
                    (__ppx_bonsai_tuple__268_, __ppx_bonsai_tuple__269_)))
           ~equal:(fun __old_for_cutoff_ppx_bonsai__276_
                     __new_for_cutoff_ppx_bonsai__277_ ->
                     ((match (__old_for_cutoff_ppx_bonsai__276_,
                               __new_for_cutoff_ppx_bonsai__277_)
                       with
                       | ((Some _, _), (Some _, _)) -> true
                       | ((Some _, _), _) | (_, (Some _, _)) -> false
                       | ((None, Some (_, __old_for_cutoff__275_)),
                          (None, Some (_, __new_for_cutoff__274_))) ->
                           phys_equal __old_for_cutoff__275_ __new_for_cutoff__274_
                       | ((None, Some (_, __new_for_cutoff__274_)), _)
                         | (_, (None, Some (_, __new_for_cutoff__274_))) -> false
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
             (let __let_syntax__280_ = ((EXPR1)
                [@ppxlib.enter_value __ppx_bonsai_tuple__278_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__281_ = ((EXPR2)
                [@ppxlib.enter_value __ppx_bonsai_tuple__279_])[@@ppxlib.do_not_enter_value
                                                                 ] in
              ((Let_syntax.map2)[@alert "-for_ppx_bonsai"])
                ~here:{
                        Ppx_here_lib.pos_fname = "_none_";
                        pos_lnum = 0;
                        pos_cnum = (-1);
                        pos_bol = 0
                      } __let_syntax__280_ __let_syntax__281_
                ~f:(fun __ppx_bonsai_tuple__278_ __ppx_bonsai_tuple__279_ ->
                      (__ppx_bonsai_tuple__278_, __ppx_bonsai_tuple__279_))))
          ~f:(fun __pattern_syntax__284_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                __pattern_syntax__284_
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
             (let __let_syntax__290_ = ((EXPR1)
                [@ppxlib.enter_value __ppx_bonsai_tuple__285_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__291_ = ((EXPR2)
                [@ppxlib.enter_value __ppx_bonsai_tuple__286_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__292_ = ((EXPR3)
                [@ppxlib.enter_value __ppx_bonsai_tuple__287_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__293_ = ((EXPR4)
                [@ppxlib.enter_value __ppx_bonsai_tuple__288_])[@@ppxlib.do_not_enter_value
                                                                 ]
              and __let_syntax__294_ = ((EXPR5)
                [@ppxlib.enter_value __ppx_bonsai_tuple__289_])[@@ppxlib.do_not_enter_value
                                                                 ] in
              ((Let_syntax.map5)[@alert "-for_ppx_bonsai"])
                ~here:{
                        Ppx_here_lib.pos_fname = "_none_";
                        pos_lnum = 0;
                        pos_cnum = (-1);
                        pos_bol = 0
                      } __let_syntax__290_ __let_syntax__291_ __let_syntax__292_
                __let_syntax__293_ __let_syntax__294_
                ~f:(fun __ppx_bonsai_tuple__285_ __ppx_bonsai_tuple__286_
                      __ppx_bonsai_tuple__287_ __ppx_bonsai_tuple__288_
                      __ppx_bonsai_tuple__289_ ->
                      (__ppx_bonsai_tuple__285_, __ppx_bonsai_tuple__286_,
                        __ppx_bonsai_tuple__287_, __ppx_bonsai_tuple__288_,
                        __ppx_bonsai_tuple__289_))))
          ~f:(fun __pattern_syntax__300_ ->
                ((Let_syntax.sub
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    (Let_syntax.return
                       ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                           ~here:{
                                   Ppx_here_lib.pos_fname = "_none_";
                                   pos_lnum = 0;
                                   pos_cnum = (-1);
                                   pos_bol = 0
                                 } __pattern_syntax__300_
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
          ~f:(fun __pattern_syntax__301_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                __pattern_syntax__301_
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
                                       ((((Let_syntax.map)
                                           [@alert "-for_ppx_bonsai"])
                                           ~here:{
                                                   Ppx_here_lib.pos_fname =
                                                     "_none_";
                                                   pos_lnum = 0;
                                                   pos_cnum = (-1);
                                                   pos_bol = 0
                                                 } __pattern_syntax__301_
                                           ~f:((function
                                                | Some (_, __pattern_syntax__302_)
                                                    -> __pattern_syntax__302_
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
          ~f:(fun __pattern_syntax__303_ ->
                ((Let_syntax.sub
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    (Let_syntax.return
                       ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                           ~here:{
                                   Ppx_here_lib.pos_fname = "_none_";
                                   pos_lnum = 0;
                                   pos_cnum = (-1);
                                   pos_bol = 0
                                 } __pattern_syntax__303_ ~f:(function | _ -> ()))
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
          ~f:(fun __pattern_syntax__304_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                __pattern_syntax__304_
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
          ~f:(fun __pattern_syntax__306_ ->
                ((Let_syntax.sub
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    (Let_syntax.return
                       ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                           ~here:{
                                   Ppx_here_lib.pos_fname = "_none_";
                                   pos_lnum = 0;
                                   pos_cnum = (-1);
                                   pos_bol = 0
                                 } __pattern_syntax__306_ ~f:(function | _ -> ()))
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
          ~f:(fun __pattern_syntax__307_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                __pattern_syntax__307_
                                ~f:(function
                                    | x when condition1 && (x > 1) -> 0
                                    | y when condition2 -> 1
                                    | z -> 2))[@ocaml.warning "-26-27"])
                    ~branches:3
                    ~with_:(function
                            | ((0)[@merlin.hide ]) ->
                                let x = __pattern_syntax__307_ in
                                ((let _ : _ = x in BODY)[@merlin.focus ])
                            | ((1)[@merlin.hide ]) ->
                                let y = __pattern_syntax__307_ in ((BODY)
                                  [@merlin.focus ])
                            | ((2)[@merlin.hide ]) ->
                                let z = __pattern_syntax__307_ in ((BODY)
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
          ~f:(fun __pattern_syntax__308_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 0;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                                __pattern_syntax__308_
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
                                       ((((Let_syntax.map)
                                           [@alert "-for_ppx_bonsai"])
                                           ~here:{
                                                   Ppx_here_lib.pos_fname =
                                                     "_none_";
                                                   pos_lnum = 0;
                                                   pos_cnum = (-1);
                                                   pos_bol = 0
                                                 } __pattern_syntax__308_
                                           ~f:((function
                                                | (_, __pattern_syntax__310_) ->
                                                    __pattern_syntax__310_
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
                                                 ((((Let_syntax.map)
                                                     [@alert "-for_ppx_bonsai"])
                                                     ~here:{
                                                             Ppx_here_lib.pos_fname
                                                               = "_none_";
                                                             pos_lnum = 0;
                                                             pos_cnum = (-1);
                                                             pos_bol = 0
                                                           } __pattern_syntax__308_
                                                     ~f:((function
                                                          | (__pattern_syntax__309_,
                                                             _) ->
                                                              __pattern_syntax__309_
                                                          | _ -> assert false)
                                                     [@ocaml.warning "-11"]))
                                                 [@merlin.hide ]))
                                              ~f:(fun x1 ->
                                                    let _ : _ = x1 in ((BODY)
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
      let __let_syntax__312_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__313_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__312_ __let_syntax__313_ ~f:(fun x1 x2 -> MY_BODY)
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
      let __let_syntax__316_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__317_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__318_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__319_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__320_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__321_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ] in
      Let_syntax.arr6
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__316_ __let_syntax__317_ __let_syntax__318_
        __let_syntax__319_ __let_syntax__320_ __let_syntax__321_
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
      let __let_syntax__328_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__329_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__330_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__331_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__332_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__333_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__334_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ] in
      Let_syntax.arr7
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__328_ __let_syntax__329_ __let_syntax__330_
        __let_syntax__331_ __let_syntax__332_ __let_syntax__333_ __let_syntax__334_
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
                                                               ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__342_ __let_syntax__343_ __let_syntax__344_
           __let_syntax__345_ __let_syntax__346_ __let_syntax__347_
           __let_syntax__348_
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        __let_syntax__349_ ~f:(fun (x1, x2, x3, x4, x5, x6, x7) x8 -> MY_BODY)
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
      let __let_syntax__359_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__360_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__361_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__362_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__363_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__364_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__365_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__366_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__367_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__368_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__369_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__370_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__371_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__372_ = ((E14)[@ppxlib.enter_value x14])[@@ppxlib.do_not_enter_value
                                                                 ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__359_ __let_syntax__360_ __let_syntax__361_
           __let_syntax__362_ __let_syntax__363_ __let_syntax__364_
           __let_syntax__365_
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__366_ __let_syntax__367_ __let_syntax__368_
           __let_syntax__369_ __let_syntax__370_ __let_syntax__371_
           __let_syntax__372_
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
      let __let_syntax__389_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__390_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__391_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__392_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__393_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__394_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__395_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__396_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__397_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__398_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__399_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__400_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__401_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                 ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__389_ __let_syntax__390_ __let_syntax__391_
           __let_syntax__392_ __let_syntax__393_ __let_syntax__394_
           __let_syntax__395_
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        (((Let_syntax.map6)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } __let_syntax__396_ __let_syntax__397_ __let_syntax__398_
           __let_syntax__399_ __let_syntax__400_ __let_syntax__401_
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
      let __let_syntax__417_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__418_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__419_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__420_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__421_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__422_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__423_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__424_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__425_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__426_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__427_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__428_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__429_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__430_ = ((E14)[@ppxlib.enter_value x14])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__431_ = ((E15)[@ppxlib.enter_value x15])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__432_ = ((E16)[@ppxlib.enter_value x16])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__433_ = ((E17)[@ppxlib.enter_value x17])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__434_ = ((E18)[@ppxlib.enter_value x18])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__435_ = ((E19)[@ppxlib.enter_value x19])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__436_ = ((E20)[@ppxlib.enter_value x20])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__437_ = ((E21)[@ppxlib.enter_value x21])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__438_ = ((E22)[@ppxlib.enter_value x22])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__439_ = ((E23)[@ppxlib.enter_value x23])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__440_ = ((E24)[@ppxlib.enter_value x24])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__441_ = ((E25)[@ppxlib.enter_value x25])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__442_ = ((E26)[@ppxlib.enter_value x26])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__443_ = ((E27)[@ppxlib.enter_value x27])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__444_ = ((E28)[@ppxlib.enter_value x28])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__445_ = ((E29)[@ppxlib.enter_value x29])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__446_ = ((E30)[@ppxlib.enter_value x30])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__447_ = ((E31)[@ppxlib.enter_value x31])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__448_ = ((E32)[@ppxlib.enter_value x32])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__449_ = ((E33)[@ppxlib.enter_value x33])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__450_ = ((E34)[@ppxlib.enter_value x34])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__451_ = ((E35)[@ppxlib.enter_value x35])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__452_ = ((E36)[@ppxlib.enter_value x36])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__453_ = ((E37)[@ppxlib.enter_value x37])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__454_ = ((E38)[@ppxlib.enter_value x38])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__455_ = ((E39)[@ppxlib.enter_value x39])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__456_ = ((E40)[@ppxlib.enter_value x40])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__457_ = ((E41)[@ppxlib.enter_value x41])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__458_ = ((E42)[@ppxlib.enter_value x42])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__459_ = ((E43)[@ppxlib.enter_value x43])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__460_ = ((E44)[@ppxlib.enter_value x44])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__461_ = ((E45)[@ppxlib.enter_value x45])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__462_ = ((E46)[@ppxlib.enter_value x46])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__463_ = ((E47)[@ppxlib.enter_value x47])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__464_ = ((E48)[@ppxlib.enter_value x48])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__465_ = ((E49)[@ppxlib.enter_value x49])[@@ppxlib.do_not_enter_value
                                                                 ]
      and __let_syntax__466_ = ((E50)[@ppxlib.enter_value x50])[@@ppxlib.do_not_enter_value
                                                                 ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 }
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__417_ __let_syntax__418_ __let_syntax__419_
              __let_syntax__420_ __let_syntax__421_ __let_syntax__422_
              __let_syntax__423_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__424_ __let_syntax__425_ __let_syntax__426_
              __let_syntax__427_ __let_syntax__428_ __let_syntax__429_
              __let_syntax__430_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__431_ __let_syntax__432_ __let_syntax__433_
              __let_syntax__434_ __let_syntax__435_ __let_syntax__436_
              __let_syntax__437_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__438_ __let_syntax__439_ __let_syntax__440_
              __let_syntax__441_ __let_syntax__442_ __let_syntax__443_
              __let_syntax__444_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__445_ __let_syntax__446_ __let_syntax__447_
              __let_syntax__448_ __let_syntax__449_ __let_syntax__450_
              __let_syntax__451_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__452_ __let_syntax__453_ __let_syntax__454_
              __let_syntax__455_ __let_syntax__456_ __let_syntax__457_
              __let_syntax__458_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           (((Let_syntax.map7)[@alert "-for_ppx_bonsai"])
              ~here:{
                      Ppx_here_lib.pos_fname = "_none_";
                      pos_lnum = 0;
                      pos_cnum = (-1);
                      pos_bol = 0
                    } __let_syntax__459_ __let_syntax__460_ __let_syntax__461_
              __let_syntax__462_ __let_syntax__463_ __let_syntax__464_
              __let_syntax__465_
              ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
           ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
        __let_syntax__466_
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

module%test [@name "arr_debug expansion"] _ = struct
  let expand_arr_debug expr =
    Ppx_let_expander.expand
      (Ppx_bonsai_expander.arr_debug Location_of_callsite)
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      expr
    |> print_expr
  ;;

  let%expect_test "single let%arr_debug, unpunned" =
    expand_arr_debug
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
              }
        (Let_syntax.debug_node
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } ~name:"a" ~equal:Base.phys_equal
           ~sexp_of:Sexplib0.Sexp_conv.sexp_of_opaque ((E)[@ppxlib.enter_value a]))
        ~f:(fun a -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr_debug on 2 things" =
    expand_arr_debug
      [%expr
        let x1 = E1
        and x2 = E2 in
        MY_BODY];
    [%expect
      {|
      let __let_syntax__526_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                               ]
      and __let_syntax__527_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                               ] in
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.debug_node
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } ~name:"x1" ~equal:Base.phys_equal
           ~sexp_of:Sexplib0.Sexp_conv.sexp_of_opaque __let_syntax__526_)
        (Let_syntax.debug_node
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } ~name:"x2" ~equal:Base.phys_equal
           ~sexp_of:Sexplib0.Sexp_conv.sexp_of_opaque __let_syntax__527_)
        ~f:(fun x1 x2 -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr_debug with ignored fields" =
    expand_arr_debug
      [%expr
        let { a; _ } = E in
        MY_BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.debug_node
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } ~name:"a" ~equal:Base.phys_equal
           ~sexp_of:Sexplib0.Sexp_conv.sexp_of_opaque
           ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
               ~here:{
                       Ppx_here_lib.pos_fname = "_none_";
                       pos_lnum = 0;
                       pos_cnum = (-1);
                       pos_bol = 0
                     } ((E)[@ppxlib.enter_value a])
               ~f:(function
                   | { a = __pattern_syntax__530_;_} -> __pattern_syntax__530_))
           [@merlin.hide ])) ~f:(fun a -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr_debug with multiple ignored fields" =
    expand_arr_debug
      [%expr
        let { a; b; _ } = E in
        MY_BODY];
    [%expect
      {|
      Let_syntax.arr2
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.debug_node
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } ~name:"a" ~equal:Base.phys_equal
           ~sexp_of:Sexplib0.Sexp_conv.sexp_of_opaque
           ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
               ~here:{
                       Ppx_here_lib.pos_fname = "_none_";
                       pos_lnum = 0;
                       pos_cnum = (-1);
                       pos_bol = 0
                     } E
               ~f:(function
                   | { a = __pattern_syntax__532_; b = _;_} ->
                       __pattern_syntax__532_))[@merlin.hide ]))
        (Let_syntax.debug_node
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } ~name:"b" ~equal:Base.phys_equal
           ~sexp_of:Sexplib0.Sexp_conv.sexp_of_opaque
           ((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
               ~here:{
                       Ppx_here_lib.pos_fname = "_none_";
                       pos_lnum = 0;
                       pos_cnum = (-1);
                       pos_bol = 0
                     } E
               ~f:(function
                   | { a = _; b = __pattern_syntax__533_;_} ->
                       __pattern_syntax__533_))[@merlin.hide ]))
        ~f:(fun a b -> MY_BODY)
      |}]
  ;;

  let%expect_test "let%arr_debug with type-annotated binding" =
    expand_arr_debug
      [%expr
        let (a : int) = E in
        MY_BODY];
    [%expect
      {|
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              }
        (Let_syntax.debug_node
           ~here:{
                   Ppx_here_lib.pos_fname = "_none_";
                   pos_lnum = 0;
                   pos_cnum = (-1);
                   pos_bol = 0
                 } ~name:"a" ~equal:Base.phys_equal
           ~sexp_of:Sexplib0.Sexp_conv.sexp_of_opaque ((E)[@ppxlib.enter_value a]))
        ~f:(fun (a : int) -> MY_BODY)
      |}]
  ;;
end
