open Core
open Ppxlib

let locality = Ppx_let_expander.Locality.global
let loc = Location.none
let print_expr expr = Pprintast.string_of_expression expr |> print_string

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
                pos_lnum = 1;
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
                pos_lnum = 1;
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
                pos_lnum = 1;
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
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return MY_EXPR_1)
        ~f:(fun __pattern_syntax__005_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 1;
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
                pos_lnum = 1;
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
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } MY_EXPR
        ~f:(fun __pattern_syntax__007_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 1;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((Let_syntax.map
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 1;
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
                                    pos_lnum = 1;
                                    pos_cnum = (-1);
                                    pos_bol = 0
                                  }
                            (Let_syntax.return
                               ((Let_syntax.map
                                   ~here:{
                                           Ppx_here_lib.pos_fname = "_none_";
                                           pos_lnum = 1;
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
                                              pos_lnum = 1;
                                              pos_cnum = (-1);
                                              pos_bol = 0
                                            }
                                      (Let_syntax.return
                                         ((Let_syntax.map
                                             ~here:{
                                                     Ppx_here_lib.pos_fname =
                                                       "_none_";
                                                     pos_lnum = 1;
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
    -1,74 +1,32
    -|((Let_syntax.sub
    -|    ~here:{
    -|            Ppx_here_lib.pos_fname = "_none_";
    -|            pos_lnum = 1;
    -|            pos_cnum = (-1);
    -|            pos_bol = 0
    -|          } MY_EXPR
    +|((Let_syntax.sub ~here MY_EXPR
          ~f:(fun __pattern_syntax__ID_REPLACED_IN_TEST_ ->
    -|          ((Let_syntax.sub
    -|              ~here:{
    -|                      Ppx_here_lib.pos_fname = "_none_";
    -|                      pos_lnum = 1;
    -|                      pos_cnum = (-1);
    -|                      pos_bol = 0
    -|                    }
    +|          ((Let_syntax.sub ~here
    -|              (Let_syntax.return
    -|                 ((Let_syntax.map
    -|                     ~here:{
    -|                             Ppx_here_lib.pos_fname = "_none_";
    -|                             pos_lnum = 1;
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
    -|                                pos_lnum = 1;
    -|                                pos_cnum = (-1);
    -|                                pos_bol = 0
    -|                              }
    +|                    ((Let_syntax.sub ~here
    -|                        (Let_syntax.return
    -|                           ((Let_syntax.map
    -|                               ~here:{
    -|                                       Ppx_here_lib.pos_fname = "_none_";
    -|                                       pos_lnum = 1;
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
    -|                                          pos_lnum = 1;
    -|                                          pos_cnum = (-1);
    -|                                          pos_bol = 0
    -|                                        }
    +|                              ((Let_syntax.sub ~here
    -|                                  (Let_syntax.return
    -|                                     ((Let_syntax.map
    -|                                         ~here:{
    -|                                                 Ppx_here_lib.pos_fname =
    -|                                                   "_none_";
    -|                                                 pos_lnum = 1;
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
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__023_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 1;
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
                                          pos_lnum = 1;
                                          pos_cnum = (-1);
                                          pos_bol = 0
                                        }
                                  (Let_syntax.return
                                     ((Let_syntax.map
                                         ~here:{
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 1;
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
                                                    pos_lnum = 1;
                                                    pos_cnum = (-1);
                                                    pos_bol = 0
                                                  }
                                            (Let_syntax.return
                                               ((Let_syntax.map
                                                   ~here:{
                                                           Ppx_here_lib.pos_fname
                                                             = "_none_";
                                                           pos_lnum = 1;
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
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } (Module.Let_syntax.Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__029_ ->
              ((Module.Let_syntax.Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 1;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Module.Let_syntax.Let_syntax.return
                     ((Module.Let_syntax.Let_syntax.map
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 1;
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
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } (Module.Let_syntax.Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__031_ ->
              ((Module.Let_syntax.Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 1;
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
                                          pos_lnum = 1;
                                          pos_cnum = (-1);
                                          pos_bol = 0
                                        }
                                  (Module.Let_syntax.Let_syntax.return
                                     ((Module.Let_syntax.Let_syntax.map
                                         ~here:{
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 1;
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
                                          pos_lnum = 1;
                                          pos_cnum = (-1);
                                          pos_bol = 0
                                        }
                                  (Module.Let_syntax.Let_syntax.return
                                     ((Module.Let_syntax.Let_syntax.map
                                         ~here:{
                                                 Ppx_here_lib.pos_fname =
                                                   "_none_";
                                                 pos_lnum = 1;
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
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } EXPR
        ~f:(fun __pattern_syntax__034_ ->
              ((Let_syntax.sub
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 1;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  (Let_syntax.return
                     ((Let_syntax.map
                         ~here:{
                                 Ppx_here_lib.pos_fname = "_none_";
                                 pos_lnum = 1;
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
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return __let_syntax__035_)
          ~f:(fun __pattern_syntax__036_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 1;
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
                                            pos_lnum = 1;
                                            pos_cnum = (-1);
                                            pos_bol = 0
                                          }
                                    (Let_syntax.return
                                       ((Let_syntax.map
                                           ~here:{
                                                   Ppx_here_lib.pos_fname =
                                                     "_none_";
                                                   pos_lnum = 1;
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
                pos_lnum = 1;
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
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff MY_EXPR
         ~equal:(fun
                   (__old_for_cutoff__044_,
                    { b = __old_for_cutoff__042_; c = __old_for_cutoff__040_;_})
                   (__new_for_cutoff__043_,
                    { b = __new_for_cutoff__041_; c = __new_for_cutoff__039_;_})
                   ->
                   (phys_equal __old_for_cutoff__044_ __new_for_cutoff__043_) &&
                     ((phys_equal __old_for_cutoff__042_ __new_for_cutoff__041_)
                        &&
                        (phys_equal __old_for_cutoff__040_ __new_for_cutoff__039_))))
      ~f:(fun (a, { b; c;_}) -> MY_BODY)
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
                   (__old_for_cutoff__051_,
                    { b = __old_for_cutoff__049_; c = __old_for_cutoff__047_;_})
                   (__new_for_cutoff__050_,
                    { b = __new_for_cutoff__048_; c = __new_for_cutoff__046_;_})
                   ->
                   (phys_equal __old_for_cutoff__051_ __new_for_cutoff__050_) &&
                     ((phys_equal __old_for_cutoff__049_ __new_for_cutoff__048_)
                        &&
                        (phys_equal __old_for_cutoff__047_ __new_for_cutoff__046_))))
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
    let __let_syntax__053_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__054_ = OTHER_EXPR[@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff __let_syntax__053_
         ~equal:(fun
                   (__old_for_cutoff__060_,
                    { b = __old_for_cutoff__058_; c = __old_for_cutoff__056_;_})
                   (__new_for_cutoff__059_,
                    { b = __new_for_cutoff__057_; c = __new_for_cutoff__055_;_})
                   ->
                   (phys_equal __old_for_cutoff__060_ __new_for_cutoff__059_) &&
                     ((phys_equal __old_for_cutoff__058_ __new_for_cutoff__057_)
                        &&
                        (phys_equal __old_for_cutoff__056_ __new_for_cutoff__055_))))
      (Let_syntax.cutoff __let_syntax__054_
         ~equal:(fun
                   (__old_for_cutoff__066_,
                    { y = __old_for_cutoff__064_; z = __old_for_cutoff__062_;_})
                   (__new_for_cutoff__065_,
                    { y = __new_for_cutoff__063_; z = __new_for_cutoff__061_;_})
                   ->
                   (phys_equal __old_for_cutoff__066_ __new_for_cutoff__065_) &&
                     ((phys_equal __old_for_cutoff__064_ __new_for_cutoff__063_)
                        &&
                        (phys_equal __old_for_cutoff__062_ __new_for_cutoff__061_))))
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
    let __let_syntax__069_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__070_ = ((OTHER_EXPR)[@ppxlib.enter_value y])[@@ppxlib.do_not_enter_value
                                                                    ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.cutoff __let_syntax__069_
         ~equal:(fun
                   (__old_for_cutoff__076_,
                    { b = __old_for_cutoff__074_; c = __old_for_cutoff__072_;_})
                   (__new_for_cutoff__075_,
                    { b = __new_for_cutoff__073_; c = __new_for_cutoff__071_;_})
                   ->
                   (phys_equal __old_for_cutoff__076_ __new_for_cutoff__075_) &&
                     ((phys_equal __old_for_cutoff__074_ __new_for_cutoff__073_)
                        &&
                        (phys_equal __old_for_cutoff__072_ __new_for_cutoff__071_))))
      __let_syntax__070_ ~f:(fun (a, { b; c;_}) y -> MY_BODY)
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
              pos_lnum = 1;
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
    let __let_syntax__080_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
    and __let_syntax__081_ = OTHER_EXPR[@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr2
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            } __let_syntax__080_ __let_syntax__081_
      ~f:(fun (a, { b; c; d }) (x, { y; z; w }) -> MY_BODY)
    |}]
;;

let%test_module "Destructuring vs. no destructuring criteria." =
  (module struct
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
      (* This is the most common case and since there is no need for destruction here;
         this one is not destructed. *)
      run_test
        [%expr
          let a = NO_DESTRUCTION in
          BODY];
      [%expect
        {|
        Let_syntax.arr
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } ((NO_DESTRUCTION)[@ppxlib.enter_value a]) ~f:(fun a -> BODY)
        |}]
    ;;

    let%expect_test "Single ppat_var does not get destructed (multiple arms)" =
      (* This is the most common case and since there is no need for destruction here;
         this one is not destructed. *)
      run_test
        [%expr
          let a = NO_DESTRUCTION
          and b = ALSO_NO_DESTRUCTION in
          BODY];
      [%expect
        {|
        let __let_syntax__085_ = ((NO_DESTRUCTION)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                            ]
        and __let_syntax__086_ = ((ALSO_NO_DESTRUCTION)[@ppxlib.enter_value b])
        [@@ppxlib.do_not_enter_value ] in
        Let_syntax.arr2
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } __let_syntax__085_ __let_syntax__086_ ~f:(fun a b -> BODY)
        |}]
    ;;

    let%expect_test "alias might result in no destruction (multiple arms)" =
      (* This test case shows that despite there being an underscored ignored inside of
         the left handside pattern, the destruction does not occur since there is an alias. *)
      run_test
        [%expr
          let (_ as a) = NO_DESTRUCTION
          and (_ as b) = ALSO_NO_DESTRUCTION in
          BODY];
      [%expect
        {|
        let __let_syntax__089_ = ((NO_DESTRUCTION)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                            ]
        and __let_syntax__090_ = ((ALSO_NO_DESTRUCTION)[@ppxlib.enter_value b])
        [@@ppxlib.do_not_enter_value ] in
        Let_syntax.arr2
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } __let_syntax__089_ __let_syntax__090_
          ~f:(fun (_ as a) (_ as b) -> BODY)
        |}]
    ;;

    let%expect_test "alias might result in destruction." =
      (* This test case showcases the true behavior of how aliases are treated. They
         block any "ignores" inside of them, but remain ineffective is something
         is ignored outside of its contents/reach. *)
      run_test
        [%expr
          let (_ as a), _ = DESTRUCT in
          BODY];
      [%expect
        {|
        Let_syntax.arr
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.cutoff ((DESTRUCT)[@ppxlib.enter_value a])
             ~equal:(fun ((_ as __old_for_cutoff__094_), _)
                       ((_ as __new_for_cutoff__093_), _) ->
                       phys_equal __old_for_cutoff__094_ __new_for_cutoff__093_))
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
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.cutoff BASE
             ~equal:(fun
                       { foo = __old_for_cutoff__099_;
                         bar = { baz = __old_for_cutoff__097_;_};_}
                       { foo = __new_for_cutoff__098_;
                         bar = { baz = __new_for_cutoff__096_;_};_}
                       ->
                       (phys_equal __old_for_cutoff__099_ __new_for_cutoff__098_) &&
                         (phys_equal __old_for_cutoff__097_ __new_for_cutoff__096_)))
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
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.cutoff DESTRUCT
             ~equal:(fun (((__old_for_cutoff__104_, _) : t), __old_for_cutoff__102_)
                       (((__new_for_cutoff__103_, _) : t), __new_for_cutoff__101_) ->
                       (phys_equal __old_for_cutoff__104_ __new_for_cutoff__103_) &&
                         (phys_equal __old_for_cutoff__102_ __new_for_cutoff__101_)))
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
        let __let_syntax__106_ = DESTRUCT[@@ppxlib.do_not_enter_value ]
        and __let_syntax__107_ = ALSO_DESTRUCT[@@ppxlib.do_not_enter_value ] in
        Let_syntax.arr2
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.cutoff __let_syntax__106_
             ~equal:(fun (((__old_for_cutoff__111_, _) : t), __old_for_cutoff__109_)
                       (((__new_for_cutoff__110_, _) : t), __new_for_cutoff__108_) ->
                       (phys_equal __old_for_cutoff__111_ __new_for_cutoff__110_) &&
                         (phys_equal __old_for_cutoff__109_ __new_for_cutoff__108_)))
          (Let_syntax.cutoff __let_syntax__107_
             ~equal:(fun (((__old_for_cutoff__115_, _) : t), __old_for_cutoff__113_)
                       (((__new_for_cutoff__114_, _) : t), __new_for_cutoff__112_) ->
                       (phys_equal __old_for_cutoff__115_ __new_for_cutoff__114_) &&
                         (phys_equal __old_for_cutoff__113_ __new_for_cutoff__112_)))
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
                  pos_lnum = 1;
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
        let __let_syntax__119_ = NO_DESTRUCTION[@@ppxlib.do_not_enter_value ]
        and __let_syntax__120_ = ALSO_NO_DESTRUCTION[@@ppxlib.do_not_enter_value ] in
        Let_syntax.arr2
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } __let_syntax__119_ __let_syntax__120_
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
                  pos_lnum = 1;
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
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.cutoff DESTRUCT
             ~equal:(fun
                       (({ a = __old_for_cutoff__127_; b = (module _)  } : t),
                        __old_for_cutoff__125_)
                       (({ a = __new_for_cutoff__126_; b = (module _)  } : t),
                        __new_for_cutoff__124_)
                       ->
                       (phys_equal __old_for_cutoff__127_ __new_for_cutoff__126_) &&
                         (phys_equal __old_for_cutoff__125_ __new_for_cutoff__124_)))
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
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.cutoff ALWAYS_EQUAL ~equal:(fun _ _ -> true))
          ~f:(fun (({ a = _; b = (module _)  } : t), _) -> BODY)
        |}]
    ;;

    let%expect_test "Duplicate variables in pattern" =
      (* This test showcases current behavior. Since OCaml does not allow
         multiple "parallel" bindings on the same level, there is an error
         shown to the user by the OCaml compiler and not the ppx.
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
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.cutoff ((EXPR)[@ppxlib.enter_value a])
             ~equal:(fun { a = __old_for_cutoff__133_; b = __old_for_cutoff__133_;_}
                       { a = __new_for_cutoff__132_; b = __new_for_cutoff__132_;_} ->
                       (phys_equal __old_for_cutoff__133_ __new_for_cutoff__132_) &&
                         (phys_equal __old_for_cutoff__131_ __new_for_cutoff__130_)))
          ~f:(fun { a; b = a;_} -> BODY)
        |}]
    ;;
  end)
;;

let%expect_test "current match%arr behavior" =
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
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            } EXPR
      ~f:(function | A (a, _) -> MY_BODY | B -> MY_BODY | _ -> MY_BODY)
    |}]
;;

let%test_module "match%sub with tuple payload" =
  (module struct
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
                    pos_lnum = 1;
                    pos_cnum = (-1);
                    pos_bol = 0
                  }
            (Let_syntax.return
               (let __let_syntax__137_ = ((EXPR1)
                  [@ppxlib.enter_value __ppx_bonsai_tuple__135_])[@@ppxlib.do_not_enter_value
                                                                   ]
                and __let_syntax__138_ = ((EXPR2)
                  [@ppxlib.enter_value __ppx_bonsai_tuple__136_])[@@ppxlib.do_not_enter_value
                                                                   ] in
                Let_syntax.map
                  (Let_syntax.both __let_syntax__137_ __let_syntax__138_)
                  ~f:(fun (__ppx_bonsai_tuple__135_, __ppx_bonsai_tuple__136_) ->
                        (__ppx_bonsai_tuple__135_, __ppx_bonsai_tuple__136_))))
            ~f:(fun __pattern_syntax__140_ ->
                  ((Let_syntax.switch
                      ~here:{
                              Ppx_here_lib.pos_fname = "_none_";
                              pos_lnum = 1;
                              pos_cnum = (-1);
                              pos_bol = 0
                            }
                      ~match_:((Let_syntax.map __pattern_syntax__140_
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
                    pos_lnum = 1;
                    pos_cnum = (-1);
                    pos_bol = 0
                  }
            (Let_syntax.return
               (let __let_syntax__146_ = ((EXPR1)
                  [@ppxlib.enter_value __ppx_bonsai_tuple__141_])[@@ppxlib.do_not_enter_value
                                                                   ]
                and __let_syntax__147_ = ((EXPR2)
                  [@ppxlib.enter_value __ppx_bonsai_tuple__142_])[@@ppxlib.do_not_enter_value
                                                                   ]
                and __let_syntax__148_ = ((EXPR3)
                  [@ppxlib.enter_value __ppx_bonsai_tuple__143_])[@@ppxlib.do_not_enter_value
                                                                   ]
                and __let_syntax__149_ = ((EXPR4)
                  [@ppxlib.enter_value __ppx_bonsai_tuple__144_])[@@ppxlib.do_not_enter_value
                                                                   ]
                and __let_syntax__150_ = ((EXPR5)
                  [@ppxlib.enter_value __ppx_bonsai_tuple__145_])[@@ppxlib.do_not_enter_value
                                                                   ] in
                Let_syntax.map
                  (Let_syntax.both __let_syntax__146_
                     (Let_syntax.both __let_syntax__147_
                        (Let_syntax.both __let_syntax__148_
                           (Let_syntax.both __let_syntax__149_ __let_syntax__150_))))
                  ~f:(fun
                        (__ppx_bonsai_tuple__141_,
                         (__ppx_bonsai_tuple__142_,
                          (__ppx_bonsai_tuple__143_,
                           (__ppx_bonsai_tuple__144_, __ppx_bonsai_tuple__145_))))
                        ->
                        (__ppx_bonsai_tuple__141_, __ppx_bonsai_tuple__142_,
                          __ppx_bonsai_tuple__143_, __ppx_bonsai_tuple__144_,
                          __ppx_bonsai_tuple__145_))))
            ~f:(fun __pattern_syntax__152_ ->
                  ((Let_syntax.sub
                      ~here:{
                              Ppx_here_lib.pos_fname = "_none_";
                              pos_lnum = 1;
                              pos_cnum = (-1);
                              pos_bol = 0
                            }
                      (Let_syntax.return
                         ((Let_syntax.map
                             ~here:{
                                     Ppx_here_lib.pos_fname = "_none_";
                                     pos_lnum = 1;
                                     pos_cnum = (-1);
                                     pos_bol = 0
                                   } __pattern_syntax__152_
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
                    pos_lnum = 1;
                    pos_cnum = (-1);
                    pos_bol = 0
                  } (Let_syntax.return EXPR)
            ~f:(fun __pattern_syntax__153_ ->
                  ((Let_syntax.switch
                      ~here:{
                              Ppx_here_lib.pos_fname = "_none_";
                              pos_lnum = 1;
                              pos_cnum = (-1);
                              pos_bol = 0
                            }
                      ~match_:((Let_syntax.map __pattern_syntax__153_
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
                                              pos_lnum = 1;
                                              pos_cnum = (-1);
                                              pos_bol = 0
                                            }
                                      (Let_syntax.return
                                         ((Let_syntax.map
                                             ~here:{
                                                     Ppx_here_lib.pos_fname =
                                                       "_none_";
                                                     pos_lnum = 1;
                                                     pos_cnum = (-1);
                                                     pos_bol = 0
                                                   } __pattern_syntax__153_
                                             ~f:((function
                                                  | Some (_, __pattern_syntax__154_)
                                                      -> __pattern_syntax__154_
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
  end)
;;

let%test_module "arrn nesting edge cases" =
  (module struct
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
                  pos_lnum = 1;
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
        let __let_syntax__156_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__157_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                                 ] in
        Let_syntax.arr2
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } __let_syntax__156_ __let_syntax__157_ ~f:(fun x1 x2 -> MY_BODY)
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
        let __let_syntax__160_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__161_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__162_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__163_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__164_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__165_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                                 ] in
        Let_syntax.arr6
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } __let_syntax__160_ __let_syntax__161_ __let_syntax__162_
          __let_syntax__163_ __let_syntax__164_ __let_syntax__165_
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
        let __let_syntax__172_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__173_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__174_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__175_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__176_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__177_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__178_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                                 ] in
        Let_syntax.arr7
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } __let_syntax__172_ __let_syntax__173_ __let_syntax__174_
          __let_syntax__175_ __let_syntax__176_ __let_syntax__177_ __let_syntax__178_
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
        let __let_syntax__186_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__187_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__188_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__189_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__190_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__191_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__192_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__193_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                                 ] in
        Let_syntax.arr2
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__186_ __let_syntax__187_ __let_syntax__188_
             __let_syntax__189_ __let_syntax__190_ __let_syntax__191_
             __let_syntax__192_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          __let_syntax__193_ ~f:(fun (x1, x2, x3, x4, x5, x6, x7) x8 -> MY_BODY)
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
        let __let_syntax__203_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__204_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__205_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__206_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__207_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__208_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__209_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__210_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__211_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__212_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__213_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__214_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__215_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__216_ = ((E14)[@ppxlib.enter_value x14])[@@ppxlib.do_not_enter_value
                                                                   ] in
        Let_syntax.arr2
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__203_ __let_syntax__204_ __let_syntax__205_
             __let_syntax__206_ __let_syntax__207_ __let_syntax__208_
             __let_syntax__209_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__210_ __let_syntax__211_ __let_syntax__212_
             __let_syntax__213_ __let_syntax__214_ __let_syntax__215_
             __let_syntax__216_
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
        let __let_syntax__233_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__234_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__235_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__236_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__237_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__238_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__239_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__240_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__241_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__242_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__243_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__244_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__245_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                   ] in
        Let_syntax.arr2
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__233_ __let_syntax__234_ __let_syntax__235_
             __let_syntax__236_ __let_syntax__237_ __let_syntax__238_
             __let_syntax__239_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          (Let_syntax.map6
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__240_ __let_syntax__241_ __let_syntax__242_
             __let_syntax__243_ __let_syntax__244_ __let_syntax__245_
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
        let __let_syntax__261_ = ((E1)[@ppxlib.enter_value x1])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__262_ = ((E2)[@ppxlib.enter_value x2])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__263_ = ((E3)[@ppxlib.enter_value x3])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__264_ = ((E4)[@ppxlib.enter_value x4])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__265_ = ((E5)[@ppxlib.enter_value x5])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__266_ = ((E6)[@ppxlib.enter_value x6])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__267_ = ((E7)[@ppxlib.enter_value x7])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__268_ = ((E8)[@ppxlib.enter_value x8])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__269_ = ((E9)[@ppxlib.enter_value x9])[@@ppxlib.do_not_enter_value
                                                                 ]
        and __let_syntax__270_ = ((E10)[@ppxlib.enter_value x10])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__271_ = ((E11)[@ppxlib.enter_value x11])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__272_ = ((E12)[@ppxlib.enter_value x12])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__273_ = ((E13)[@ppxlib.enter_value x13])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__274_ = ((E14)[@ppxlib.enter_value x14])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__275_ = ((E15)[@ppxlib.enter_value x15])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__276_ = ((E16)[@ppxlib.enter_value x16])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__277_ = ((E17)[@ppxlib.enter_value x17])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__278_ = ((E18)[@ppxlib.enter_value x18])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__279_ = ((E19)[@ppxlib.enter_value x19])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__280_ = ((E20)[@ppxlib.enter_value x20])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__281_ = ((E21)[@ppxlib.enter_value x21])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__282_ = ((E22)[@ppxlib.enter_value x22])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__283_ = ((E23)[@ppxlib.enter_value x23])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__284_ = ((E24)[@ppxlib.enter_value x24])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__285_ = ((E25)[@ppxlib.enter_value x25])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__286_ = ((E26)[@ppxlib.enter_value x26])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__287_ = ((E27)[@ppxlib.enter_value x27])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__288_ = ((E28)[@ppxlib.enter_value x28])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__289_ = ((E29)[@ppxlib.enter_value x29])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__290_ = ((E30)[@ppxlib.enter_value x30])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__291_ = ((E31)[@ppxlib.enter_value x31])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__292_ = ((E32)[@ppxlib.enter_value x32])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__293_ = ((E33)[@ppxlib.enter_value x33])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__294_ = ((E34)[@ppxlib.enter_value x34])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__295_ = ((E35)[@ppxlib.enter_value x35])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__296_ = ((E36)[@ppxlib.enter_value x36])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__297_ = ((E37)[@ppxlib.enter_value x37])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__298_ = ((E38)[@ppxlib.enter_value x38])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__299_ = ((E39)[@ppxlib.enter_value x39])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__300_ = ((E40)[@ppxlib.enter_value x40])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__301_ = ((E41)[@ppxlib.enter_value x41])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__302_ = ((E42)[@ppxlib.enter_value x42])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__303_ = ((E43)[@ppxlib.enter_value x43])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__304_ = ((E44)[@ppxlib.enter_value x44])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__305_ = ((E45)[@ppxlib.enter_value x45])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__306_ = ((E46)[@ppxlib.enter_value x46])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__307_ = ((E47)[@ppxlib.enter_value x47])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__308_ = ((E48)[@ppxlib.enter_value x48])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__309_ = ((E49)[@ppxlib.enter_value x49])[@@ppxlib.do_not_enter_value
                                                                   ]
        and __let_syntax__310_ = ((E50)[@ppxlib.enter_value x50])[@@ppxlib.do_not_enter_value
                                                                   ] in
        Let_syntax.arr8
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                }
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__261_ __let_syntax__262_ __let_syntax__263_
             __let_syntax__264_ __let_syntax__265_ __let_syntax__266_
             __let_syntax__267_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__268_ __let_syntax__269_ __let_syntax__270_
             __let_syntax__271_ __let_syntax__272_ __let_syntax__273_
             __let_syntax__274_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__275_ __let_syntax__276_ __let_syntax__277_
             __let_syntax__278_ __let_syntax__279_ __let_syntax__280_
             __let_syntax__281_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__282_ __let_syntax__283_ __let_syntax__284_
             __let_syntax__285_ __let_syntax__286_ __let_syntax__287_
             __let_syntax__288_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__289_ __let_syntax__290_ __let_syntax__291_
             __let_syntax__292_ __let_syntax__293_ __let_syntax__294_
             __let_syntax__295_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__296_ __let_syntax__297_ __let_syntax__298_
             __let_syntax__299_ __let_syntax__300_ __let_syntax__301_
             __let_syntax__302_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          (Let_syntax.map7
             ~here:{
                     Ppx_here_lib.pos_fname = "_none_";
                     pos_lnum = 1;
                     pos_cnum = (-1);
                     pos_bol = 0
                   } __let_syntax__303_ __let_syntax__304_ __let_syntax__305_
             __let_syntax__306_ __let_syntax__307_ __let_syntax__308_
             __let_syntax__309_
             ~f:(fun t0 t1 t2 t3 t4 t5 t6 -> (t0, t1, t2, t3, t4, t5, t6)))
          __let_syntax__310_
          ~f:(fun (x1, x2, x3, x4, x5, x6, x7) (x8, x9, x10, x11, x12, x13, x14)
                (x15, x16, x17, x18, x19, x20, x21)
                (x22, x23, x24, x25, x26, x27, x28)
                (x29, x30, x31, x32, x33, x34, x35)
                (x36, x37, x38, x39, x40, x41, x42)
                (x43, x44, x45, x46, x47, x48, x49) x50 -> MY_BODY)
        |}]
    ;;
  end)
;;
