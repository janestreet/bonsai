open Core
open Ppxlib

let locality = `global
let loc = Location.none
let print_expr expr = Pprintast.string_of_expression expr |> print_string

let%expect_test "single let%sub " =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.sub
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
    [@nontail ]) |}]
;;

let%expect_test "single pattern sub with modul" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.sub
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
    [@nontail ]) |}]
;;

let assert_fails_with_syntax_error ~f =
  try
    ignore (f ());
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
      Ppx_bonsai_expander.sub
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      ~locality
      [%expr
        let a = MY_EXPR_1
        and b = MY_EXPR_2 in
        MY_BODY]);
  [%expect {|
    let%sub should not be used with 'and'. |}]
;;

let%expect_test "single pattern sub open" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.sub
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
    [@nontail ]) |}]
;;

let%expect_test "double pattern map open" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      Ppx_bonsai_expander.sub
      Ppx_let_expander.Extension_kind.default_open
      ~modul:None
      ~locality
      [%expr
        let a = MY_EXPR_1
        and b = MY_EXPR_2 in
        MY_BODY]);
  [%expect {|
    let%sub should not be used with 'and'. |}]
;;

let%expect_test "while%sub is banned" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      Ppx_bonsai_expander.sub
      Ppx_let_expander.Extension_kind.default_open
      ~modul:None
      ~locality
      [%expr
        while a = MY_EXPR_1 do
          MY_BODY
        done]);
  [%expect {|
    while%sub is not supported |}]
;;

let%expect_test "if%sub is supported" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.sub
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
        ~f:(fun __pattern_syntax__004_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 1;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((Let_syntax.map __pattern_syntax__004_
                              ~f:(function | true -> 0 | false -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | ((0)[@merlin.hide ]) -> BODY_1
                          | ((1)[@merlin.hide ]) -> BODY_2
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ]) |}]
;;

let%expect_test "very simple match%sub" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.sub
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
   [@nontail ]) |}]
;;

let%expect_test "destructuring let%sub" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.sub
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
       ~f:(fun __pattern_syntax__006_ ->
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
                              } __pattern_syntax__006_
                        ~f:(function
                            | (_, { b = _; c = __pattern_syntax__009_ }) ->
                                __pattern_syntax__009_))[@merlin.hide ]))
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
                                        } __pattern_syntax__006_
                                  ~f:(function
                                      | (_,
                                         { b = __pattern_syntax__008_; c = _ })
                                          -> __pattern_syntax__008_))
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
                                                  } __pattern_syntax__006_
                                            ~f:(function
                                                | (__pattern_syntax__007_,
                                                   { b = _; c = _ }) ->
                                                    __pattern_syntax__007_))
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
    Ppx_bonsai_expander.sub
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
       ~f:(fun __pattern_syntax__010_ ->
             ((Let_syntax.switch
                 ~here:{
                         Ppx_here_lib.pos_fname = "_none_";
                         pos_lnum = 1;
                         pos_cnum = (-1);
                         pos_bol = 0
                       }
                 ~match_:((Let_syntax.map __pattern_syntax__010_
                             ~f:(function
                                 | Choice_1 (a, b) -> 0
                                 | Choice_2 _ -> 1
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
                                              } __pattern_syntax__010_
                                        ~f:((function
                                             | Choice_1
                                                 (_, __pattern_syntax__012_) ->
                                                 __pattern_syntax__012_
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
                                                        } __pattern_syntax__010_
                                                  ~f:((function
                                                       | Choice_1
                                                           (__pattern_syntax__011_,
                                                            _)
                                                           ->
                                                           __pattern_syntax__011_
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
    Ppx_bonsai_expander.sub
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
        ~f:(fun __pattern_syntax__013_ ->
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
                               } __pattern_syntax__013_
                         ~f:(function
                             | Choice_1 __pattern_syntax__014_ ->
                                 __pattern_syntax__014_))[@merlin.hide ]))
                  ~f:(fun x -> ((CHOICE_1_BODY)[@nontail ])))
              [@nontail ])))
    [@nontail ]) |}]
;;

let%expect_test "module-qualified match%sub" =
  let modul = Some { txt = lident "Module"; loc } in
  Ppx_let_expander.expand
    Ppx_bonsai_expander.sub
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
       ~f:(fun __pattern_syntax__015_ ->
             ((Module.Let_syntax.Let_syntax.switch
                 ~here:{
                         Ppx_here_lib.pos_fname = "_none_";
                         pos_lnum = 1;
                         pos_cnum = (-1);
                         pos_bol = 0
                       }
                 ~match_:((Module.Let_syntax.Let_syntax.map
                             __pattern_syntax__015_
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
                                              } __pattern_syntax__015_
                                        ~f:((function
                                             | Choice_1 __pattern_syntax__016_
                                                 -> __pattern_syntax__016_
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
                                              } __pattern_syntax__015_
                                        ~f:((function
                                             | Choice_2 __pattern_syntax__017_
                                                 -> __pattern_syntax__017_
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
    Ppx_bonsai_expander.sub
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
        ~f:(fun __pattern_syntax__018_ ->
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
                               } __pattern_syntax__018_
                         ~f:(function | (_ : int) -> ()))[@nontail ]))
                  ~f:(fun _ -> ((BODY)[@nontail ])))
              [@nontail ])))
    [@nontail ]) |}]
;;

let%expect_test "function%sub" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.sub
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
    fun __let_syntax__019_ ->
      ((Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return __let_syntax__019_)
          ~f:(fun __pattern_syntax__020_ ->
                ((Let_syntax.switch
                    ~here:{
                            Ppx_here_lib.pos_fname = "_none_";
                            pos_lnum = 1;
                            pos_cnum = (-1);
                            pos_bol = 0
                          }
                    ~match_:((Let_syntax.map __pattern_syntax__020_
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
                                                 } __pattern_syntax__020_
                                           ~f:((function
                                                | Some __pattern_syntax__021_ ->
                                                    __pattern_syntax__021_
                                                | _ -> assert false)
                                           [@ocaml.warning "-11"]))
                                       [@merlin.hide ]))
                                    ~f:(fun a -> ((EXPR_SOME)[@nontail ])))
                                [@nontail ])
                            | ((1)[@merlin.hide ]) -> EXPR_NONE
                            | _ -> assert false))
                [@nontail ])))
      [@nontail ]) |}]
;;

let%expect_test "function%arr" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.arr
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
    fun __let_syntax__022_ ->
      Let_syntax.arr
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } __let_syntax__022_
        ~f:(function | Some a -> EXPR_SOME | None -> EXPR_NONE) |}]
;;

let%expect_test "destructuring let%arr uses cutoff" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.arr
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
                  (__old_for_cutoff__028_,
                   { b = __old_for_cutoff__026_; c = __old_for_cutoff__024_;_})
                  (__new_for_cutoff__027_,
                   { b = __new_for_cutoff__025_; c = __new_for_cutoff__023_;_})
                  ->
                  (phys_equal __old_for_cutoff__028_ __new_for_cutoff__027_) &&
                    ((phys_equal __old_for_cutoff__026_ __new_for_cutoff__025_)
                       &&
                       (phys_equal __old_for_cutoff__024_ __new_for_cutoff__023_))))
     ~f:(fun (a, { b; c;_}) -> MY_BODY)
   |}]
;;

let%expect_test "destructuring let%arr uses cutoff (multiple arms)" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.arr
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
   let __let_syntax__030_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
   and __let_syntax__031_ = OTHER_EXPR[@@ppxlib.do_not_enter_value ] in
   Let_syntax.arr
     ~here:{
             Ppx_here_lib.pos_fname = "_none_";
             pos_lnum = 1;
             pos_cnum = (-1);
             pos_bol = 0
           }
     (Let_syntax.both
        (Let_syntax.cutoff __let_syntax__030_
           ~equal:(fun
                     (__old_for_cutoff__037_,
                      { b = __old_for_cutoff__035_;
                        c = __old_for_cutoff__033_;_})
                     (__new_for_cutoff__036_,
                      { b = __new_for_cutoff__034_;
                        c = __new_for_cutoff__032_;_})
                     ->
                     (phys_equal __old_for_cutoff__037_ __new_for_cutoff__036_)
                       &&
                       ((phys_equal __old_for_cutoff__035_
                           __new_for_cutoff__034_)
                          &&
                          (phys_equal __old_for_cutoff__033_
                             __new_for_cutoff__032_))))
        (Let_syntax.cutoff __let_syntax__031_
           ~equal:(fun
                     (__old_for_cutoff__043_,
                      { y = __old_for_cutoff__041_;
                        z = __old_for_cutoff__039_;_})
                     (__new_for_cutoff__042_,
                      { y = __new_for_cutoff__040_;
                        z = __new_for_cutoff__038_;_})
                     ->
                     (phys_equal __old_for_cutoff__043_ __new_for_cutoff__042_)
                       &&
                       ((phys_equal __old_for_cutoff__041_
                           __new_for_cutoff__040_)
                          &&
                          (phys_equal __old_for_cutoff__039_
                             __new_for_cutoff__038_)))))
     ~f:(fun ((a, { b; c;_}), (x, { y; z;_})) -> MY_BODY)
   |}]
;;

let%expect_test "one arm of destructuring let%arr uses cutoff" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.arr
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
   let __let_syntax__045_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
   and __let_syntax__046_ = ((OTHER_EXPR)[@ppxlib.enter_value y])[@@ppxlib.do_not_enter_value
                                                                   ] in
   Let_syntax.arr
     ~here:{
             Ppx_here_lib.pos_fname = "_none_";
             pos_lnum = 1;
             pos_cnum = (-1);
             pos_bol = 0
           }
     (Let_syntax.both
        (Let_syntax.cutoff __let_syntax__045_
           ~equal:(fun
                     (__old_for_cutoff__052_,
                      { b = __old_for_cutoff__050_;
                        c = __old_for_cutoff__048_;_})
                     (__new_for_cutoff__051_,
                      { b = __new_for_cutoff__049_;
                        c = __new_for_cutoff__047_;_})
                     ->
                     (phys_equal __old_for_cutoff__052_ __new_for_cutoff__051_)
                       &&
                       ((phys_equal __old_for_cutoff__050_
                           __new_for_cutoff__049_)
                          &&
                          (phys_equal __old_for_cutoff__048_
                             __new_for_cutoff__047_)))) __let_syntax__046_)
     ~f:(fun ((a, { b; c;_}), y) -> MY_BODY)
   |}]
;;

let%expect_test "Destructuring does not happen when there is no ignoring" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.arr
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
    Ppx_bonsai_expander.arr
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
   let __let_syntax__055_ = MY_EXPR[@@ppxlib.do_not_enter_value ]
   and __let_syntax__056_ = OTHER_EXPR[@@ppxlib.do_not_enter_value ] in
   Let_syntax.arr
     ~here:{
             Ppx_here_lib.pos_fname = "_none_";
             pos_lnum = 1;
             pos_cnum = (-1);
             pos_bol = 0
           } (Let_syntax.both __let_syntax__055_ __let_syntax__056_)
     ~f:(fun ((a, { b; c; d }), (x, { y; z; w })) -> MY_BODY)
   |}]
;;

let%test_module "Destructuring vs. no destructuring criteria." =
  (module struct
    let run_test expr =
      expr
      |> Ppx_let_expander.expand
           Ppx_bonsai_expander.arr
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
            } ((NO_DESTRUCTION)[@ppxlib.enter_value a]) ~f:(fun a -> BODY) |}]
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
    let __let_syntax__059_ = ((NO_DESTRUCTION)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                        ]
    and __let_syntax__060_ = ((ALSO_NO_DESTRUCTION)[@ppxlib.enter_value b])
    [@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            } (Let_syntax.both __let_syntax__059_ __let_syntax__060_)
      ~f:(fun (a, b) -> BODY) |}]
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
    let __let_syntax__062_ = ((NO_DESTRUCTION)[@ppxlib.enter_value a])[@@ppxlib.do_not_enter_value
                                                                        ]
    and __let_syntax__063_ = ((ALSO_NO_DESTRUCTION)[@ppxlib.enter_value b])
    [@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            } (Let_syntax.both __let_syntax__062_ __let_syntax__063_)
      ~f:(fun ((_ as a), (_ as b)) -> BODY) |}]
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
         ~equal:(fun ((_ as __old_for_cutoff__066_), _)
                   ((_ as __new_for_cutoff__065_), _) ->
                   phys_equal __old_for_cutoff__066_ __new_for_cutoff__065_))
      ~f:(fun ((_ as a), _) -> BODY) |}]
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
         ~equal:(fun (((__old_for_cutoff__071_, _) : t), __old_for_cutoff__069_)
                   (((__new_for_cutoff__070_, _) : t), __new_for_cutoff__068_) ->
                   (phys_equal __old_for_cutoff__071_ __new_for_cutoff__070_) &&
                     (phys_equal __old_for_cutoff__069_ __new_for_cutoff__068_)))
      ~f:(fun (((b, _) : t), c) -> BODY) |}]
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
    let __let_syntax__073_ = DESTRUCT[@@ppxlib.do_not_enter_value ]
    and __let_syntax__074_ = ALSO_DESTRUCT[@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (Let_syntax.both
         (Let_syntax.cutoff __let_syntax__073_
            ~equal:(fun
                      (((__old_for_cutoff__078_, _) : t), __old_for_cutoff__076_)
                      (((__new_for_cutoff__077_, _) : t), __new_for_cutoff__075_)
                      ->
                      (phys_equal __old_for_cutoff__078_ __new_for_cutoff__077_)
                        &&
                        (phys_equal __old_for_cutoff__076_ __new_for_cutoff__075_)))
         (Let_syntax.cutoff __let_syntax__074_
            ~equal:(fun
                      (((__old_for_cutoff__082_, _) : t), __old_for_cutoff__080_)
                      (((__new_for_cutoff__081_, _) : t), __new_for_cutoff__079_)
                      ->
                      (phys_equal __old_for_cutoff__082_ __new_for_cutoff__081_)
                        &&
                        (phys_equal __old_for_cutoff__080_ __new_for_cutoff__079_))))
      ~f:(fun ((((b, _) : t), c), (((d, _) : t), e)) -> BODY) |}]
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
            } NO_DESTRUCTION ~f:(fun { a; b } -> BODY) |}]
    ;;

    let%expect_test "'closed' record results in no destruction (multiple arms)" =
      run_test
        [%expr
          let { a; b } = NO_DESTRUCTION
          and { c; d } = ALSO_NO_DESTRUCTION in
          BODY];
      [%expect
        {|
    let __let_syntax__085_ = NO_DESTRUCTION[@@ppxlib.do_not_enter_value ]
    and __let_syntax__086_ = ALSO_NO_DESTRUCTION[@@ppxlib.do_not_enter_value ] in
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            } (Let_syntax.both __let_syntax__085_ __let_syntax__086_)
      ~f:(fun ({ a; b }, { c; d }) -> BODY) |}]
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
            } NO_DESTRUCTION ~f:(fun ({ a; b = (module X)  }, c) -> BODY) |}]
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
                   (({ a = __old_for_cutoff__092_; b = (module _)  } : t),
                    __old_for_cutoff__090_)
                   (({ a = __new_for_cutoff__091_; b = (module _)  } : t),
                    __new_for_cutoff__089_)
                   ->
                   (phys_equal __old_for_cutoff__092_ __new_for_cutoff__091_) &&
                     (phys_equal __old_for_cutoff__090_ __new_for_cutoff__089_)))
      ~f:(fun (({ a; b = (module _)  } : t), c) -> BODY) |}]
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
      ~f:(fun (({ a = _; b = (module _)  } : t), _) -> BODY) |}]
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
             ~equal:(fun { a = __old_for_cutoff__098_; b = __old_for_cutoff__098_;_}
                       { a = __new_for_cutoff__097_; b = __new_for_cutoff__097_;_} ->
                       (phys_equal __old_for_cutoff__098_ __new_for_cutoff__097_) &&
                         (phys_equal __old_for_cutoff__096_ __new_for_cutoff__095_)))
          ~f:(fun { a; b = a;_} -> BODY) |}]
    ;;
  end)
;;

let%expect_test "current match%arr behavior" =
  Ppx_let_expander.expand
    Ppx_bonsai_expander.arr
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
