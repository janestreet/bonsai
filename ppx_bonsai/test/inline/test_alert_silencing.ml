open! Core
open Ppxlib
open Test_util

let locality = Ppx_let_expander.Locality.global
let loc = Location.none

let expand_arr expr =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.arr Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    expr
  |> print_expr
;;

let expand_sub expr =
  Ppx_let_expander.expand
    (Ppx_bonsai_expander.sub Location_of_callsite)
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality
    expr
  |> print_expr
;;

let%expect_test "[let%arr] without destructuring generates no alert-silencing" =
  expand_arr
    [%expr
      let a = MY_EXPR in
      MY_BODY];
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            } ((MY_EXPR)[@ppxlib.enter_value a]) ~f:(fun a -> MY_BODY)
    |}]
;;

let%expect_test "[let%sub] pattern projections silence the alert on map only" =
  expand_sub
    [%expr
      let a, b = MY_EXPR in
      MY_BODY];
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } MY_EXPR
        ~f:(fun __pattern_syntax__002_ ->
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
                               } __pattern_syntax__002_
                         ~f:(function
                             | (_, __pattern_syntax__004_) ->
                                 __pattern_syntax__004_))[@merlin.hide ]))
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
                                         } __pattern_syntax__002_
                                   ~f:(function
                                       | (__pattern_syntax__003_, _) ->
                                           __pattern_syntax__003_))
                               [@merlin.hide ]))
                            ~f:(fun a ->
                                  let () = ()[@@merlin.hide ] in ((MY_BODY)
                                    [@nontail ])))
                        [@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "[let%sub] user-written map is not silenced" =
  expand_sub
    [%expr
      let a, b = Let_syntax.map ~here:MY_POS MY_EXPR ~f:MY_FN in
      MY_BODY];
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.map ~here:MY_POS MY_EXPR ~f:MY_FN)
        ~f:(fun __pattern_syntax__005_ ->
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
                               } __pattern_syntax__005_
                         ~f:(function
                             | (_, __pattern_syntax__007_) ->
                                 __pattern_syntax__007_))[@merlin.hide ]))
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
                                         } __pattern_syntax__005_
                                   ~f:(function
                                       | (__pattern_syntax__006_, _) ->
                                           __pattern_syntax__006_))
                               [@merlin.hide ]))
                            ~f:(fun a ->
                                  let () = ()[@@merlin.hide ] in ((MY_BODY)
                                    [@nontail ])))
                        [@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "[let%sub] zero-variable destructuring path silences map" =
  expand_sub
    [%expr
      let () = MY_EXPR in
      MY_BODY];
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } MY_EXPR
        ~f:(fun __pattern_syntax__008_ ->
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
                               } __pattern_syntax__008_ ~f:(function | () -> ()))
                     [@nontail ]))
                  ~f:(fun _ ->
                        let () = ()[@@merlin.hide ] in ((MY_BODY)[@nontail ])))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "[match%sub] case-number computation silences the alert" =
  expand_sub
    [%expr
      match MY_EXPR with
      | Choice_1 x -> CHOICE_1_BODY x
      | Choice_2 -> CHOICE_2_BODY];
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__009_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                              __pattern_syntax__009_
                              ~f:(function | Choice_1 x -> 0 | Choice_2 -> 1))
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
                                               } __pattern_syntax__009_
                                         ~f:((function
                                              | Choice_1 __pattern_syntax__010_
                                                  -> __pattern_syntax__010_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun x -> ((CHOICE_1_BODY x)[@nontail ])))
                              [@nontail ])
                          | ((1)[@merlin.hide ]) -> CHOICE_2_BODY
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}]
;;

let%expect_test "[match%arr] silences the alert" =
  expand_arr
    [%expr
      match MY_A, MY_B with
      | a, b -> MY_BODY (a, b)];
  [%expect
    {|
    Let_syntax.arr
      ~here:{
              Ppx_here_lib.pos_fname = "_none_";
              pos_lnum = 0;
              pos_cnum = (-1);
              pos_bol = 0
            }
      (let __let_syntax__013_ = ((MY_A)
         [@ppxlib.enter_value __ppx_bonsai_tuple__011_])[@@ppxlib.do_not_enter_value
                                                          ]
       and __let_syntax__014_ = ((MY_B)
         [@ppxlib.enter_value __ppx_bonsai_tuple__012_])[@@ppxlib.do_not_enter_value
                                                          ] in
       ((Let_syntax.map2)[@alert "-for_ppx_bonsai"])
         ~here:{
                 Ppx_here_lib.pos_fname = "_none_";
                 pos_lnum = 0;
                 pos_cnum = (-1);
                 pos_bol = 0
               } __let_syntax__013_ __let_syntax__014_
         ~f:(fun __ppx_bonsai_tuple__011_ __ppx_bonsai_tuple__012_ ->
               (__ppx_bonsai_tuple__011_, __ppx_bonsai_tuple__012_)))
      ~f:(function | (a, b) -> MY_BODY (a, b))
    |}]
;;

(* Guards aren't allowed in match%sub anyway, but just as an extra check: *)
let%expect_test "[match%sub] map in a when branch is not silenced" =
  expand_sub
    [%expr
      match MY_EXPR with
      | Choice_1 x when Let_syntax.map ~here:MY_POS MY_GUARD ~f:MY_FN -> CHOICE_1_BODY x
      | Choice_2 -> CHOICE_2_BODY];
  [%expect
    {|
    ((Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 0;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__017_ ->
              ((Let_syntax.switch
                  ~here:{
                          Ppx_here_lib.pos_fname = "_none_";
                          pos_lnum = 0;
                          pos_cnum = (-1);
                          pos_bol = 0
                        }
                  ~match_:((((Let_syntax.map)[@alert "-for_ppx_bonsai"])
                              __pattern_syntax__017_
                              ~f:(function
                                  | Choice_1 x when
                                      Let_syntax.map ~here:MY_POS MY_GUARD
                                        ~f:MY_FN
                                      -> 0
                                  | Choice_2 -> 1))[@ocaml.warning "-26-27"])
                  ~branches:2
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
                                               } __pattern_syntax__017_
                                         ~f:((function
                                              | Choice_1 __pattern_syntax__018_
                                                  -> __pattern_syntax__018_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun x -> ((CHOICE_1_BODY x)[@nontail ])))
                              [@nontail ])
                          | ((1)[@merlin.hide ]) -> CHOICE_2_BODY
                          | _ -> assert false))
              [@nontail ])))
    [@nontail ])
    |}]
;;
