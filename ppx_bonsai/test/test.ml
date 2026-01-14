module _ = Test_local
module _ = Test_match_sub_tuples

module Arrow_example = struct
  open Arrow.Let_syntax
  module X = Arrow

  let _arrow_example_1 a : _ X.c =
    match%sub a with
    | 0 -> return (X.return_v true)
    | _ -> return (X.return_v false)
  ;;

  let _arrow_example_2 a : _ X.c =
    match%sub a with
    | 0 -> return (X.return_v true)
    | b ->
      return
        (let%map b in
         b = 1)
  ;;

  let _arrow_example_3 a : _ X.c =
    match%sub a with
    | 0 -> return (X.return_v true)
    | _ as b ->
      return
        (let%map b in
         b = 1)
  ;;

  type abc = A of int

  let _a = A 1

  let _arrow_example_4 a : _ X.c =
    match%sub a with
    | A b ->
      return
        (let%map b in
         b = 1)
  ;;

  let _arrow_example_5 (a : _ X.v) : _ X.c =
    let%arr a in
    a
  ;;

  let _arrow_example_6 : 'a X.v -> 'a X.c =
    function%arr
    | a -> a
  ;;

  let _arrow_example_7 : 'a X.v -> 'a X.c =
    function%sub
    | a -> return a
  ;;

  let _arrow_example_8 : default:'a -> 'a option X.v -> 'a X.c =
    fun ~default ->
    function%arr
    | Some a -> a
    | None -> default
  ;;

  let _arrow_example_9 : default:'a X.v -> 'a option X.v -> 'a X.c =
    fun ~default ->
    function%sub
    | Some a -> return a
    | None -> return default
  ;;

  let (_ : _) =
    (* Use this code to test if :MerlinTypeOf behaves properly. In particular, you should
       be able to:

       1. Ask for the type of things inside each arm of the [match%sub].

       2. Ask for the type of the entire [match%sub] by putting your cursor on the
          [match%sub] or [with].

       3. Ask for the type of the expression being matched on inside of the [match%sub].

       4. Ask for the type of the case patterns of the match statement.
          - The type of ppat_var's alone (e.g. case_1_lhs) is ['a X.v]
          - The type of anything else is the type that the pattern would normally be.
          - This distinction is weird, but it makes the variables identifiers match the
            type on the right hand side.

       5. Add [+ "hi"] to random parts of the string and make sure that the type error is
          on the right location.

       6. Append [, _ ] to the lhs parts of the match arms to make sure that the pattern
          matching error is in the right location.
    *)
    match%sub X.return_v (Some "hello") with
    | Some "heyo" -> return (X.return_v 0.)
    | Some case_1_lhs ->
      let%arr lhs_a = case_1_lhs
      and lhs_b = X.return_v 2.
      and lhs_c = X.return_v 3. in
      Float.of_string lhs_a +. lhs_b +. lhs_c
    | None -> return (X.return_v (1. +. 4.))
  ;;

  let (_ : _) =
    (* This code tests that the same annotations used on the match statement result in the
       same results doing :MerlinTypeOf. Sadly, an implementation that strips away all
       constraints would still pass this test, so try to swap [string X.v] with [int X.v]
       to verify that this isn't just stripping away type annotations. *)
    match%sub X.return_v (Some "hello") with
    | (Some ("heyo" : string) : string option) -> return (X.return_v 0.)
    | ((Some (((case_1_lhs : string X.v) as _x3 : string X.v) : string X.v) as _x :
         string option X.v) as _x2 :
        string option X.v) ->
      let%arr lhs_a = case_1_lhs
      and lhs_b = X.return_v 2.
      and lhs_c = X.return_v 3. in
      Float.of_string lhs_a +. lhs_b +. lhs_c
    | (None : string option) -> return (X.return_v (1. +. 4.))
  ;;
end
