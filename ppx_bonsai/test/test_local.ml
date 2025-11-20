module type S = sig
  type 'a t
  type graph

  module Let_syntax : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val return : 'a -> local_ graph -> 'a t

    module Let_syntax : sig
      val map : here:[%call_pos] -> 'a t -> f:('a -> 'b) -> 'b t
      val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
      val both : 'a t -> 'b t -> ('a * 'b) t
      val arr : here:[%call_pos] -> 'a t -> f:('a -> 'b) -> 'b t
      val arr2 : here:[%call_pos] -> 'a1 t -> 'a2 t -> f:('a1 -> 'a2 -> 'b) -> 'b t

      val arr3
        :  here:[%call_pos]
        -> 'a1 t
        -> 'a2 t
        -> 'a3 t
        -> f:('a1 -> 'a2 -> 'a3 -> 'b)
        -> 'b t

      val arr4
        :  here:[%call_pos]
        -> 'a1 t
        -> 'a2 t
        -> 'a3 t
        -> 'a4 t
        -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b)
        -> 'b t

      val arr5
        :  here:[%call_pos]
        -> 'a1 t
        -> 'a2 t
        -> 'a3 t
        -> 'a4 t
        -> 'a5 t
        -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b)
        -> 'b t

      val arr6
        :  here:[%call_pos]
        -> 'a1 t
        -> 'a2 t
        -> 'a3 t
        -> 'a4 t
        -> 'a5 t
        -> 'a6 t
        -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b)
        -> 'b t

      val arr7
        :  here:[%call_pos]
        -> 'a1 t
        -> 'a2 t
        -> 'a3 t
        -> 'a4 t
        -> 'a5 t
        -> 'a6 t
        -> 'a7 t
        -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b)
        -> 'b t

      val return : 'a t -> 'a t
      val cutoff : 'a t -> equal:('a -> 'a -> bool) -> 'a t

      val switch
        :  here:Lexing.position
        -> match_:int t
        -> branches:int
        -> with_:local_ (int -> 'a t)
        -> 'a t

      val sub : ?here:_ -> 'a -> f:local_ ('a -> 'b) -> 'b
    end
  end
end

module M (X : S) = struct
  open X.Let_syntax

  let _arrow_example_1 a (local_ graph) =
    match%sub a with
    | 0 -> return true graph
    | _ -> return false graph
  ;;

  let _arrow_example_2 a (local_ graph) =
    match%sub a with
    | 0 -> return true graph
    | b ->
      let%map b in
      b = 1
  ;;

  let _arrow_example_3 a (local_ graph) =
    match%sub a with
    | 0 -> return true graph
    | _ as b ->
      let%map b in
      b = 1
  ;;

  type abc = A of int

  let _a = A 1

  let _arrow_example_4 a : _ X.t =
    match%sub a with
    | A b ->
      let%map b in
      b = 1
  ;;

  let _arrow_example_5 (a : _ X.t) : _ X.t =
    let%arr a in
    a
  ;;

  let _arrow_example_6 : 'a X.t -> 'a X.t =
    function%arr
    | a -> a
  ;;

  let _arrow_example_7 : 'a X.t -> 'a X.t =
    function%sub
    | a -> a
  ;;

  let _arrow_example_8 : default:'a -> 'a option X.t -> 'a X.t =
    fun ~default ->
    function%arr
    | Some a -> a
    | None -> default
  ;;

  let _arrow_example_9 : default:'a X.t -> 'a option X.t -> 'a X.t =
    fun ~default ->
    function%sub
    | Some a -> a
    | None -> default
  ;;

  let (_ : _) =
    fun (local_ graph) ->
    (* Use this code to test if :MerlinTypeOf behaves properly. In particular, you should
       be able to:

       1. Ask for the type of things inside each arm of the [match%sub].

       2. Ask for the type of the entire [match%sub] by putting your cursor on the
          [match%sub] or [with].

       3. Ask for the type of the expression being matched on inside of the [match%sub].

       4. Ask for the type of the case patterns of the match statement.
          - The type of ppat_var's alone (e.g. case_1_lhs) is ['a X.t]
          - The type of anything else is the type that the pattern would normally be.
          - This distinction is weird, but it makes the variables identifiers match the
            type on the right hand side.

       5. Add [+ "hi"] to random parts of the string and make sure that the type error is
          on the right location.

       6. Append [, _ ] to the lhs parts of the match arms to make sure that the pattern
          matching error is in the right location.
    *)
    match%sub return (Some "hello") graph with
    | Some "heyo" -> return 0. graph
    | Some case_1_lhs ->
      let%arr lhs_a = case_1_lhs
      and lhs_b = return 2. graph
      and lhs_c = return 3. graph in
      Float.of_string lhs_a +. lhs_b +. lhs_c
    | None -> return (1. +. 4.) graph
  ;;

  let (_ : _) =
    fun (local_ graph) ->
    (* This code tests that the same annotations used on the match statement result in the
       same results doing :MerlinTypeOf. Sadly, an implementation that strips away all
       constraints would still pass this test, so try to swap [string X.t] with [int X.t]
       to verify that this isn't just stripping away type annotations. *)
    match%sub return (Some "hello") graph with
    | (Some ("heyo" : string) : string option) -> return 0. graph
    | ((Some (((case_1_lhs : string X.t) as _x3 : string X.t) : string X.t) as _x :
         string option X.t) as _x2 :
        string option X.t) ->
      let%arr lhs_a = case_1_lhs
      and lhs_b = return 2. graph
      and lhs_c = return 3. graph in
      Float.of_string lhs_a +. lhs_b +. lhs_c
    | (None : string option) -> return (1. +. 4.) graph
  ;;
end
