open! Core
open Ppxlib
open Ast_builder.Default
open Ppx_let_expander
open Ppx_pattern_bind

module Location_behavior = struct
  type t =
    | Location_of_callsite (** Uses [~here:[%here]] *)
    | Location_in_scope (** Uses [~here:here] *)

  let to_ppx_let_behavior = function
    | Location_of_callsite -> Ppx_let_expander.With_location.Location_of_callsite
    | Location_in_scope -> Location_in_scope "here"
  ;;
end

(* A helper module for removing [%lazy] from an expression. *)
module Extract_lazy_extension : sig
  type t =
    { expr_without_lazy : expression
    ; graph_name : label loc
    }

  (** If it finds a [%lazy]/[%lazy graph_name] at the beginning of an expression, it
      removes it and returns a label loc corresponding to the correct graph name to use. *)
  val extract : expression -> t option
end = struct
  type t =
    { expr_without_lazy : expression (** The new expression without [%lazy]. *)
    ; graph_name : label loc
    (** The graph name found in the [%lazy] block, or "graph" by default. *)
    }

  let parse_graph_name ~extension_loc ~extension_payload =
    let name, loc =
      match extension_payload with
      | PStr [] -> "graph", extension_loc
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_ident { txt = Lident graph; loc = graph_loc }; _ }
                  , _ )
            ; _
            }
          ] -> graph, graph_loc
      | _ ->
        Location.raise_errorf
          ~loc:extension_loc
          "This should either have no arguments (i.e. [%%lazy]) or a single graph \
           identifier (e.g. [%%lazy my_graph])."
    in
    name, { loc with loc_ghost = true }
  ;;

  (** [Some _] if a [%lazy] was found and removed, and [None] otherwise *)
  let rec extract expr : t option =
    match
      Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:expr.pexp_loc expr.pexp_desc
    with
    (*=Things that look like function application.: [%lazy] x | [%lazy] f x *)
    | Pexp_apply
        ( { pexp_desc = Pexp_extension ({ txt = "lazy"; loc = _ }, extension_payload)
          ; pexp_loc
          ; _
          }
        , (Nolabel, f) :: args ) ->
      let expr_without_lazy =
        match args with
        | [] -> f
        | _ :: _ -> Ast_helper.Exp.apply ~loc:expr.pexp_loc f args
      in
      let graph, graph_loc =
        parse_graph_name ~extension_loc:pexp_loc ~extension_payload
      in
      let graph_name = Located.mk ~loc:graph_loc graph in
      Some { expr_without_lazy; graph_name }
    (* Things that look like tuples: [%lazy] x, y *)
    | Pexp_tuple ((None, first) :: rest) ->
      let%map.Option { expr_without_lazy = first'; graph_name } = extract first in
      let expr_without_lazy =
        { expr with
          pexp_desc =
            Ppxlib_jane.Shim.Expression_desc.to_parsetree
              ~loc:expr.pexp_loc
              (Pexp_tuple ((None, first') :: rest))
        }
      in
      { expr_without_lazy; graph_name }
    | _ -> None
  ;;
end

(* Maps tuples of [('a Bonsai.t * 'b Bonsai.t ...)] to [('a * 'b ...) Bonsai.t] *)
let rec match_tuple_mapper
  ~loc
  ~location_behavior
  ~modul
  ~locality
  ~(expressions : expression list)
  =
  let open (val Ast_builder.make loc) in
  let temp_variable_to_expression =
    List.map expressions ~f:(fun expression ->
      let temp_name = gen_symbol ~prefix:"__ppx_bonsai_tuple" () in
      temp_name, expression)
  in
  let value_t_that_tuples_everything =
    let value_bindings =
      List.map temp_variable_to_expression ~f:(fun (temp_variable_name, expression) ->
        value_binding ~pat:(ppat_var (Located.mk temp_variable_name)) ~expr:expression)
    in
    let tuple_creation =
      pexp_tuple
        (List.map temp_variable_to_expression ~f:(fun (temp_variable_name, _) ->
           pexp_ident (Located.mk (Lident temp_variable_name))))
    in
    pexp_let Nonrecursive value_bindings tuple_creation
  in
  (* Using the [arrn] expander lets us handle n-tuples where n > 7. We still expand to
     [map] instead of [arr] for backwards compatibility with [Proc]. *)
  Ppx_let_expander.expand
    (arr ~expand_with:"map" location_behavior)
    Extension_kind.n
    ~modul
    ~locality
    value_t_that_tuples_everything

and join_tuples_into_single_t ~loc ~location_behavior ~modul ~locality expr =
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc expr.pexp_desc with
  | Pexp_tuple labeled_expressions ->
    (match Ppxlib_jane.as_unlabeled_tuple labeled_expressions with
     | Some expressions ->
       match_tuple_mapper
         ~location_behavior
         ~modul
         ~loc:{ expr.pexp_loc with loc_ghost = true }
         ~expressions
         ~locality
     | None -> expr)
  | _ -> expr

and arr ~(expand_with : label) (location_behavior : Location_behavior.t) : (module Ext) =
  let module Arr : Ext = struct
    let name = "arr"
    let with_location = Location_behavior.to_ppx_let_behavior location_behavior
    let prevent_tail_call = false

    let location_ghoster =
      object
        inherit Ast_traverse.map as super
        method! location loc = super#location { loc with loc_ghost = true }
      end
    ;;

    (* Check for nested let%arr or let%sub extensions *)
    let assert_no_nested_let_arr_or_let_sub_extensions ~outer_context expression =
      let describe_extension ~extension_name payload =
        match payload with
        | PStr [ { pstr_desc = Pstr_eval ({ pexp_desc; _ }, _); _ } ] ->
          (match pexp_desc with
           | Pexp_let _ -> "let%" ^ extension_name
           | Pexp_match _ -> "match%" ^ extension_name
           | Pexp_ifthenelse _ -> "if%" ^ extension_name
           | _ -> "%" ^ extension_name)
        | _ -> "%" ^ extension_name
      in
      let finder =
        object
          inherit [unit] Ast_traverse.fold as super

          method! expression expr acc =
            match expr.pexp_desc with
            | Pexp_extension ({ txt = ("arr" | "sub") as extension_name; loc }, payload)
              ->
              let extension_desc = describe_extension ~extension_name payload in
              Location.raise_errorf
                ~loc
                "Nested %s is not allowed. You cannot use %%arr or %%sub extensions \
                 inside the body of another %s."
                extension_desc
                outer_context
            | _ -> super#expression expr acc
        end
      in
      finder#expression expression ()
    ;;

    module Extracted_variable = struct
      type t =
        { original_label : label loc
        ; new_label : label loc
        ; old_label : label loc
        }

      let of_original_name : label loc -> t =
        fun original_label ->
        let new_label =
          { original_label with txt = gen_symbol ~prefix:"__new_for_cutoff" () }
        in
        let old_label =
          { original_label with txt = gen_symbol ~prefix:"__old_for_cutoff" () }
        in
        { original_label; old_label; new_label }
      ;;
    end

    let ignores_at_least_1_subpattern pattern =
      let ignore_finder =
        object
          inherit [bool] Ast_traverse.fold as super

          method! pattern (pattern : pattern) acc =
            match acc with
            | true -> true
            | false ->
              (match Ppxlib_jane.Shim.Pattern_desc.of_parsetree pattern.ppat_desc with
               (* let (_ as a) = x in ... *)
               | Ppat_alias (_, _) -> false
               | Ppat_any
               (* let { a ; b ; _ } = x in ... *)
               | Ppat_record (_, Open)
               (* let ~a, .. = x in ... *)
               | Ppat_tuple (_, Open)
               (* let { a = (module _) ; b } = x in ... *)
               | Ppat_unpack { txt = None; _ } -> true
               | Ppat_record (_, Closed)
               | Ppat_tuple (_, Closed)
               | Ppat_unpack { txt = Some _; _ } -> super#pattern pattern acc
               | _ -> super#pattern pattern acc)
        end
      in
      ignore_finder#pattern pattern false
    ;;

    let duplicate_pattern pattern =
      let ident_to_extracted_variable =
        let variables_of =
          object
            inherit [Extracted_variable.t String.Map.t] Ast_traverse.fold as super

            method! pattern p acc =
              let acc = super#pattern p acc in
              match p.ppat_desc with
              | Ppat_var var | Ppat_alias (_, var) ->
                let extracted_variable = Extracted_variable.of_original_name var in
                Core.Map.set acc ~key:var.txt ~data:extracted_variable
              | _ -> acc
          end
        in
        variables_of#pattern pattern String.Map.empty
      in
      let old_pattern =
        replace_variable
          ~f:(fun label ->
            match Core.Map.find ident_to_extracted_variable label.txt with
            | None -> `Remove
            | Some extracted_variable -> `Rename extracted_variable.old_label.txt)
          pattern
      in
      let new_pattern =
        replace_variable
          ~f:(fun label ->
            match Core.Map.find ident_to_extracted_variable label.txt with
            | None -> `Remove
            | Some extracted_variable -> `Rename extracted_variable.new_label.txt)
          pattern
      in
      old_pattern, new_pattern, Core.Map.data ident_to_extracted_variable
    ;;

    let extract_phys_equal_check ~loc variables =
      let located_ident_to_longident (label : label loc) : longident_loc =
        let { txt; loc } = label in
        { txt = lident txt; loc }
      in
      let phys_equalities =
        List.map
          variables
          ~f:(fun { Extracted_variable.old_label; new_label; original_label = _ } ->
            let old_label = pexp_ident ~loc (located_ident_to_longident old_label) in
            let new_label = pexp_ident ~loc (located_ident_to_longident new_label) in
            [%expr phys_equal [%e old_label] [%e new_label]])
      in
      List.reduce phys_equalities ~f:(fun prev next -> [%expr [%e next] && [%e prev]])
    ;;

    let add_cutoff_to_value_binding ~loc ~modul value_binding =
      let old_pattern, new_pattern, variables = duplicate_pattern value_binding.pvb_pat in
      let check = extract_phys_equal_check ~loc variables in
      let fn =
        match check with
        | None -> [%expr fun _ _ -> true]
        | Some check ->
          location_ghoster#expression
            [%expr fun [%p old_pattern] [%p new_pattern] -> [%e check]]
      in
      let expr =
        bind_apply
          ~prevent_tail_call
          ~fn_label:"equal"
          ~op_name:"cutoff"
          ~loc
          ~modul
          ~with_location:
            (match location_behavior with
             | Location_behavior.Location_of_callsite -> No_location
             | Location_behavior.Location_in_scope -> Location_in_scope "here")
          ~arg:value_binding.pvb_expr
          ~fn
          ()
      in
      { value_binding with pvb_expr = expr }
    ;;

    let maybe_add_cutoff_to_value_binding
      ~(loc : location)
      ~(modul : longident loc option)
      (value_binding : value_binding)
      =
      let loc = { loc with loc_ghost = true } in
      match ignores_at_least_1_subpattern value_binding.pvb_pat with
      | false -> value_binding
      | true -> add_cutoff_to_value_binding ~loc ~modul value_binding
    ;;

    let disallow_expression ~loc:_ _ = function
      | Pexp_while (_, _) -> Error "while%%arr is not supported."
      | _ -> Ok ()
    ;;

    let destruct ~assume_exhaustive:_ ~loc:_ ~modul:_ ~lhs:_ ~rhs:_ ~body:_ = None

    (* These functions have been copied verbatim. *)
    module From_let_expander = struct
      (* Wrap a function body in [exclave_] *)
      let wrap_exclave ~loc expr = [%expr exclave_ [%e expr]]

      let maybe_wrap_exclave ~loc ~locality expr =
        match locality with
        | `global -> expr
        | `local -> wrap_exclave ~loc expr
      ;;
    end

    module Expand_balanced = struct
      open From_let_expander

      module Min_and_max = struct
        type 'a t =
          { min : 'a
          ; max : 'a
          }
      end

      let find_min_and_max_position =
        object
          inherit [position Min_and_max.t] Ast_traverse.fold

          method! location location { min; max } =
            let min = Location.min_pos location.loc_start min
            and max = Location.max_pos location.loc_end max in
            { Min_and_max.min; max }
        end
      ;;

      let find_min_and_max_positions : pattern Nonempty_list.t -> position Min_and_max.t =
        fun bindings ->
        let (hd :: tl) = bindings in
        let init =
          { Min_and_max.min = hd.ppat_loc.loc_start; max = hd.ppat_loc.loc_end }
        in
        List.fold ~init tl ~f:(fun { min; max } pattern ->
          find_min_and_max_position#pattern pattern { min; max })
      ;;

      let tupleize (bindings : pattern Nonempty_list.t) ~build_multiarg_fun =
        let tuple_loc =
          let%tydi { min; max } = find_min_and_max_positions bindings in
          { loc_start = min; loc_end = max; loc_ghost = true }
        in
        (* Produces a tuple pattern, with the original names of the bindings. *)
        let tuple_pat_for_toplevel_f =
          ppat_tuple ~loc:tuple_loc (Nonempty_list.to_list bindings)
        in
        (* Produces an expression like [fun x1 x2 x3 ... xn -> (x1, x2, x3, ..., xn)] *)
        let tuplize_n_fun =
          let names_and_locs =
            Nonempty_list.mapi bindings ~f:(fun i { ppat_loc; _ } ->
              [%string "t%{i#Int}"], { ppat_loc with loc_ghost = true })
          in
          let tuple_exp =
            Nonempty_list.map names_and_locs ~f:(fun (name, loc) -> evar name ~loc)
            |> Nonempty_list.to_list
            |> pexp_tuple ~loc:tuple_loc
          in
          let args =
            Nonempty_list.map names_and_locs ~f:(fun (name, loc) -> pvar name ~loc)
          in
          build_multiarg_fun ~args ~body:tuple_exp
        in
        tuplize_n_fun, tuple_pat_for_toplevel_f
      ;;

      let expand
        ~loc
        ~modul
        ~locality
        ~(with_location : With_location.t)
        ~n
        ppx_bindings
        ppx_body
        =
        let operator ~op_name = function
          | 1 -> eoperator ~loc ~modul op_name
          | n -> eoperator ~loc ~modul [%string "%{op_name}%{n#Int}"]
        in
        let build_multiarg_fun ~args ~body =
          Nonempty_list.fold_right
            args
            ~init:(maybe_wrap_exclave ~loc ~locality body)
            ~f:(fun pat inner ->
              maybe_destruct
                ~destruct
                ~modul
                ~return_value_in_exclave:false
                ~zero_alloc:false
                ~loc
                ~lhs:pat
                ~body:inner)
        in
        let build_application unlabelled_exps ~f_exp ~op_name =
          let args =
            (match with_location with
             | With_location.No_location -> []
             | With_location.Location_of_callsite ->
               [ Ppx_let_expander.location_arg ~loc ]
             | With_location.Location_in_scope name ->
               [ Ppx_let_expander.location_arg_in_scope ~loc name ])
            @ (Nonempty_list.map unlabelled_exps ~f:(fun exp -> Nolabel, exp)
               |> Nonempty_list.to_list)
            @ [ Labelled "f", f_exp ]
          in
          pexp_apply ~loc (operator ~op_name (Nonempty_list.length unlabelled_exps)) args
        in
        let rec loop = function
          | Balance_list_tree.Leaf vb -> vb.pvb_expr, vb.pvb_pat
          | Node children ->
            let exps, pats = Nonempty_list.map children ~f:loop |> Nonempty_list.unzip in
            let tuplize_n_fun, tuple_pat_for_toplevel_f =
              tupleize pats ~build_multiarg_fun
            in
            let mapn_exp =
              (* NOTE: We are using [map] here instead of [arr] for backwards
                 compatibility with the [Proc] API. Using [map] is safe here as each [map]
                 call is only used/depended on once. *)
              build_application exps ~f_exp:tuplize_n_fun ~op_name:"map"
            in
            mapn_exp, tuple_pat_for_toplevel_f
        in
        match Balance_list_tree.balance ~n ppx_bindings with
        | Error err -> invalid_arg (Error.to_string_hum err)
        | Ok balanced ->
          let subtrees =
            match balanced with
            | Balance_list_tree.Leaf _ -> Nonempty_list.singleton balanced
            | Node xs -> xs
          in
          let exps, pats = Nonempty_list.map subtrees ~f:loop |> Nonempty_list.unzip in
          let f_exp = build_multiarg_fun ~args:pats ~body:ppx_body in
          build_application exps ~f_exp ~op_name:expand_with
      ;;
    end

    let wrap_expansion
      :  loc:location -> modul:longident loc option -> value_binding list -> expression
      -> expand:(loc:location -> value_binding list -> expression -> expression)
      -> expression
      =
      fun ~loc ~modul value_bindings expression ~expand:_ ->
      assert_no_nested_let_arr_or_let_sub_extensions ~outer_context:"let%arr" expression;
      let value_bindings =
        List.map value_bindings ~f:(maybe_add_cutoff_to_value_binding ~loc ~modul)
      in
      Expand_balanced.expand
        ~loc
        ~modul
        ~locality:`global
        ~with_location
        ~n:7
        value_bindings
        expression
    ;;

    let bool ~loc bool =
      let constructor = if bool then "true" else "false" in
      pexp_construct ~loc { txt = lident constructor; loc } None
    ;;

    let build_cutoff_expr ~loc ~modul expr cases =
      (* If we were just checking for the same branch case below (i.e. if your cases have
         patterns A and B, then checking against patterns (A, A) and (B, B)), then we
         would need a final case for (_, _) to catch (A, B), etc.

         However, since we add cases like (A, _) and (_, A) in addition to (A, A), our
         cases must be exhaustive if the original match cases are exhaustive, even without
         a final (_, _) case. *)
      let equal_cases =
        List.concat_map cases ~f:(fun case ->
          let old_pattern, new_pattern, variables = duplicate_pattern case.pc_lhs in
          (* Either the pattern matches to both the old and new values, and you stay on
             the same branch... *)
          let same_branch_case =
            let check = extract_phys_equal_check ~loc variables in
            let both_pattern = [%pat? [%p old_pattern], [%p new_pattern]] in
            let new_rhs =
              match check with
              | Some check -> check
              | None -> bool ~loc true
            in
            { pc_lhs = both_pattern; pc_guard = None; pc_rhs = new_rhs }
          in
          (* ...or the pattern matches to exactly one of the old and new values, which
             means you're definitely changing branches. *)
          let different_branch_case =
            let either_pattern = [%pat? [%p new_pattern], _ | _, [%p new_pattern]] in
            let false_rhs = bool ~loc false in
            { pc_lhs = either_pattern; pc_guard = None; pc_rhs = false_rhs }
          in
          match case.pc_guard, case.pc_lhs.ppat_desc with
          | None, Ppat_any ->
            (* (In this case, the different-branch case is always redundant.) *)
            [ same_branch_case ]
          | None, _ -> [ same_branch_case; different_branch_case ]
          | Some guard, _ ->
            let err =
              Location.error_extensionf
                ~loc:guard.pexp_loc
                "match%%arr doesn't support when clauses. Try using let%%arr ... match \
                 ... or casing within a match%%arr arm instead"
              |> pexp_extension ~loc:guard.pexp_loc
            in
            [ { same_branch_case with pc_guard = Some err } ])
      in
      let old_var = gen_symbol ~prefix:"__old_for_cutoff_ppx_bonsai" () in
      let new_var = gen_symbol ~prefix:"__new_for_cutoff_ppx_bonsai" () in
      let match_ =
        let e =
          pexp_match
            ~loc
            [%expr [%e evar ~loc old_var], [%e evar ~loc new_var]]
            equal_cases
        in
        { e with
          pexp_attributes =
            (* Disable partial match (for polymorphic variants) + match nonexhaustive
               warning *)
            [ attribute
                ~loc
                ~name:{ txt = "ocaml.warning"; loc }
                ~payload:(PStr [ pstr_eval ~loc [%expr "-8-11"] [] ])
            ]
        }
      in
      let fn = [%expr fun [%p pvar ~loc old_var] [%p pvar ~loc new_var] -> [%e match_]] in
      bind_apply
        ~prevent_tail_call
        ~fn_label:"equal"
        ~op_name:"cutoff"
        ~loc
        ~modul
        ~with_location:
          (match location_behavior with
           | Location_behavior.Location_of_callsite -> No_location
           | Location_behavior.Location_in_scope -> Location_in_scope "here")
        ~arg:expr
        ~fn
        ()
      |> location_ghoster#expression
    ;;

    let expand_match
      ~extension_kind:_
      ~(match_kind : Match_kind.t)
      ~loc
      ~modul
      ~(locality : Locality.t)
      expr
      cases
      =
      (match locality with
       | { allocate_function_on_stack = false; return_value_in_exclave = false } -> ()
       | _ -> Location.raise_errorf ~loc "ppx_bonsai supports neither [bindl] nor [mapl]");
      (* Detect if this match is actually an if statement (desugared from if%arr). An if
         statement desugars to a match with true/false boolean patterns. *)
      let outer_context =
        match match_kind with
        | Match -> "match%arr"
        | If_ -> "if%arr"
      in
      List.iter cases ~f:(fun case ->
        assert_no_nested_let_arr_or_let_sub_extensions ~outer_context case.pc_rhs);
      let expr =
        join_tuples_into_single_t ~loc ~location_behavior ~modul ~locality expr
      in
      let cutoff_expr =
        match
          List.exists cases ~f:(fun case -> ignores_at_least_1_subpattern case.pc_lhs)
        with
        | true -> build_cutoff_expr ~loc ~modul expr cases
        | false -> expr
      in
      bind_apply
        ~prevent_tail_call
        ~loc
        ~modul
        ~with_location
        ~op_name:name
        ~arg:cutoff_expr
        ~fn:(pexp_function ~loc cases)
        ()
    ;;
  end
  in
  (module Arr : Ext)
;;

let arr = arr ~expand_with:"arr"

let sub (location_behavior : Location_behavior.t) : (module Ext) =
  let module Sub : Ext = struct
    let name = "sub"
    let with_location = Location_behavior.to_ppx_let_behavior location_behavior
    let wrap_expansion = wrap_expansion_identity
    let prevent_tail_call = true

    let disallow_expression ~loc _ pexp_desc =
      match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
      (* It is worse to use let%sub...and instead of multiple let%sub in a row, so
         disallow it. *)
      | Pexp_let (_, Nonrecursive, _ :: _ :: _, _) ->
        Error "let%sub should not be used with 'and'."
      | Pexp_let (Mutable, _, _, _) -> Error "let%sub should not be used with 'mutable'."
      | Pexp_while (_, _) -> Error "while%sub is not supported"
      | _ -> Ok ()
    ;;

    let already_has_nontail expr =
      List.exists expr.pexp_attributes ~f:(fun attribute ->
        String.equal attribute.attr_name.txt "nontail")
    ;;

    let sub_return ~loc ~modul ~lhs ~rhs ~body =
      let returned_rhs = qualified_return ~loc ~modul rhs in
      let body =
        match already_has_nontail body with
        | false -> nontail ~loc body
        | true -> body
      in
      bind_apply
        ~prevent_tail_call
        ~op_name:name
        ~loc
        ~modul
        ~with_location
        ~arg:returned_rhs
        ~fn:(pexp_fun Nolabel None ~loc lhs body)
        ()
    ;;

    let destruct ~assume_exhaustive ~loc ~modul ~lhs ~rhs ~body =
      match lhs.ppat_desc with
      | Ppat_var _ -> None
      | _ ->
        let bindings = [ value_binding ~loc ~pat:lhs ~expr:rhs ] in
        let pattern_projections =
          project_pattern_variables ~assume_exhaustive ~modul ~with_location bindings
        in
        Some
          (match pattern_projections with
           (* We handle the special case of having no pattern projections (which means
              there were no variables to be projected) by projecting the whole pattern
              once, just to ensure that the expression being projected matches the
              pattern. We only do this when the pattern is exhaustive, because otherwise
              the pattern matching is already happening inside the [switch] call. *)
           | [] when assume_exhaustive ->
             let projection_case = case ~lhs ~guard:None ~rhs:(eunit ~loc) in
             let fn = pexp_function ~loc [ projection_case ] in
             let rhs =
               bind_apply
                 ~op_name:Map.name
                 ~loc
                 ~modul
                 ~with_location
                 ~arg:rhs
                 ~fn
                 ()
                 ~prevent_tail_call
             in
             sub_return ~loc ~modul ~lhs:(ppat_any ~loc) ~rhs ~body
           | _ ->
             List.fold
               pattern_projections
               ~init:body
               ~f:(fun expr { txt = binding; loc = _ } ->
                 sub_return
                   ~loc:
                     { loc_start = lhs.ppat_loc.loc_start
                     ; loc_end = body.pexp_loc.loc_end
                     ; loc_ghost = true
                     }
                   ~modul
                   ~lhs:binding.pvb_pat
                   ~rhs:binding.pvb_expr
                   ~body:expr))
    ;;

    let focus_any =
      object
        inherit Ast_traverse.map as super

        method! pattern pattern =
          match pattern.ppat_desc with
          | Ppat_any -> Merlin_helpers.focus_pattern pattern
          | _ -> super#pattern pattern
      end
    ;;

    let switch ~loc ~switch_loc ~modul case_number case_number_cases =
      let case_number = focus_any#expression case_number in
      pexp_apply
        ~loc
        (eoperator ~loc:switch_loc ~modul "switch")
        [ ( Labelled "here"
          , match location_behavior with
            | Location_of_callsite -> Ppx_here_expander.lift_position ~loc:switch_loc
            | Location_in_scope -> evar ~loc "here" )
        ; Labelled "match_", { case_number with pexp_loc = switch_loc }
        ; Labelled "branches", eint ~loc:switch_loc (List.length case_number_cases - 1)
        ; Labelled "with_", pexp_function ~loc:switch_loc case_number_cases
        ]
    ;;

    let maybe_wrap_with_lazy ~loc ~modul ~graph_name_if_lazy case =
      let case_loc = { case.pc_rhs.pexp_loc with loc_ghost = true } in
      let pc_rhs =
        match graph_name_if_lazy with
        | Some { txt = graph; loc = graph_loc } ->
          pexp_apply
            ~loc:case_loc
            (eoperator ~loc ~modul "delay")
            [ Nolabel, pexp_ident ~loc:graph_loc { txt = Lident graph; loc = graph_loc }
            ; ( Labelled "f"
              , pexp_fun
                  ~loc:case_loc
                  Nolabel
                  None
                  (ppat_var ~loc:graph_loc { txt = graph; loc = graph_loc })
                  case.pc_rhs )
            ]
        | None -> case.pc_rhs
      in
      { case with pc_rhs }
    ;;

    let expand_match ~extension_kind:_ ~match_kind:_ ~loc ~modul ~locality expr =
      let expr, graph_name_if_lazy =
        match Extract_lazy_extension.extract expr with
        | Some { expr_without_lazy; graph_name } -> expr_without_lazy, Some graph_name
        | None -> expr, None
      in
      let expr =
        join_tuples_into_single_t ~loc ~location_behavior ~modul ~locality expr
      in
      function
      | [] -> assert false
      | [ (case : case) ] ->
        let case =
          case
          |> maybe_wrap_with_lazy ~loc ~modul ~graph_name_if_lazy
          |> add_usages_for_vars_definitely_used_in_when_clause
        in
        let returned_expr = qualified_return ~loc ~modul expr in
        let fn =
          maybe_destruct
            ~destruct
            ~loc
            ~modul
            ~return_value_in_exclave:locality.return_value_in_exclave
            ~zero_alloc:false
            ~lhs:case.pc_lhs
            ~body:case.pc_rhs
        in
        bind_apply
          ~op_name:name
          ~loc
          ~modul
          ~with_location
          ~arg:returned_expr
          ~fn
          ~prevent_tail_call
          ()
      | _ :: _ :: _ as cases ->
        let cases =
          List.map cases ~f:(fun case ->
            maybe_wrap_with_lazy ~loc ~modul ~graph_name_if_lazy case)
        in
        let var_name = gen_symbol ~prefix:"__pattern_syntax" () in
        let var_expression = evar ~loc var_name in
        let var_pattern = pvar ~loc var_name in
        let body = indexed_match ~loc ~modul ~destruct ~switch var_expression cases in
        sub_return ~loc ~modul ~lhs:var_pattern ~rhs:expr ~body
    ;;
  end
  in
  (module Sub : Ext)
;;

module For_testing = struct
  module Balance_list_tree = Balance_list_tree
end
