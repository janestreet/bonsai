open Base
open Ppxlib
open Ast_builder.Default
open Ppx_let_expander
open Ppx_pattern_bind

module Sub : Ext = struct
  let name = "sub"
  let with_location = true
  let wrap_expansion = wrap_expansion_identity

  let disallow_expression _ = function
    (* It is worse to use let%sub...and instead of multiple let%sub in a row,
       so disallow it. *)
    | Pexp_let (Nonrecursive, _ :: _ :: _, _) ->
      Error "let%sub should not be used with 'and'."
    | Pexp_while (_, _) -> Error "while%sub is not supported"
    | _ -> Ok ()
  ;;

  let sub_return ~loc ~modul ~lhs ~rhs ~body =
    let returned_rhs = qualified_return ~loc ~modul rhs in
    bind_apply
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
        project_pattern_variables ~assume_exhaustive ~modul ~with_location:true bindings
      in
      Some
        (match pattern_projections with
         (* We handle the special case of having no pattern projections (which
            means there were no variables to be projected) by projecting the
            whole pattern once, just to ensure that the expression being
            projected matches the pattern. We only do this when the pattern is
            exhaustive, because otherwise the pattern matching is already
            happening inside the [switch] call. *)
         | [] when assume_exhaustive ->
           let projection_case = case ~lhs ~guard:None ~rhs:(eunit ~loc) in
           let fn = pexp_function ~loc [ projection_case ] in
           let rhs =
             bind_apply ~op_name:Map.name ~loc ~modul ~with_location:true ~arg:rhs ~fn ()
           in
           sub_return ~loc ~modul ~lhs:(ppat_any ~loc) ~rhs ~body
         | _ ->
           List.fold pattern_projections ~init:body ~f:(fun expr { txt = binding; loc } ->
             sub_return ~loc ~modul ~lhs:binding.pvb_pat ~rhs:binding.pvb_expr ~body:expr))
  ;;

  let switch ~loc ~switch_loc ~modul case_number case_number_cases =
    pexp_apply
      ~loc
      (eoperator ~loc:switch_loc ~modul "switch")
      [ Labelled "here", Ppx_here_expander.lift_position ~loc:switch_loc
      ; Labelled "match_", { case_number with pexp_loc = switch_loc }
      ; Labelled "branches", eint ~loc:switch_loc (List.length case_number_cases - 1)
      ; Labelled "with_", pexp_function ~loc:switch_loc case_number_cases
      ]
  ;;

  let expand_match ~loc ~modul ~locality expr = function
    | [] -> assert false
    | [ (case : case) ] ->
      let returned_expr = qualified_return ~loc ~modul expr in
      let fn =
        maybe_destruct ~destruct ~loc ~modul ~locality ~lhs:case.pc_lhs ~body:case.pc_rhs
      in
      bind_apply ~op_name:name ~loc ~modul ~with_location ~arg:returned_expr ~fn ()
    | _ :: _ :: _ as cases ->
      let var_name = gen_symbol ~prefix:"__pattern_syntax" () in
      let var_expression = evar ~loc var_name in
      let var_pattern = pvar ~loc var_name in
      let body = indexed_match ~loc ~modul ~destruct ~switch var_expression cases in
      sub_return ~loc ~modul ~lhs:var_pattern ~rhs:expr ~body
  ;;
end

module Arr : Ext = struct
  let name = "arr"
  let with_location = true

  let location_ghoster =
    object
      inherit Ast_traverse.map as super
      method! location loc = super#location { loc with loc_ghost = true }
    end
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
            (match pattern.ppat_desc with
             (* let (_ as a) = x in ... *)
             | Ppat_alias (_, _) -> false
             | Ppat_any
             (* let { a ; b ; _ } = x in ... *)
             | Ppat_record (_, Open)
             (* let { a = (module _) ; b } = x in ... *)
             | Ppat_unpack { txt = None; _ } -> true
             | Ppat_record (_, Closed)
             | Ppat_unpack { txt = Some _; _ }
             | Ppat_constant _
             | Ppat_interval (_, _)
             | Ppat_var _ | Ppat_tuple _
             | Ppat_construct (_, _)
             | Ppat_array _
             | Ppat_or (_, _)
             | Ppat_constraint (_, _)
             | Ppat_type _ | Ppat_lazy _ | Ppat_extension _
             | Ppat_open (_, _)
             | Ppat_exception _
             | Ppat_variant (_, _) -> super#pattern pattern acc)
      end
    in
    ignore_finder#pattern pattern false
  ;;

  let add_cutoff_to_value_binding ~loc ~modul value_binding =
    let variables =
      let variables_of =
        object
          inherit [string loc list] Ast_traverse.fold as super

          method! pattern p acc =
            let acc = super#pattern p acc in
            match p.ppat_desc with
            | Ppat_var var -> var :: acc
            | Ppat_alias (_, var) -> var :: acc
            | _ -> acc
        end
      in
      variables_of#pattern value_binding.pvb_pat []
    in
    let ident_to_extracted_variable, variables =
      List.fold_map
        ~init:(Base.Map.empty (module String))
        variables
        ~f:(fun acc variable ->
          let extracted_variable = Extracted_variable.of_original_name variable in
          Base.Map.set acc ~key:variable.txt ~data:extracted_variable, extracted_variable)
    in
    let old_pattern =
      replace_variable
        ~f:(fun label ->
          match Base.Map.find ident_to_extracted_variable label.txt with
          | None -> `Remove
          | Some extracted_variable -> `Rename extracted_variable.old_label.txt)
        value_binding.pvb_pat
    in
    let new_pattern =
      replace_variable
        ~f:(fun label ->
          match Base.Map.find ident_to_extracted_variable label.txt with
          | None -> `Remove
          | Some extracted_variable -> `Rename extracted_variable.new_label.txt)
        value_binding.pvb_pat
    in
    let located_ident_to_longident (label : label loc) : longident_loc =
      let { txt; loc } = label in
      { txt = lident txt; loc }
    in
    let phys_equalities =
      List.map variables ~f:(fun { old_label; new_label; original_label = _ } ->
        let old_label = pexp_ident ~loc (located_ident_to_longident old_label) in
        let new_label = pexp_ident ~loc (located_ident_to_longident new_label) in
        [%expr phys_equal [%e old_label] [%e new_label]])
    in
    let check =
      List.reduce phys_equalities ~f:(fun prev next -> [%expr [%e next] && [%e prev]])
    in
    let fn =
      match check with
      | None -> [%expr fun _ _ -> true]
      | Some check ->
        location_ghoster#expression
          [%expr fun [%p old_pattern] [%p new_pattern] -> [%e check]]
    in
    let expr =
      bind_apply
        ~fn_label:"equal"
        ~op_name:"cutoff"
        ~loc
        ~modul
        ~with_location:false
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

  let wrap_expansion
    :  loc:location -> modul:longident loc option -> value_binding list -> expression
      -> expand:(loc:location -> value_binding list -> expression -> expression)
      -> expression
    =
    fun ~loc ~modul value_bindings expression ~expand ->
    let value_bindings =
      List.map value_bindings ~f:(maybe_add_cutoff_to_value_binding ~loc ~modul)
    in
    expand ~loc value_bindings expression
  ;;

  let disallow_expression _ = function
    | Pexp_while (_, _) -> Error "while%%arr is not supported."
    | _ -> Ok ()
  ;;

  let destruct ~assume_exhaustive:_ ~loc:_ ~modul:_ ~lhs:_ ~rhs:_ ~body:_ = None

  let expand_match ~loc ~modul ~locality expr cases =
    (match locality with
     | `global -> ()
     | `local ->
       Location.raise_errorf ~loc "ppx_bonsai supports neither [bindl] nor [mapl]");
    bind_apply
      ~loc
      ~modul
      ~with_location
      ~op_name:name
      ~arg:expr
      ~fn:(pexp_function ~loc cases)
      ()
  ;;
end

let sub = (module Sub : Ext)
let arr = (module Arr : Ext)
