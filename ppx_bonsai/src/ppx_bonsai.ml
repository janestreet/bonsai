open Ppxlib
open Ast_builder.Default
open Ppx_let_expander
open Ppx_bonsai_expander
module Test_extension = Test_extension

let locality = Locality.global

let location_behavior : Location_behavior.t ref =
  ref Location_behavior.Location_of_callsite
;;

let ext t extension_kind =
  Extension.declare_with_path_arg
    (ext_full_name (t !location_behavior) ~locality extension_kind)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg expr ->
      expand (t !location_behavior) extension_kind ~locality ~modul:arg expr)
;;

(* If [%lazy] wasn't expanded, it means you're using it incorrectly. *)
let lazy_error =
  Extension.declare_with_path_arg
    "lazy"
    Extension.Context.expression
    Ast_pattern.(alt_option (single_expr_payload (pexp_ident (lident __))) (pstr nil))
    (fun ~loc ~path:_ ~arg:_ _ ->
      pexp_extension
        ~loc
        (Location.error_extensionf
           ~loc
           "Syntax error. The correct syntax is `match%%sub [%%lazy] ...` or `match%%sub \
            [%%lazy graph] ...`. Does your code compile if you delete this [%%lazy] \
            block?"))
;;

let () =
  Driver.add_arg
    "-for-bonsai-internals-use-location-in-scope"
    ~doc:
      "{unit}: Normally ppx_bonsai passes [%here] to locations. If this flag is \
       provided, it'll pass the identifier \"here\" and assume that it's in scope. This \
       is meant for bonsai internals that thread/report the location."
    (Arg.Unit
       (fun () ->
         location_behavior := Location_behavior.Location_in_scope;
         ()))
;;

let () =
  Driver.register_transformation
    "bonsai"
    ~extensions:
      [ ext sub Extension_kind.default
      ; ext sub Extension_kind.default_open
      ; ext arr Extension_kind.default
      ; ext arr Extension_kind.default_open
      ; lazy_error
      ]
;;

let () = Test_extension.register ()
