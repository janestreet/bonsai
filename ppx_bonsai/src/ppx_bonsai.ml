open Ppxlib
open Ppx_let_expander
open Ppx_bonsai_expander
module Test_extension = Test_extension

let locality = `global

let ext t extension_kind =
  Extension.declare_with_path_arg
    (ext_full_name t ~locality extension_kind)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg expr -> expand t extension_kind ~locality ~modul:arg expr)
;;

let () =
  Driver.register_transformation
    "bonsai"
    ~extensions:
      [ ext sub Extension_kind.default
      ; ext sub Extension_kind.default_open
      ; ext arr Extension_kind.default
      ; ext arr Extension_kind.default_open
      ]
;;

let () = Test_extension.register ()
