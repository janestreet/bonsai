open Ppxlib

let expect_test ~loc structure_item = [%stri [%%expect_test [%%i structure_item]]]
let create_structure ~loc structure_item _binding = [ expect_test ~loc structure_item ]

let register () =
  let ppx_name = "expect_test.bonsai" in
  Driver.register_transformation
    ppx_name
    ~rules:
      [ Context_free.Rule.extension
          (Extension.declare_inline
             ppx_name
             Structure_item
             Ast_pattern.(pstr (as__ (pstr_value nonrecursive (__ ^:: nil)) ^:: nil))
             (fun ~loc ~path:_ structure_item binding ->
               create_structure ~loc structure_item binding))
      ]
;;

module For_testing = struct
  let create_structure = create_structure
end
