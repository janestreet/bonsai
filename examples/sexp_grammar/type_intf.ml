open! Core
module Auto_generated = Bonsai_web_ui_auto_generated

module T = struct
  type record =
    { name : string
    ; age : int
    ; market_on_gold : int * int
    ; likes_cats : bool [@default true]
    }
  [@@deriving sexp, sexp_grammar, quickcheck]

  type binary_tree =
    | Leaf of { value : int }
    | Node of
        { value : int
        ; left : binary_tree option
        ; right : binary_tree option
        }
  [@@deriving sexp, sexp_grammar, quickcheck]

  type t =
    | Int of int
    | Bool of bool
    | Float of float
    | Char of char
    | String of string
    | Option of string option
    | Record of record
    | Inline_record of
        { username : string
        ; is_bonsai_dev : bool
        ; favourite_point_in_z3 : int * int * int
        }
    | Simple_list of int list
    | Complex_list of record list
    | Recursive_type of binary_tree
  [@@deriving sexp, sexp_grammar, quickcheck]
end

include T

let view t = Auto_generated.view (module T) t `This_view_may_change_without_notice
let form = Auto_generated.form (module T) ()
