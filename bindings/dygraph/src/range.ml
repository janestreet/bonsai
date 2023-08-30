open Core
open! Import
open Gen_js_api

type t =
  { low : float
  ; high : float
  }
[@@deriving sexp, equal]

(* ranges in dygraphs are represented as [| low; high |]. *)
let t_to_js { low; high } = Ojs.array_to_js Ojs.float_to_js [| low; high |]

let t_of_js ojs =
  let data = Ojs.array_of_js Ojs.float_of_js ojs in
  { low = data.(0); high = data.(1) }
;;

module Spec = struct
  type nonrec t =
    | Infer
    | Specified of t
  [@@deriving sexp, equal]

  let t_to_js = function
    | Infer -> Ojs.null
    | Specified t -> t_to_js t
  ;;
end
