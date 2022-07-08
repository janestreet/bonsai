open! Core

val expect_output_and_identity_roundtrip
  :  ?expect_diff:(unit -> unit)
  -> path:string list
  -> query:string list Core.String.Map.t
  -> sexp_of_t:('a -> Sexp.t)
  -> expect:(unit -> unit)
  -> (Uri_parsing.Components.t, 'a Uri_parsing.Parse_result.t) Uri_parsing.Projection.t
  -> unit
