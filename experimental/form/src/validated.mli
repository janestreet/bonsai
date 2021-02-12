open! Core_kernel
open! Import
open Bonsai_web

(** https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate *)

val make
  :  ('input, 'unparsed Product.Same.t) Bonsai.Arrow_deprecated.t
  -> parse:('unparsed -> 'parsed Or_error.t)
  -> unparse:('parsed -> 'unparsed)
  -> ('input, 'parsed Product.Errorable.Same.t) Bonsai.Arrow_deprecated.t

val make_via_string
  :  (module Stringable with type t = 'parsed)
  -> ('input, string Product.Same.t) Bonsai.Arrow_deprecated.t
  -> ('input, 'parsed Product.Errorable.Same.t) Bonsai.Arrow_deprecated.t
