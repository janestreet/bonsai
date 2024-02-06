open! Base

(* Bonsai_web_test_async is a wrapper library over [bonsai_web_test] that lets you write
   bonsai tests that use async. *)

include Bonsai.For_open
include Bonsai_web_test
include Async_js_test
